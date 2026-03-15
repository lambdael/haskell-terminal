{-# LANGUAGE ScopedTypeVariables, TypeFamilies, FlexibleContexts #-}
-- | GPipe ベースのターミナルエミュレータ メインモジュール。
--
-- GPipe + GPipe-GLFW + gpipe-freetype を使用して
-- ターミナル画面をレンダリングする。
module Hsterm.GPipe.Main (runGPipeTerminal) where

import Prelude hiding ((<*))

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException(..), try, catch)
import Control.Monad (unless, when, void)
import Control.Monad.IO.Class (liftIO)
import Data.Array ((!))
import Data.Char (chr, ord)
import Data.IORef
import Data.Maybe (fromJust, fromMaybe, isJust)
import qualified Data.Map.Strict as Map
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import Data.Word (Word8)
import System.IO
import qualified System.Process (readProcess)

import Data.Colour.SRGB (RGB(..), toSRGB, sRGB24read)
import Data.Colour (Colour)

import Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import Graphics.GPipe.Context.GLFW (Key(..), KeyState(..), ModifierKeys(..),
                                    MouseButton(..), MouseButtonState(..))

import Graphics.GPipe.Font
import Graphics.GPipe.Font.Types (GlyphMetrics(..), GlyphRegion(..))
import qualified Data.Sequence as Seq

import System.Posix.IO (fdToHandle)
import System.Posix.IOCtl (ioctl)
import System.Posix.Signals (signalProcess, sigINT, sigTERM, installHandler, Handler(..))
import System.Posix.Types (CPid, Fd)
import Foreign.C.Types (CUShort)

import qualified System.Console.Terminfo as TI
import qualified System.Console.Terminfo.Keys as TI

import Terminal.Terminal (newTerminal, applyAction, setSize)
import Terminal.Types
import Terminal.Posix.TIOCSWINSZ (TIOCSWINSZ(..))
import Terminal.Posix.Winsize (Winsize(..))

import Hsterm.GPipe.Config
import Hsterm.GPipe.Dyre (restoreResumeState, ResumeState(..), requestReload)
import Hsterm.GPipe.Monad
import Hsterm.GPipe.Terminal
import Hsterm.GPipe.Renderer

-- ── エントリーポイント ────────────────────────────────────

-- | fontconfig でモノスペースフォントを探す。
findMonospaceFont :: String -> IO FilePath
findMonospaceFont pattern_ = do
  let query = if null pattern_ then "monospace:style=Regular" else pattern_
  result <- try $ System.Process.readProcess "fc-match" ["-f", "%{file}", query] ""
  case (result :: Either SomeException String) of
    Right path -> return $ head $ lines path
    Left _     -> error $ "fc-match failed: no font found for pattern: " ++ query

-- | GPipe ターミナルを起動する。
--
-- デフォルト設定で起動する場合は @runGPipeTerminal defaultConfig@ を呼ぶ。
-- Dyre リロードからの復帰時はターミナル状態と PTY を復元する。
runGPipeTerminal :: TerminalConfig -> IO ()
runGPipeTerminal cfg = do
  -- Dyre コンパイルエラーの表示
  case tcErrorMsg cfg of
    Just err -> hPutStrLn stderr $ "Configuration error:\n" ++ err
    Nothing  -> return ()

  fontPath <- findMonospaceFont (tcFontFamily cfg)
  putStrLn $ "Using font: " ++ fontPath

  let pixelSize = tcFontSize cfg

  -- フォントアトラスを構築
  -- ターミナルで使用される文字セット: ASCII + ボックス描画 + ブロック要素 等
  let termCharSet = [' '..'~']          -- ASCII printable (32-126)
                 ++ ['\x2500'..'\x257F'] -- Box Drawing
                 ++ ['\x2580'..'\x259F'] -- Block Elements
                 ++ ['\x2190'..'\x21FF'] -- Arrows
                 ++ ['\x25A0'..'\x25FF'] -- Geometric Shapes
                 ++ ['\x2600'..'\x26FF'] -- Miscellaneous Symbols
  atlas <- buildAtlas (defaultTextConfig fontPath) { tcPixelSize = pixelSize, tcCharSet = termCharSet }
  putStrLn $ "Atlas: " ++ show (faWidth atlas) ++ "x" ++ show (faHeight atlas)
    ++ ", " ++ show (Map.size (faGlyphs atlas)) ++ " glyphs"

  -- セルサイズを計算
  let cellW = cellWidth atlas
      cellH = fromIntegral (faLineH atlas) :: Float

  -- Dyre リロードからの復帰チェック
  mResume <- restoreResumeState

  -- PTY ハンドルとターミナル状態の取得（復帰 or 新規起動）
  (pty, termRef, initialScrollOffset) <- case mResume of
    Just rs -> do
      -- 復帰: 保存された fd から PTY ハンドルを再構築
      putStrLn "Resuming from saved state..."
      let fd  = fromIntegral (rsPtyFdNum rs)  :: Fd
          pid = fromIntegral (rsPtyPidNum rs) :: CPid
      master <- fdToHandle fd
      hSetBuffering master NoBuffering
      let pty = PtyHandle { ptyMaster = master, ptyPid = pid, ptyFd = fd }
      -- ターミナル状態を復元（terminfo ハンドルは再構築）
      termInfo <- TI.setupTermFromEnv
      let term = (rsTerm rs) { terminfo = Just termInfo }
      termRef <- newIORef term
      return (pty, termRef, rsScrollOffset rs)

    Nothing -> do
      -- 新規起動
      pty <- spawnShell (tcShell cfg)
      putStrLn $ "Shell started, PID: " ++ show (ptyPid pty)
      termInfo <- TI.setupTermFromEnv
      termRef <- newIORef $ (newTerminal (tcInitialRows cfg, tcInitialCols cfg) (Just termInfo))
        { scrollbackMax = tcScrollback cfg }
      return (pty, termRef, 0)

  -- 復帰時はターミナルの寸法を使い、新規時は設定値を使う
  let (effRows, effCols) = case mResume of
        Just rs -> (rows (rsTerm rs), cols (rsTerm rs))
        Nothing -> (tcInitialRows cfg, tcInitialCols cfg)
      winW = ceiling (cellW * fromIntegral effCols) :: Int
      winH = ceiling (cellH * fromIntegral effRows) :: Int

  dirty   <- newIORef True
  scrollOffsetRef <- newIORef initialScrollOffset

  -- 選択状態
  selectionRef <- newIORef (Nothing :: Maybe Selection)
  -- クリップボード操作要求（IOコールバック → ContextT ループ間通信）
  clipboardWriteRef <- newIORef (Nothing :: Maybe String)
  pasteRequestRef   <- newIORef False

  -- PTY 読み取りスレッド
  _ <- forkIO $ runTerminalReader termRef dirty (ptyMaster pty)

  -- GPipe メインループ
  runContextT GLFW.defaultHandleConfig $ do
    let winConf = (GLFW.defaultWindowConfig "hsterm")
          { GLFW.configWidth = winW, GLFW.configHeight = winH }
    win <- newWindow (WindowFormatColor RGBA8) winConf

    -- アトラスをテクスチャにアップロード
    fontTex <- uploadAtlasTexture atlas

    -- PTY にバイト列を送る共通ヘルパー
    let sendPtyIO :: String -> IO ()
        sendPtyIO s = do
          hPutStr stderr $ "sendPty: " ++ show (map ord s) ++ "\n"
          hPutStr (ptyMaster pty) s
          hFlush (ptyMaster pty)

    -- HstermM アクション実行用の環境
    let hstermEnv = HstermEnv
          { heTermRef         = termRef
          , hePtyMaster       = ptyMaster pty
          , hePtyFd           = ptyFd pty
          , hePtyPid          = ptyPid pty
          , heDirty           = dirty
          , heScrollOffsetRef = scrollOffsetRef
          , heSelectionRef    = selectionRef
          , heClipboardWrite  = clipboardWriteRef
          , hePasteRequest    = pasteRequestRef
          }
        runAction = runHstermM hstermEnv
        -- キーバインドにリロードアクションを追加
        bindings = Map.insert (KeyCombo True True False Key'R) requestReload
                 $ tcKeyBindings cfg

    -- Ctrl が押されているかを追跡する（charCallback で制御文字を二重送信しないため）
    ctrlHeld <- liftIO $ newIORef False

    -- キー入力コールバック（文字入力用）
    -- GLFW の charCallback は印字可能な Unicode 文字だけ発火する。
    -- ただし Ctrl+キーの場合も制御文字として発火することがあるため、
    -- Ctrl が押されている間は charCallback を無視する。
    _ <- GLFW.setCharCallback win $ Just $ \ch -> do
      isCtrl <- readIORef ctrlHeld
      unless isCtrl $ do
        writeIORef scrollOffsetRef 0
        sendPtyIO [ch]

    -- キー入力コールバック（制御キー用）
    _ <- GLFW.setKeyCallback win $ Just $ \key _scancode keyState mods -> do
      let ctrl = modifierKeysControl mods
      -- Ctrl の状態を記録
      liftIO $ writeIORef ctrlHeld ctrl
      when (keyState == KeyState'Pressed || keyState == KeyState'Repeating) $ do
        let shift = modifierKeysShift mods
            alt   = modifierKeysAlt mods
            combo = KeyCombo ctrl shift alt key

        -- カスタムキーバインドを検索
        case Map.lookup combo bindings of
          Just action -> runAction action
          Nothing -> do
            -- カスタムバインドがない場合のデフォルトターミナル入力処理
            -- 通常キー入力時はスクロール位置をリセット
            writeIORef scrollOffsetRef 0
            case key of
              -- Ctrl+文字 → 制御コード (Ctrl-A=0x01 ... Ctrl-Z=0x1A)
              _ | ctrl, Just c <- keyToAlpha key ->
                sendPtyIO [chr (ord c - ord 'a' + 1)]

              -- Enter → CR
              Key'Enter     -> sendPtyIO "\r"
              -- Backspace → DEL (0x7F)
              Key'Backspace -> sendPtyIO "\x7f"
              -- Tab → HT
              Key'Tab       -> sendPtyIO "\t"
              -- Escape → ESC
              Key'Escape    -> sendPtyIO "\x1b"

              -- 特殊キー → terminfo エスケープシーケンス
              _ -> do
                term <- readIORef termRef
                case terminfo term of
                  Just tiHandle ->
                    case lookup key terminalKeyMap of
                      Just cap ->
                        case TI.getCapability tiHandle cap of
                          Just str -> sendPtyIO str
                          Nothing  -> return ()
                      Nothing -> return ()
                  Nothing -> return ()

    -- ウィンドウサイズ変更コールバック
    winSizeRef <- liftIO $ newIORef (V2 winW winH)

    -- マウスカーソル位置を追跡する（ボタンイベント時にセル座標を計算するため）
    mousePosRef <- liftIO $ newIORef (0.0 :: Double, 0.0 :: Double)
    -- マウスボタン押下中フラグ（ドラッグ検出用）
    mouseBtnRef <- liftIO $ newIORef False

    -- マウスカーソル位置コールバック
    _ <- GLFW.setCursorPosCallback win $ Just $ \px py -> do
      writeIORef mousePosRef (px, py)
      term <- readIORef termRef
      let mode = mouseMode term
      isDragging <- readIORef mouseBtnRef
      if mode /= MouseNone
        then
          -- MouseAll / MouseButton モード時、移動中もイベント送信
          when (mode == MouseAll || (mode == MouseButton && isDragging)) $ do
            let col = floor (px / realToFrac cellW) + 1 :: Int
                row = floor (py / realToFrac cellH) + 1 :: Int
            when (col >= 1 && row >= 1 && col <= cols term && row <= rows term) $ do
              let enc = mouseEncoding term
                  cb = if isDragging then 32 else 35 + 32
              sendPtyIO $ encodeMouseEvent enc cb col row True
        else
          -- マウスモード無効時: ドラッグ中なら選択範囲を更新
          when isDragging $ do
            let col = max 1 (min (cols term) (floor (px / realToFrac cellW) + 1 :: Int))
                row = max 1 (min (rows term) (floor (py / realToFrac cellH) + 1 :: Int))
            modifyIORef' selectionRef $ fmap $ \s -> s { selCurrent = (row, col) }
            writeIORef dirty True

    -- マウスボタンコールバック
    _ <- GLFW.setMouseButtonCallback win $ Just $ \button state _mods -> do
      term <- readIORef termRef
      let mode = mouseMode term
          isPress = state == MouseButtonState'Pressed
      if mode /= MouseNone
        then do
          (px, py) <- readIORef mousePosRef
          let col = floor (px / realToFrac cellW) + 1 :: Int
              row = floor (py / realToFrac cellH) + 1 :: Int
          when (col >= 1 && row >= 1 && col <= cols term && row <= rows term) $ do
            let enc = mouseEncoding term
                btn = case button of
                        MouseButton'1 -> 0  -- left
                        MouseButton'2 -> 1  -- middle
                        MouseButton'3 -> 2  -- right
                        _             -> 0
            writeIORef mouseBtnRef isPress
            sendPtyIO $ encodeMouseEvent enc btn col row isPress
        else
          -- マウスモード無効時: 左ボタンでテキスト選択
          when (button == MouseButton'1) $ do
            (px, py) <- readIORef mousePosRef
            let col = max 1 (min (cols term) (floor (px / realToFrac cellW) + 1 :: Int))
                row = max 1 (min (rows term) (floor (py / realToFrac cellH) + 1 :: Int))
            writeIORef mouseBtnRef isPress
            if isPress
              then do
                writeIORef selectionRef (Just (Selection (row, col) (row, col)))
                writeIORef dirty True
              else return ()

    -- スクロールコールバック
    _ <- GLFW.setScrollCallback win $ Just $ \_ dy -> do
      term <- readIORef termRef
      let mode = mouseMode term
      if mode /= MouseNone
        then do
          (px, py) <- readIORef mousePosRef
          let col = floor (px / realToFrac cellW) + 1 :: Int
              row = floor (py / realToFrac cellH) + 1 :: Int
          when (col >= 1 && row >= 1 && col <= cols term && row <= rows term) $ do
            let enc = mouseEncoding term
                btn = if dy > 0 then 64 else 65  -- scroll up / down
            sendPtyIO $ encodeMouseEvent enc btn col row True
        else do
          -- マウスモード無効時: スクロールバック操作
          let maxOff = Seq.length (scrollbackBuffer term)
              step = if dy > 0 then 3 else -3
          modifyIORef' scrollOffsetRef (\off -> max 0 (min maxOff (off + step)))
          writeIORef dirty True

    _ <- GLFW.setWindowSizeCallback win $ Just $ \w h -> do
      let newCols = floor (fromIntegral w / cellW) :: Int
          newRows = floor (fromIntegral h / cellH) :: Int
      when (newCols > 0 && newRows > 0) $ do
        modifyIORef' termRef $ setSize (newRows, newCols)
        writeIORef winSizeRef (V2 w h)
        writeIORef dirty True
        -- PTY にウィンドウサイズを通知
        let wsize = Winsize
              { ws_col    = fromIntegral newCols
              , ws_row    = fromIntegral newRows
              , ws_xpixel = fromIntegral w
              , ws_ypixel = fromIntegral h
              }
        ioctl (ptyFd pty) TIOCSWINSZ wsize
        signalProcess 28 (ptyPid pty)  -- SIGWINCH

    -- メインループ
    startTime <- liftIO getCurrentTime
    mainLoop win fontTex atlas cfg termRef dirty winSizeRef scrollOffsetRef selectionRef clipboardWriteRef pasteRequestRef sendPtyIO (ptyPid pty) mousePosRef startTime

-- | メインループ。
--
-- GPipe-GLFW は swapWindowBuffers の中で GLFW イベントをポーリングする。
-- render を呼ばずに swapWindowBuffers だけ呼ぶと GL コンテキストが
-- カレントでないため GLX BadAccess が発生する。
-- そのため、毎フレーム必ず render + swap を行う。
-- ダブルバッファリングでは swap 後のバックバッファ内容は未定義なので
-- idle フレームでも全画面を再描画する必要がある。
--
-- ★ セルデータテクスチャ方式:
-- 固定グリッド頂点バッファはリサイズ時のみ再生成する。
-- 毎フレームは 4 枚のセルデータテクスチャ（bgColor, fgColor, glyphUV,
-- glyphPos）のみ更新する。シェーダーがテクスチャルックアップで
-- 各セルの情報を取得するため、頂点データは画面内容に依存しない。
mainLoop
  :: Window os RGBAFloat ()
  -> Texture2D os (Format RFloat)
  -> FontAtlas
  -> TerminalConfig
  -> IORef Terminal
  -> IORef Bool
  -> IORef (V2 Int)
  -> IORef Int             -- ^ scrollOffsetRef
  -> IORef (Maybe Selection) -- ^ selectionRef
  -> IORef (Maybe String)  -- ^ clipboardWriteRef
  -> IORef Bool            -- ^ pasteRequestRef
  -> (String -> IO ())     -- ^ sendPty
  -> CPid
  -> IORef (Double, Double) -- ^ mousePosRef
  -> UTCTime -- ^ startTime
  -> ContextT GLFW.Handle os IO ()
mainLoop win fontTex atlas cfg termRef dirty winSizeRef scrollOffsetRef selectionRef clipboardWriteRef pasteRequestRef sendPty pid mousePosRef startTime = do
  -- ウィンドウサイズを取得
  winSize <- liftIO $ readIORef winSizeRef

  let scheme    = tcColorScheme cfg
      shaderCfg = tcShaderConfig cfg
      cellW     = cellWidth atlas
      cellH     = fromIntegral (faLineH atlas) :: Float
      animated  = hasAnimatedSlots scheme

  -- 初期ターミナルサイズから固定グリッドバッファ確保
  term0 <- liftIO $ readIORef termRef
  let r0 = rows term0
      c0 = cols term0
      gridCap = r0 * c0 * 6

  -- シェーダーをコンパイル（3種: 背景・テキスト・カーソル）
  textShader <- compileTextShader win winSize shaderCfg scheme cellW cellH c0 r0
  bgShader   <- compileBgShader win winSize shaderCfg scheme cellW cellH c0 r0
  curShader  <- compileCursorShader win winSize shaderCfg cellW cellH c0 r0

  -- 背景クリア色（デフォルト背景スロットの静的色部分）
  let clearCol = case csNormal scheme (tcDefaultBg cfg) of
        SolidColor c -> c
        _            -> V4 0 0 0 1

  bgGridBuf  <- newBuffer gridCap
  txtGridBuf <- newBuffer gridCap
  curBuf     <- newBuffer (6 :: Int)

  writeBuffer bgGridBuf 0 (buildBgGridVertices atlas r0 c0)
  writeBuffer txtGridBuf 0 (buildTextGridVertices atlas r0 c0)

  -- セルデータテクスチャ（cols×rows, RGBA32F）
  bgColorTex'  <- newTexture2D RGBA32F (V2 c0 r0) 1
  fgColorTex'  <- newTexture2D RGBA32F (V2 c0 r0) 1
  glyphUVTex'  <- newTexture2D RGBA32F (V2 c0 r0) 1
  glyphPosTex' <- newTexture2D RGBA32F (V2 c0 r0) 1

  -- time / mouse uniform バッファ
  timeBuf  <- newBuffer 1 :: ContextT GLFW.Handle os IO (Buffer os (Uniform (B Float)))
  mouseBuf <- newBuffer 1 :: ContextT GLFW.Handle os IO (Buffer os (Uniform (B2 Float)))

  bgGridRef  <- liftIO $ newIORef bgGridBuf
  txtGridRef <- liftIO $ newIORef txtGridBuf
  curBufRef  <- liftIO $ newIORef curBuf
  curLenRef  <- liftIO $ newIORef (0 :: Int)
  prevDimRef <- liftIO $ newIORef (r0, c0)

  bgTexRef  <- liftIO $ newIORef bgColorTex'
  fgTexRef  <- liftIO $ newIORef fgColorTex'
  uvTexRef  <- liftIO $ newIORef glyphUVTex'
  posTexRef <- liftIO $ newIORef glyphPosTex'

  go textShader bgShader curShader winSize clearCol
     bgGridRef txtGridRef curBufRef curLenRef prevDimRef
     bgTexRef fgTexRef uvTexRef posTexRef
     timeBuf mouseBuf
  where
    scheme    = tcColorScheme cfg
    shaderCfg = tcShaderConfig cfg
    cellW     = cellWidth atlas
    cellH     = fromIntegral (faLineH atlas) :: Float
    animated  = hasAnimatedSlots scheme
    nearestFilter = SamplerFilter Nearest Nearest Nearest Nothing

    go textShader bgShader curShader prevSize clearCol
       bgGridRef txtGridRef curBufRef curLenRef prevDimRef
       bgTexRef fgTexRef uvTexRef posTexRef
       timeBuf mouseBuf = do
      -- フレームレート制限
      liftIO $ threadDelay (tcFrameDelay cfg)

      -- 経過時間を計算
      now <- liftIO getCurrentTime
      let elapsed = realToFrac (diffUTCTime now startTime) :: Float

      -- time / mouse uniform を更新
      writeBuffer timeBuf 0 [elapsed]
      (mx, my) <- liftIO $ readIORef mousePosRef
      writeBuffer mouseBuf 0 [V2 (realToFrac mx) (realToFrac my :: Float)]

      -- アニメーション付きスロットがあれば毎フレーム dirty
      when animated $ liftIO $ writeIORef dirty True

      -- クリップボード操作（ContextT 内で実行する必要があるため、ここで処理）
      mCopyText <- liftIO $ readIORef clipboardWriteRef
      case mCopyText of
        Just txt -> do
          _ <- GLFW.setClipboardString win txt
          liftIO $ writeIORef clipboardWriteRef Nothing
        Nothing -> return ()
      wantPaste <- liftIO $ readIORef pasteRequestRef
      when wantPaste $ do
        liftIO $ writeIORef pasteRequestRef False
        mClip <- GLFW.getClipboardString win
        case mClip of
          Just (Just str) -> liftIO $ sendPty str
          _ -> return ()

      curSize <- liftIO $ readIORef winSizeRef

      -- ウィンドウサイズが変わったらシェーダーを再コンパイル
      (textShader', bgShader', curShader') <-
        if curSize /= prevSize
          then do
            term <- liftIO $ readIORef termRef
            let r = rows term
                c = cols term
            ts <- compileTextShader win curSize shaderCfg scheme cellW cellH c r
            bs <- compileBgShader win curSize shaderCfg scheme cellW cellH c r
            cs <- compileCursorShader win curSize shaderCfg cellW cellH c r
            return (ts, bs, cs)
          else return (textShader, bgShader, curShader)

      isDirty <- liftIO $ readIORef dirty
      liftIO $ writeIORef dirty False

      term <- liftIO $ readIORef termRef
      let r = rows term
          c = cols term
          gridLen = r * c * 6

      prevDim <- liftIO $ readIORef prevDimRef
      let resized = (r, c) /= prevDim
      when resized $ do
        liftIO $ writeIORef prevDimRef (r, c)
        bg'  <- newBuffer gridLen
        txt' <- newBuffer gridLen
        writeBuffer bg' 0 (buildBgGridVertices atlas r c)
        writeBuffer txt' 0 (buildTextGridVertices atlas r c)
        liftIO $ writeIORef bgGridRef bg'
        liftIO $ writeIORef txtGridRef txt'
        bgT  <- newTexture2D RGBA32F (V2 c r) 1
        fgT  <- newTexture2D RGBA32F (V2 c r) 1
        uvT  <- newTexture2D RGBA32F (V2 c r) 1
        posT <- newTexture2D RGBA32F (V2 c r) 1
        liftIO $ writeIORef bgTexRef bgT
        liftIO $ writeIORef fgTexRef fgT
        liftIO $ writeIORef uvTexRef uvT
        liftIO $ writeIORef posTexRef posT

      bgTex  <- liftIO $ readIORef bgTexRef
      fgTex  <- liftIO $ readIORef fgTexRef
      uvTex  <- liftIO $ readIORef uvTexRef
      posTex <- liftIO $ readIORef posTexRef

      when (isDirty || resized) $ do
        scrollOff <- liftIO $ readIORef scrollOffsetRef
        sel <- liftIO $ readIORef selectionRef
        let selRange = fmap selectionRange sel
            colorFn = mkColorResolver scheme elapsed
            (bgColors, fgColors, glyphUVs, glyphPositions) =
              buildCellData atlas term colorFn scrollOff selRange
            texSize = V2 c r

        writeTexture2D bgTex  0 0 texSize bgColors
        writeTexture2D fgTex  0 0 texSize fgColors
        writeTexture2D uvTex  0 0 texSize glyphUVs
        writeTexture2D posTex 0 0 texSize glyphPositions

        let cursorVerts = if scrollOff > 0
                          then []
                          else buildCursorVertices atlas term (tcCursorColor cfg)
            curLen = length cursorVerts
        liftIO $ writeIORef curLenRef curLen
        when (curLen > 0) $ do
          curBuf <- liftIO $ readIORef curBufRef
          writeBuffer curBuf 0 cursorVerts

      bgGridBuf  <- liftIO $ readIORef bgGridRef
      txtGridBuf <- liftIO $ readIORef txtGridRef
      curBuf     <- liftIO $ readIORef curBufRef
      curLen     <- liftIO $ readIORef curLenRef

      render $ do
        clearWindowColor win clearCol

        do arr <- newVertexArray bgGridBuf
           bgShader' $ BgShaderEnv
             { bgPrimitives = toPrimitiveArray TriangleList (takeVertices gridLen arr)
             , bgColorTex = (bgTex, nearestFilter, (pure ClampToEdge, V4 0 0 0 0))
             , bgTimeBuf = (timeBuf, 0)
             , bgMouseBuf = (mouseBuf, 0)
             }

        when (curLen > 0) $ do
          arr <- newVertexArray curBuf
          curShader' $ CursorShaderEnv
            { curPrimitives = toPrimitiveArray TriangleList (takeVertices curLen arr)
            , curTimeBuf = (timeBuf, 0)
            , curMouseBuf = (mouseBuf, 0)
            }

        do arr <- newVertexArray txtGridBuf
           textShader' $ TextShaderEnv
             { tePrimitives  = toPrimitiveArray TriangleList (takeVertices gridLen arr)
             , teFontAtlas   = (fontTex, SamplerFilter Linear Linear Linear Nothing, (pure ClampToEdge, 0))
             , teFgColorTex  = (fgTex, nearestFilter, (pure ClampToEdge, V4 0 0 0 0))
             , teGlyphUVTex  = (uvTex, nearestFilter, (pure ClampToEdge, V4 0 0 0 0))
             , teGlyphPosTex = (posTex, nearestFilter, (pure ClampToEdge, V4 0 0 0 0))
             , teTimeBuf = (timeBuf, 0)
             , teMouseBuf = (mouseBuf, 0)
             }

      swapWindowBuffers win

      closeReq <- GLFW.windowShouldClose win
      case closeReq of
        Just True ->
          liftIO $ signalProcess sigINT pid
        _ ->
          go textShader' bgShader' curShader' curSize clearCol
             bgGridRef txtGridRef curBufRef curLenRef prevDimRef
             bgTexRef fgTexRef uvTexRef posTexRef
             timeBuf mouseBuf

-- ── キーマッピング ────────────────────────────────────────

-- | GLFW Key から英小文字を返す（Ctrl+キー変換用）。
keyToAlpha :: Key -> Maybe Char
keyToAlpha Key'A = Just 'a'
keyToAlpha Key'B = Just 'b'
keyToAlpha Key'C = Just 'c'
keyToAlpha Key'D = Just 'd'
keyToAlpha Key'E = Just 'e'
keyToAlpha Key'F = Just 'f'
keyToAlpha Key'G = Just 'g'
keyToAlpha Key'H = Just 'h'
keyToAlpha Key'I = Just 'i'
keyToAlpha Key'J = Just 'j'
keyToAlpha Key'K = Just 'k'
keyToAlpha Key'L = Just 'l'
keyToAlpha Key'M = Just 'm'
keyToAlpha Key'N = Just 'n'
keyToAlpha Key'O = Just 'o'
keyToAlpha Key'P = Just 'p'
keyToAlpha Key'Q = Just 'q'
keyToAlpha Key'R = Just 'r'
keyToAlpha Key'S = Just 's'
keyToAlpha Key'T = Just 't'
keyToAlpha Key'U = Just 'u'
keyToAlpha Key'V = Just 'v'
keyToAlpha Key'W = Just 'w'
keyToAlpha Key'X = Just 'x'
keyToAlpha Key'Y = Just 'y'
keyToAlpha Key'Z = Just 'z'
keyToAlpha _     = Nothing

-- | 特殊キーと terminfo ケーパビリティの対応表。
terminalKeyMap :: [(Key, TI.Capability String)]
terminalKeyMap =
  [ (Key'Up,       TI.keyUp)
  , (Key'Down,     TI.keyDown)
  , (Key'Left,     TI.keyLeft)
  , (Key'Right,    TI.keyRight)
  , (Key'Home,     TI.keyHome)
  , (Key'End,      TI.keyEnd)
  , (Key'PageUp,   TI.keyPageUp)
  , (Key'PageDown, TI.keyPageDown)
  , (Key'Delete,   TI.keyDeleteChar)
  , (Key'F1,       TI.functionKey 1)
  , (Key'F2,       TI.functionKey 2)
  , (Key'F3,       TI.functionKey 3)
  , (Key'F4,       TI.functionKey 4)
  , (Key'F5,       TI.functionKey 5)
  , (Key'F6,       TI.functionKey 6)
  , (Key'F7,       TI.functionKey 7)
  , (Key'F8,       TI.functionKey 8)
  , (Key'F9,       TI.functionKey 9)
  , (Key'F10,      TI.functionKey 10)
  , (Key'F11,      TI.functionKey 11)
  , (Key'F12,      TI.functionKey 12)
  ]

-- ── マウスイベントエンコーディング ─────────────────────────

-- | マウスイベントを PTY に送信するエスケープシーケンスにエンコードする。
--
-- @encodeMouseEvent encoding buttonCode col row isPress@
--
-- * X10 互換: @ESC [ M Cb Cx Cy@ (Cb = button+32, Cx = col+32, Cy = row+32)
--   リリースは button=3。座標は 223 まで。
-- * SGR 拡張: @ESC [ < Cb ; Cx ; Cy M@ (press) / @ESC [ < Cb ; Cx ; Cy m@ (release)
--   座標制限なし。
encodeMouseEvent :: MouseEncoding -> Int -> Int -> Int -> Bool -> String
encodeMouseEvent MouseEncodingX10 btn col row isPress =
  let cb = if isPress then btn + 32 else 3 + 32  -- release = button 3
      cx = min 255 (col + 32)
      cy = min 255 (row + 32)
  in  "\ESC[M" ++ [chr cb, chr cx, chr cy]
encodeMouseEvent MouseEncodingSGR btn col row isPress =
  let suffix = if isPress then 'M' else 'm'
  in  "\ESC[<" ++ show btn ++ ";" ++ show col ++ ";" ++ show row ++ [suffix]
