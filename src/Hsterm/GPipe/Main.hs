{-# LANGUAGE ScopedTypeVariables, TypeFamilies, FlexibleContexts #-}
-- | GPipe ベースのターミナルエミュレータ メインモジュール。
--
-- GPipe + GPipe-GLFW + gpipe-freetype を使用して
-- ターミナル画面をレンダリングする。
-- GLUT/OpenGL/FTGL への依存を完全に排除した新しい実装。
module Hsterm.GPipe.Main (runGPipeTerminal) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException(..), try, catch)
import Control.Monad (unless, when, void)
import Control.Monad.IO.Class (liftIO)
import Data.Array ((!))
import Data.Char (chr, ord)
import Data.IORef
import Data.List (dropWhileEnd)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Word (Word8)
import System.Exit (exitSuccess)
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
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq

import System.Posix.IOCtl (ioctl)
import System.Posix.Signals (signalProcess, sigINT, sigTERM, installHandler, Handler(..))
import System.Posix.Types (CPid)
import Foreign.C.Types (CUShort)

import qualified System.Console.Terminfo as TI
import qualified System.Console.Terminfo.Keys as TI

import Terminal.Terminal (newTerminal, applyAction, setSize)
import Terminal.Types
import Terminal.Posix.TIOCSWINSZ (TIOCSWINSZ(..))
import Terminal.Posix.Winsize (Winsize(..))

import Hsterm.GPipe.Terminal
import Hsterm.GPipe.Renderer

-- ── 選択状態 ──────────────────────────────────────────────

-- | マウスによるテキスト選択の状態。
-- セル座標は 1-indexed の @(行, 列)@。
data Selection = Selection
  { selAnchor :: !(Int, Int)  -- ^ ドラッグ開始位置
  , selCurrent :: !(Int, Int) -- ^ 現在のドラッグ位置
  } deriving (Show, Eq)

-- | 選択範囲を正規化して @(開始, 終了)@ を返す（開始 <= 終了）。
selectionRange :: Selection -> ((Int, Int), (Int, Int))
selectionRange (Selection a b)
  | a <= b    = (a, b)
  | otherwise = (b, a)

-- | ターミナル状態から選択範囲のテキストを抽出する。
-- スクロールバック表示中はスクロールオフセットを考慮する。
extractSelectedText :: Terminal -> Int -> Selection -> String
extractSelectedText term scrollOffset sel =
  let ((r1, c1), (r2, c2)) = selectionRange sel
      r = rows term
      c = cols term
      sbuf = scrollbackBuffer term
      sbLen = Seq.length sbuf
      lookupChar (y, x) =
        if scrollOffset <= 0
          then character (screen term ! (y, x))
          else let vIdx = sbLen - scrollOffset + (y - 1)
               in if vIdx < 0 then ' '
                  else if vIdx < sbLen
                       then let line = Seq.index sbuf vIdx
                            in if x >= 1 && x <= length line
                               then character (line !! (x - 1))
                               else ' '
                       else let screenRow = vIdx - sbLen + 1
                            in if screenRow >= 1 && screenRow <= r && x >= 1 && x <= c
                               then character (screen term ! (screenRow, x))
                               else ' '
      trimEnd = dropWhileEnd (== ' ')
  in if r1 == r2
     -- 1行選択
     then trimEnd [lookupChar (r1, cx) | cx <- [c1..c2]]
     -- 複数行選択
     else let firstLine = trimEnd [lookupChar (r1, cx) | cx <- [c1..c]]
              midLines  = [trimEnd [lookupChar (ry, cx) | cx <- [1..c]] | ry <- [r1+1..r2-1]]
              lastLine  = trimEnd [lookupChar (r2, cx) | cx <- [1..c2]]
          in unlines (firstLine : midLines) ++ lastLine

-- ── 設定 ──────────────────────────────────────────────────

-- | GPipe ターミナルの設定。
data GPipeTermConfig = GPipeTermConfig
  { gpFontPath  :: !FilePath
  , gpFontSize  :: !Int
  , gpColorMap  :: TerminalColor -> Colour Double
  , gpColorMapBright :: TerminalColor -> Colour Double
  , gpDefaultFg :: !TerminalColor
  , gpDefaultBg :: !TerminalColor
  , gpCursorColor :: !(V4 Float)
  }

-- | デフォルトの設定。
defaultGPipeConfig :: GPipeTermConfig
defaultGPipeConfig = GPipeTermConfig
  { gpFontPath  = ""
  , gpFontSize  = 24
  , gpColorMap  = defaultColor
  , gpColorMapBright = defaultColor
  , gpDefaultFg = White
  , gpDefaultBg = Black
  , gpCursorColor = V4 1.0 0.2 0.9 0.8
  }

defaultColor :: TerminalColor -> Colour Double
defaultColor Black   = sRGB24read "#330000"
defaultColor Red     = sRGB24read "#ff6565"
defaultColor Green   = sRGB24read "#56a700"
defaultColor Yellow  = sRGB24read "#eab93d"
defaultColor Blue    = sRGB24read "#204a87"
defaultColor Magenta = sRGB24read "#c4a000"
defaultColor Cyan    = sRGB24read "#89b6e2"
defaultColor White   = sRGB24read "#cccccc"
defaultColor _       = sRGB24read "#cccccc"

defaultColorBright :: TerminalColor -> Colour Double
defaultColorBright Black   = sRGB24read "#555753"
defaultColorBright Red     = sRGB24read "#ff8d8d"
defaultColorBright Green   = sRGB24read "#c8e7a8"
defaultColorBright Yellow  = sRGB24read "#ffc123"
defaultColorBright Blue    = sRGB24read "#3465a4"
defaultColorBright Magenta = sRGB24read "#f57900"
defaultColorBright Cyan    = sRGB24read "#46a4ff"
defaultColorBright White   = sRGB24read "#ffffff"
defaultColorBright _       = sRGB24read "#ffffff"

-- | TerminalColor を V4 Float に変換する。
--
-- 基本8色は bright フラグに応じてパレットを切り替える。
-- Color256 / ColorRGB はパレットに依存せず直接変換する。
colourToV4 :: Bool -> (TerminalColor -> Colour Double) -> (TerminalColor -> Colour Double) -> Bool -> TerminalColor -> V4 Float
colourToV4 _ normalMap brightMap _bright (Color256 n) = color256ToV4 normalMap brightMap n
colourToV4 _ _normalMap _brightMap _bright (ColorRGB r g b) =
  V4 (fromIntegral r / 255.0) (fromIntegral g / 255.0) (fromIntegral b / 255.0) 1.0
colourToV4 _ normalMap brightMap bright tc =
  let cm = if bright then brightMap else normalMap
      RGB r g b = toSRGB (cm tc)
  in  V4 (realToFrac r) (realToFrac g) (realToFrac b) 1.0

-- | 256色パレットのインデックスを V4 Float に変換する。
--
-- * 0–7: 基本8色（normalMap）
-- * 8–15: 明るい色（brightMap）
-- * 16–231: 6×6×6 カラーキューブ
-- * 232–255: グレースケールランプ
color256ToV4 :: (TerminalColor -> Colour Double) -> (TerminalColor -> Colour Double) -> Int -> V4 Float
color256ToV4 normalMap brightMap n
  | n < 0     = color256ToV4 normalMap brightMap 0
  | n < 8     = let basic = [Black, Red, Green, Yellow, Blue, Magenta, Cyan, White] !! n
                    RGB r g b = toSRGB (normalMap basic)
                in  V4 (realToFrac r) (realToFrac g) (realToFrac b) 1.0
  | n < 16    = let basic = [Black, Red, Green, Yellow, Blue, Magenta, Cyan, White] !! (n - 8)
                    RGB r g b = toSRGB (brightMap basic)
                in  V4 (realToFrac r) (realToFrac g) (realToFrac b) 1.0
  | n < 232   = let idx = n - 16
                    ri = idx `div` 36
                    gi = (idx `mod` 36) `div` 6
                    bi = idx `mod` 6
                    toF i = if i == 0 then 0.0
                            else fromIntegral (55 + 40 * i) / 255.0
                in  V4 (toF ri) (toF gi) (toF bi) 1.0
  | n < 256   = let g = fromIntegral (8 + 10 * (n - 232)) / 255.0 :: Float
                in  V4 g g g 1.0
  | otherwise = color256ToV4 normalMap brightMap 255

-- ── エントリーポイント ────────────────────────────────────

-- | fontconfig でモノスペースフォントを探す。
findMonospaceFont :: IO FilePath
findMonospaceFont = do
  result <- try $ System.Process.readProcess "fc-match" ["-f", "%{file}", "monospace:style=Regular"] ""
  case (result :: Either SomeException String) of
    Right path -> return $ head $ lines path
    Left _     -> error "fc-match failed: no monospace font found"

-- | GPipe ターミナルを起動する。
runGPipeTerminal :: IO ()
runGPipeTerminal = do
  fontPath <- findMonospaceFont
  putStrLn $ "Using font: " ++ fontPath

  let cfg = defaultGPipeConfig
        { gpFontPath = fontPath
        , gpColorMap = defaultColor
        , gpColorMapBright = defaultColorBright
        }
      pixelSize = gpFontSize cfg

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

  -- 初期ウィンドウサイズ（80x24ターミナル）
  let initCols = 80
      initRows = 24
      winW = ceiling (cellW * fromIntegral initCols) :: Int
      winH = ceiling (cellH * fromIntegral initRows) :: Int

  -- PTY を起動
  pty <- spawnShell
  putStrLn $ "Shell started, PID: " ++ show (ptyPid pty)

  -- ターミナル状態
  termInfo <- TI.setupTermFromEnv
  termRef <- newIORef $ newTerminal (initRows, initCols) (Just termInfo)
  dirty   <- newIORef True
  scrollOffsetRef <- newIORef (0 :: Int)

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
    let sendPty :: String -> IO ()
        sendPty s = do
          hPutStr stderr $ "sendPty: " ++ show (map ord s) ++ "\n"
          hPutStr (ptyMaster pty) s
          hFlush (ptyMaster pty)

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
        sendPty [ch]

    -- キー入力コールバック（制御キー用）
    _ <- GLFW.setKeyCallback win $ Just $ \key _scancode keyState mods -> do
      let ctrl = modifierKeysControl mods
      -- Ctrl の状態を記録
      liftIO $ writeIORef ctrlHeld ctrl
      when (keyState == KeyState'Pressed || keyState == KeyState'Repeating) $ do
        let shift = modifierKeysShift mods
            isScrollKey = shift && key `elem` [Key'PageUp, Key'PageDown, Key'Home, Key'End]
            isClipboardKey = ctrl && shift && key `elem` [Key'C, Key'V]
        -- Ctrl+Shift+C: 選択テキストをクリップボードにコピー
        when (ctrl && shift && key == Key'C) $ do
          sel <- readIORef selectionRef
          case sel of
            Just s -> do
              term <- readIORef termRef
              scrollOff <- readIORef scrollOffsetRef
              let txt = extractSelectedText term scrollOff s
              unless (null txt) $ writeIORef clipboardWriteRef (Just txt)
            Nothing -> return ()
        -- Ctrl+Shift+V: クリップボードからペースト
        when (ctrl && shift && key == Key'V) $ do
          writeIORef pasteRequestRef True
        -- スクロールバック操作（Shift+PageUp/Down/Home/End）
        when (shift && key == Key'PageUp) $ do
          term <- readIORef termRef
          let maxOff = Seq.length (scrollbackBuffer term)
              pageSize = max 1 (rows term - 1)
          modifyIORef' scrollOffsetRef (\off -> min maxOff (off + pageSize))
          writeIORef dirty True
        when (shift && key == Key'PageDown) $ do
          term <- readIORef termRef
          let pageSize = max 1 (rows term - 1)
          modifyIORef' scrollOffsetRef (\off -> max 0 (off - pageSize))
          writeIORef dirty True
        when (shift && key == Key'Home) $ do
          term <- readIORef termRef
          writeIORef scrollOffsetRef (Seq.length (scrollbackBuffer term))
          writeIORef dirty True
        when (shift && key == Key'End) $ do
          writeIORef scrollOffsetRef 0
          writeIORef dirty True
        -- 通常キー入力時はスクロール位置をリセット
        when (not isScrollKey && not isClipboardKey) $ writeIORef scrollOffsetRef 0
        when (not isScrollKey && not isClipboardKey) $ case key of
          -- Ctrl+Q で強制終了
          Key'Q | ctrl -> do
            signalProcess sigINT (ptyPid pty)
            exitSuccess

          -- Ctrl+文字 → 制御コード (Ctrl-A=0x01 ... Ctrl-Z=0x1A)
          _ | ctrl, Just c <- keyToAlpha key -> do
            sendPty [chr (ord c - ord 'a' + 1)]

          -- Enter → CR
          Key'Enter     -> sendPty "\r"
          -- Backspace → DEL (0x7F)
          Key'Backspace -> sendPty "\x7f"
          -- Tab → HT
          Key'Tab       -> sendPty "\t"
          -- Escape → ESC
          Key'Escape    -> sendPty "\x1b"

          -- 特殊キー → terminfo エスケープシーケンス
          _ -> do
            term <- readIORef termRef
            case terminfo term of
              Just tiHandle ->
                case lookup key terminalKeyMap of
                  Just cap ->
                    case TI.getCapability tiHandle cap of
                      Just str -> sendPty str
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
              sendPty $ encodeMouseEvent enc cb col row True
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
            sendPty $ encodeMouseEvent enc btn col row isPress
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
            sendPty $ encodeMouseEvent enc btn col row True
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
    mainLoop win fontTex atlas cfg termRef dirty winSizeRef scrollOffsetRef selectionRef clipboardWriteRef pasteRequestRef sendPty (ptyPid pty)

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
  -> GPipeTermConfig
  -> IORef Terminal
  -> IORef Bool
  -> IORef (V2 Int)
  -> IORef Int             -- ^ scrollOffsetRef
  -> IORef (Maybe Selection) -- ^ selectionRef
  -> IORef (Maybe String)  -- ^ clipboardWriteRef
  -> IORef Bool            -- ^ pasteRequestRef
  -> (String -> IO ())     -- ^ sendPty
  -> CPid
  -> ContextT GLFW.Handle os IO ()
mainLoop win fontTex atlas cfg termRef dirty winSizeRef scrollOffsetRef selectionRef clipboardWriteRef pasteRequestRef sendPty pid = do
  -- ウィンドウサイズを取得
  winSize <- liftIO $ readIORef winSizeRef

  -- シェーダーをコンパイル（3種: 背景・テキスト・カーソル）
  textShader <- compileTextShader win winSize
  bgShader   <- compileBgShader win winSize
  curShader  <- compileCursorShader win winSize

  -- 背景色の RGB
  let RGB bgR bgG bgB = toSRGB (gpColorMap cfg Black)
      clearCol = V4 (realToFrac bgR) (realToFrac bgG) (realToFrac bgB) (1.0 :: Float)

  -- 初期ターミナルサイズから固定グリッドバッファ確保
  term0 <- liftIO $ readIORef termRef
  let r0 = rows term0
      c0 = cols term0
      gridCap = r0 * c0 * 6

  bgGridBuf  <- newBuffer gridCap
  txtGridBuf <- newBuffer gridCap
  curBuf     <- newBuffer (6 :: Int)  -- カーソルは最大6頂点

  -- リサイズ時のみ再生成する固定グリッド頂点を書き込む
  writeBuffer bgGridBuf 0 (buildBgGridVertices atlas r0 c0)
  writeBuffer txtGridBuf 0 (buildTextGridVertices atlas r0 c0)

  -- セルデータテクスチャ（cols×rows, RGBA32F）
  bgColorTex'  <- newTexture2D RGBA32F (V2 c0 r0) 1
  fgColorTex'  <- newTexture2D RGBA32F (V2 c0 r0) 1
  glyphUVTex'  <- newTexture2D RGBA32F (V2 c0 r0) 1
  glyphPosTex' <- newTexture2D RGBA32F (V2 c0 r0) 1

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
  where
    colorFn = colourToV4 False (gpColorMap cfg) (gpColorMapBright cfg)
    nearestFilter = SamplerFilter Nearest Nearest Nearest Nothing

    go textShader bgShader curShader prevSize clearCol
       bgGridRef txtGridRef curBufRef curLenRef prevDimRef
       bgTexRef fgTexRef uvTexRef posTexRef = do
      -- フレームレート制限 ≒ 60fps
      liftIO $ threadDelay 16000

      -- クリップボード操作（ContextT 内で実行する必要があるため、ここで処理）
      -- コピー: clipboardWriteRef に text があればクリップボードに書き込む
      mCopyText <- liftIO $ readIORef clipboardWriteRef
      case mCopyText of
        Just txt -> do
          _ <- GLFW.setClipboardString win txt
          liftIO $ writeIORef clipboardWriteRef Nothing
        Nothing -> return ()
      -- ペースト: pasteRequestRef が True ならクリップボードから読み取って PTY に送信
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
            ts <- compileTextShader win curSize
            bs <- compileBgShader win curSize
            cs <- compileCursorShader win curSize
            return (ts, bs, cs)
          else return (textShader, bgShader, curShader)

      isDirty <- liftIO $ readIORef dirty
      liftIO $ writeIORef dirty False

      -- ターミナル状態を取得
      term <- liftIO $ readIORef termRef
      let r = rows term
          c = cols term
          gridLen = r * c * 6

      -- ターミナルサイズが変わったら固定グリッドバッファとテクスチャを再確保
      prevDim <- liftIO $ readIORef prevDimRef
      let resized = (r, c) /= prevDim
      when resized $ do
        liftIO $ writeIORef prevDimRef (r, c)

        -- 固定グリッドバッファ再生成
        bg'  <- newBuffer gridLen
        txt' <- newBuffer gridLen
        writeBuffer bg' 0 (buildBgGridVertices atlas r c)
        writeBuffer txt' 0 (buildTextGridVertices atlas r c)
        liftIO $ writeIORef bgGridRef bg'
        liftIO $ writeIORef txtGridRef txt'

        -- セルデータテクスチャ再生成
        bgT  <- newTexture2D RGBA32F (V2 c r) 1
        fgT  <- newTexture2D RGBA32F (V2 c r) 1
        uvT  <- newTexture2D RGBA32F (V2 c r) 1
        posT <- newTexture2D RGBA32F (V2 c r) 1
        liftIO $ writeIORef bgTexRef bgT
        liftIO $ writeIORef fgTexRef fgT
        liftIO $ writeIORef uvTexRef uvT
        liftIO $ writeIORef posTexRef posT

      -- セルデータテクスチャを dirty 時のみ更新
      -- テクスチャは GPU に保持されるため、変更がなければ再アップロード不要。
      bgTex  <- liftIO $ readIORef bgTexRef
      fgTex  <- liftIO $ readIORef fgTexRef
      uvTex  <- liftIO $ readIORef uvTexRef
      posTex <- liftIO $ readIORef posTexRef

      when (isDirty || resized) $ do
        scrollOff <- liftIO $ readIORef scrollOffsetRef
        sel <- liftIO $ readIORef selectionRef
        let selRange = fmap selectionRange sel
            (bgColors, fgColors, glyphUVs, glyphPositions) =
              buildCellData atlas term colorFn scrollOff selRange
            texSize = V2 c r

        writeTexture2D bgTex  0 0 texSize bgColors
        writeTexture2D fgTex  0 0 texSize fgColors
        writeTexture2D uvTex  0 0 texSize glyphUVs
        writeTexture2D posTex 0 0 texSize glyphPositions

        -- カーソル頂点を更新（スクロールバック表示中はカーソル非表示）
        let cursorVerts = if scrollOff > 0
                          then []
                          else buildCursorVertices atlas term (gpCursorColor cfg)
            curLen = length cursorVerts
        liftIO $ writeIORef curLenRef curLen
        when (curLen > 0) $ do
          curBuf <- liftIO $ readIORef curBufRef
          writeBuffer curBuf 0 cursorVerts

      -- 描画
      bgGridBuf  <- liftIO $ readIORef bgGridRef
      txtGridBuf <- liftIO $ readIORef txtGridRef
      curBuf     <- liftIO $ readIORef curBufRef
      curLen     <- liftIO $ readIORef curLenRef

      render $ do
        clearWindowColor win clearCol

        -- 背景色描画（bgColorTex から色を取得）
        do arr <- newVertexArray bgGridBuf
           bgShader' $ BgShaderEnv
             { bgPrimitives = toPrimitiveArray TriangleList (takeVertices gridLen arr)
             , bgColorTex = (bgTex, nearestFilter, (pure ClampToEdge, V4 0 0 0 0))
             }

        -- カーソル描画
        when (curLen > 0) $ do
          arr <- newVertexArray curBuf
          curShader' $ CursorShaderEnv
            { curPrimitives = toPrimitiveArray TriangleList (takeVertices curLen arr)
            }

        -- テキスト描画（セルデータテクスチャ + フォントアトラス）
        do arr <- newVertexArray txtGridBuf
           textShader' $ TextShaderEnv
             { tePrimitives  = toPrimitiveArray TriangleList (takeVertices gridLen arr)
             , teFontAtlas   = (fontTex, SamplerFilter Linear Linear Linear Nothing, (pure ClampToEdge, 0))
             , teFgColorTex  = (fgTex, nearestFilter, (pure ClampToEdge, V4 0 0 0 0))
             , teGlyphUVTex  = (uvTex, nearestFilter, (pure ClampToEdge, V4 0 0 0 0))
             , teGlyphPosTex = (posTex, nearestFilter, (pure ClampToEdge, V4 0 0 0 0))
             }

      -- ★ swapWindowBuffers は GLFW イベントポーリングも兼ねる
      swapWindowBuffers win

      -- ウィンドウクローズ判定
      closeReq <- GLFW.windowShouldClose win
      case closeReq of
        Just True ->
          -- シェルプロセスを終了
          liftIO $ signalProcess sigINT pid
        _ ->
          go textShader' bgShader' curShader' curSize clearCol
             bgGridRef txtGridRef curBufRef curLenRef prevDimRef
             bgTexRef fgTexRef uvTexRef posTexRef

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
keyToAlpha Key'Q = Just 'q'   -- Ctrl+Q は上で exitSuccess に使っている
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
