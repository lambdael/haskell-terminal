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
import Data.Char (chr, ord)
import Data.IORef
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Word (Word8)
import System.Exit (exitSuccess)
import System.IO
import qualified System.Process (readProcess)

import Data.Colour.SRGB (RGB(..), toSRGB, sRGB24read)
import Data.Colour (Colour)

import Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import Graphics.GPipe.Context.GLFW (Key(..), KeyState(..), ModifierKeys(..))

import Graphics.GPipe.Font
import Graphics.GPipe.Font.Types (GlyphMetrics(..), GlyphRegion(..))
import qualified Data.Map.Strict as Map

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
  atlas <- buildAtlas (defaultTextConfig fontPath) { tcPixelSize = pixelSize }
  putStrLn $ "Atlas: " ++ show (faWidth atlas) ++ "x" ++ show (faHeight atlas)
    ++ ", " ++ show (Map.size (faGlyphs atlas)) ++ " glyphs"

  -- セルサイズを計算
  let cellW = case Map.lookup 'M' (faGlyphs atlas) of
        Just gr -> fromIntegral (gmAdvanceX (grMetrics gr))
        Nothing -> fromIntegral (faLineH atlas) * 0.5 :: Float
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
      unless isCtrl $ sendPty [ch]

    -- キー入力コールバック（制御キー用）
    _ <- GLFW.setKeyCallback win $ Just $ \key _scancode keyState mods -> do
      let ctrl = modifierKeysControl mods
      -- Ctrl の状態を記録
      liftIO $ writeIORef ctrlHeld ctrl
      when (keyState == KeyState'Pressed || keyState == KeyState'Repeating) $ do
        case key of
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
    mainLoop win fontTex atlas cfg termRef dirty winSizeRef (ptyPid pty)

-- | メインループ。
--
-- GPipe-GLFW は swapWindowBuffers の中で GLFW イベントをポーリングする。
-- render を呼ばずに swapWindowBuffers だけ呼ぶと GL コンテキストが
-- カレントでないため GLX BadAccess が発生する。
-- そのため、毎フレーム必ず render + swap を行う。
-- ダブルバッファリングでは swap 後のバックバッファ内容は未定義なので
-- idle フレームでも全画面を再描画する必要がある。
mainLoop
  :: Window os RGBAFloat ()
  -> Texture2D os (Format RFloat)
  -> FontAtlas
  -> GPipeTermConfig
  -> IORef Terminal
  -> IORef Bool
  -> IORef (V2 Int)
  -> CPid
  -> ContextT GLFW.Handle os IO ()
mainLoop win fontTex atlas cfg termRef dirty winSizeRef pid = do
  -- ウィンドウサイズを取得
  winSize <- liftIO $ readIORef winSizeRef

  -- シェーダーをコンパイル
  textShader <- compileTextShader win winSize
  bgShader   <- compileBgShader win winSize

  -- 背景色の RGB
  let RGB bgR bgG bgB = toSRGB (gpColorMap cfg Black)
      clearCol = V4 (realToFrac bgR) (realToFrac bgG) (realToFrac bgB) (1.0 :: Float)

  go textShader bgShader winSize clearCol
  where
    colorFn = colourToV4 False (gpColorMap cfg) (gpColorMapBright cfg)

    go textShader bgShader prevSize clearCol = do
      -- フレームレート制限 ≒ 60fps
      liftIO $ threadDelay 16000

      curSize <- liftIO $ readIORef winSizeRef

      -- ウィンドウサイズが変わったらシェーダーを再コンパイル
      (textShader', bgShader') <-
        if curSize /= prevSize
          then do
            ts <- compileTextShader win curSize
            bs <- compileBgShader win curSize
            return (ts, bs)
          else return (textShader, bgShader)

      -- dirty フラグをクリア
      liftIO $ writeIORef dirty False

      -- ターミナル状態を読み取る
      term <- liftIO $ readIORef termRef

      -- 毎フレーム頂点データを生成
      let bgVerts     = buildBgVertices atlas term colorFn (gpDefaultBg cfg)
          cursorVerts = buildCursorVertices atlas term (gpCursorColor cfg)
          textVerts   = buildTextVertices atlas term colorFn (gpDefaultFg cfg)

      -- バッファ確保・書き込み（ContextT レベル）
      bgData <- if null bgVerts
        then return Nothing
        else do
          buf <- newBuffer (length bgVerts)
          writeBuffer buf 0 bgVerts
          return (Just buf)

      curData <- if null cursorVerts
        then return Nothing
        else do
          buf <- newBuffer (length cursorVerts)
          writeBuffer buf 0 cursorVerts
          return (Just buf)

      txtData <- if null textVerts
        then return Nothing
        else do
          buf <- newBuffer (length textVerts)
          writeBuffer buf 0 textVerts
          return (Just buf)

      -- 描画（Render モナド）
      render $ do
        clearWindowColor win clearCol

        -- 背景色セル描画
        case bgData of
          Just bgBuf -> do
            arr <- newVertexArray bgBuf
            bgShader' $ BgShaderEnv (toPrimitiveArray TriangleList arr)
          Nothing -> return ()

        -- カーソル描画
        case curData of
          Just curBuf -> do
            arr <- newVertexArray curBuf
            bgShader' $ BgShaderEnv (toPrimitiveArray TriangleList arr)
          Nothing -> return ()

        -- テキスト描画
        case txtData of
          Just textBuf -> do
            arr <- newVertexArray textBuf
            textShader' $ TextShaderEnv
              { tePrimitives = toPrimitiveArray TriangleList arr
              , teTexture    = (fontTex, SamplerFilter Linear Linear Linear Nothing, (pure ClampToEdge, 0))
              }
          Nothing -> return ()

      -- ★ swapWindowBuffers は GLFW イベントポーリングも兼ねる
      swapWindowBuffers win

      -- ウィンドウクローズ判定
      closeReq <- GLFW.windowShouldClose win
      case closeReq of
        Just True ->
          -- シェルプロセスを終了
          liftIO $ signalProcess sigINT pid
        _ ->
          go textShader' bgShader' curSize clearCol

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
  ]
