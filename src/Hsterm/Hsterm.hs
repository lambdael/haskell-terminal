{-# LANGUAGE Rank2Types #-}
module Hsterm.Hsterm where
import System.Process hiding (createPipe)
import System.Process.Internals (ProcessHandle__(..), withProcessHandle)
import Data.Array.Diff
import Data.IORef
import Data.List
import Data.Char
import Data.Maybe (fromJust, isJust, fromMaybe)
import Data.Ord ((<=),(>=)) 
-- import Control.Monad
import Control.Monad.State hiding (state, get, State)
import System.IO
import Control.Arrow
import Control.Concurrent.Thread.Delay

import Foreign.C.Types


import Data.Time.Clock
import Data.Time.Calendar
import Data.Colour.SRGB (RGB(..), toSRGB)
import Data.Colour (Colour(..))

import Graphics.UI.GLUT hiding (Bool, Float, fontWidth, fontHeight, RGB, renderer, cursor)
import Graphics.Rendering.OpenGL hiding (Bool, Float, get, RGB, renderer)
import Graphics.Rendering.OpenGL.GL.Shaders.Uniform
import Graphics.Rendering.OpenGL.GLU (perspective)
import Graphics.Rendering.OpenGL.GL.FramebufferObjects
import Graphics.Rendering.OpenGL.GL.Texturing.Environments
import Graphics.Rendering.FTGL (createTextureFont, renderFont, setFontFaceSize, RenderMode(..), getFontAdvance, getFontBBox)
import System.Posix.IO
import System.Posix.Terminal hiding (TerminalState)
import qualified GHC.IO.Handle.FD as FD
import System.Posix.IOCtl
import Control.Concurrent
import Control.Applicative hiding (many)
import System.Environment

import qualified System.Console.Terminfo  as TI
import qualified System.Console.Terminfo.Keys  as TI
import Terminal.Parser (parseANSI)
import Terminal.Terminal
import Terminal.Types
import qualified Terminal.Types as T
import qualified GHC.IO.Device as Dev

import System.Posix.Types (CPid, Fd)
import System.Posix.Signals
import System.Posix.Terminal
import Hsterm.State
-- import Hsterm.ShaderUtils
import Hsterm.Config
import qualified Hsterm.Renderer as R

import Terminal.Posix.TIOCSWINSZ
import Terminal.Posix.TIOCGWINSZ
import Terminal.Posix.Winsize

import Control.Exception


-- Constants (for now)
numColumns = 80
numRows = 24
dpi = 72
initDisplay cfg = do
  _ <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered, RGBAMode]
  createWindow "Haskell terminal emulator"
  let
      Color3 r g b = (getColor cfg) False (defaultBackgroundColor cfg)
      bg = Color4 r g b 1.0
  clearColor $= bg 
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  
glColor :: Colour Double -> Color3 GLfloat
glColor c = Color3 (realToFrac r) (realToFrac g) (realToFrac b)
    where RGB r g b = toSRGB c

unitQuad = do
  let texCoord2f = texCoord :: TexCoord2 GLfloat -> IO ()
      vertex3f = vertex :: Vertex3 GLfloat -> IO ()
  renderPrimitive Quads $ do
    texCoord2f (TexCoord2 1 1); vertex3f (Vertex3 (-0) (-0)   0 )
    texCoord2f (TexCoord2 1 0); vertex3f (Vertex3 (-0)   1    0 )
    texCoord2f (TexCoord2 0 0); vertex3f (Vertex3   1    1    0 )
    texCoord2f (TexCoord2 0 1); vertex3f (Vertex3   1  (-0)   0 )

winSizeToRowCol (Size winw winh) (Size fontw fonth) = 
  let
    ww = fromIntegral winw
    fw = fromIntegral fontw
    wh = fromIntegral winh
    fh = fromIntegral fonth
    rw = truncate (ww / (fw))
    rh = truncate (wh / (fh))
   in Size rw rh

reshapeHandler state size@(Size w_ h_) = do
  fs@(fh,fw) <- get $ fontSizeF state
  -- fh <- get $ fontHeight state
  -- fw <- get $ fontWidth state
  term' <- readIORef $ terminal state
  
  let
    w' = fromIntegral w_
    h' = fromIntegral h_
  let
    term = setSize (truncate (h' / (fh))::Int , truncate(w' / (fw))::Int) term'

  writeIORef (terminal state) term

  matrixMode $= Projection
  windowSize $= size
  loadIdentity
  ortho (0::GLdouble) (fromIntegral w_) 0 (fromIntegral h_) 0 1
  hIn  <- get $ inp state
  
  let ti = terminfo term
  -- TI.hRunTermOutput hIn ti $ TI.termText $ read $ "\ESC8;" ++ (show (rows newterm)) ++ ";" ++ (show (cols newterm)) ++ "t" 
  let
    s = "\\ESC8;" ++ (show (rows term)) ++ ";" ++ (show (cols term)) ++ "t" 

    s' = '"' : s ++ "\""
  let pid' = pid state
  
  let 
    s_col = fromIntegral $ cols term :: CUShort
    s_rows = fromIntegral $ rows term :: CUShort
    s_xpix = fromIntegral $ w_ :: CUShort
    s_ypix = fromIntegral $ h_ :: CUShort


  let wsize = Winsize {
                ws_col = s_col,
                ws_row = s_rows,
                ws_xpixel = s_xpix,
                ws_ypixel = s_ypix
        }
  let pterm = pseudoterminal state

  ioctl pterm TIOCSWINSZ wsize 


  -- ws <- ioctl' pterm TIOCGWINSZ 
   -- hPutStr hIn $ read s' 
  -- print $ (show ((cols term), (rows term))) ++ " -> " ++ (show (s_col , s_rows))
  -- putStrLn $ "My terminal is " ++ show ((ws_col ws), (ws_row ws)) ++ " XD"
  signalProcess (28::Signal) pid' -- SIGWICH=28 maybe..
  return ()
  -- screen size may differ from window size
screenViewPort state = do
  winSize@(Size w_ h_) <- get windowSize
  ss@(Size w h) <- getScreenSizePix state
  -- windowSize $= size
  let
    dx = w_ - w
    dy = h_ - h
  return $ (Position 0 dy, winSize)

-- whole window viewport
windowViewPort state = do
  winSize@(Size w h) <- get windowSize
  return $ (Position 0 0, winSize)

getColor ::  TerminalConfig  -> Bool -> TerminalColor -> Color3 GLfloat
getColor cfg bright c = glColor (cm cfg c)
   where cm = if bright then colorMapBright else colorMap

myOrtho2d (Position x y, Size w h) = let
  left = fromIntegral x :: GLfloat
  right = left + (fromIntegral w) :: GLfloat
  top = fromIntegral y :: GLfloat
  bottom = top + (fromIntegral h) :: GLfloat
  w' = (fromIntegral w) :: GLfloat 
  h' = (fromIntegral h) :: GLfloat 
  in newMatrix RowMajor [
    1.0/w', 0.0, 0.0, 0.0,
    0.0, 2.0/h', 0.0, 0.0,
    0.0, 0.0, 1.0, 0.0,
    0.0, 0.0, 0.0, 1.0
   ]


displayHandler :: State -> IO ()
displayHandler state = do
  term <- get $ terminal state

  -- clear [ColorBuffer]

  matrixMode $= Projection
  loadIdentity
  matrixMode $= Modelview 0
  loadIdentity
  
  Just font <- get $ currentFont state
  
  fs@(fontHeight',fontWidth') <- get $ fontSizeF state
  -- fontHeight' <- get $ fontHeight state
  -- fontWidth' <- get $ fontWidth state
  let cfg = config state
  winSize@(Size winWidth winHeight) <- get $ windowSize 
  let
      fontWidth'' :: GLfloat
      fontWidth'' = fontWidth'
      fontHeight'' = fontHeight':: GLfloat
      setColor :: Bool -> TerminalColor -> IO ()
      setColor bright c = do color (getColor cfg bright c)
      diffH = winHeight `mod` (truncate fontHeight')
      toViewPort :: Int -> Int -> (Position , Size)
      toViewPort x y = ((Position (truncate sx) (truncate sy)), (Size (truncate fontWidth') (truncate fontHeight'))) 
        where sx = fontWidth' * (fromIntegral x - 1.0) 
              sy = fontHeight' * fromIntegral ((rows term) - y) + (fromIntegral diffH) 
              sz = 0 :: GLfloat
      toScreen :: Int -> Int -> Vector3 (GLfloat)
      toScreen x y = Vector3 sx sy sz
        where sx = fontWidth'' * (fromIntegral x - 1) :: GLfloat
              sy = fontHeight'' * fromIntegral ((rows term) - y) 
              sz = 0 :: GLfloat 
      blendQuad =
        preservingMatrix (scale fontWidth'' fontHeight'' 1.0 >> unitQuad)

  let getSeconds = getCurrentTime >>= return . fromRational . toRational . utctDayTime
  currentTime@(UTCTime d s) <-getCurrentTime  
  t <- getSeconds 

  wv@(_, Size w h) <- windowViewPort state
  sv <- screenViewPort state
  viewport $= wv
  loadIdentity
  matrixMode $= Modelview 0
  loadIdentity
  let bgr = bg state
  -- print $ "aaaa" ++ (show sv)
  m <- myOrtho2d wv
  let renderData = R.RenderData {
         R.mat = m,
         R.time = t
                              }
  bgr renderData
  matrixMode $= Projection 
  ortho (0::GLdouble) (fromIntegral w) 0 (fromIntegral h) 0 1
  viewport $= sv
  matrixMode $= Modelview 0
  currentProgram $= Nothing
  forM_ (indices $ screen term) $ \idx@(y, x) ->
    preservingMatrix $ do
        let tc = screen term ! idx
            bgc = if isInverse tc then foregroundColor tc else backgroundColor tc
            fgc = if isInverse tc then backgroundColor tc else foregroundColor tc
        translate $ toScreen x y

        -- Render a quad in the background color
        if ( bgc /= (defaultBackgroundColor cfg))
          then do
            setColor False bgc
            blendQuad
          else return () -- skip default background color
        -- Render a font in the foreground color
        setColor (isBright tc) fgc
        translate $ Vector3 (0.0::GLfloat) ((fontHeight'::GLfloat) * 0.2) (0)
        renderFont font [character tc] All

  -- Cursor
  case (optionShowCursor term) of
    True -> do
      let (y, x) = cursorPos term
      -- translate $ toScreen x y
      -- color $ glColor $ cursorColor cfg
      -- blendQuad
      let cur = cursor state
      m <- myOrtho2d (toViewPort x y)
      viewport $= toViewPort x y
      let mx = m::GLmatrix GLfloat
      cur $ renderData {
        R.mat = m
                       }
      currentProgram $= Nothing

  

    _ -> return ()

  swapBuffers
  -- delay (1000 * 30) 
runTerminal :: IORef Terminal -> Handle -> Handle -> IO ()
runTerminal a in_ out =
    forever $ do
        c <- (liftIO $ hGetChar out)
        s <- readIORef a

        -- Parse the input buffer for characters or ANSI sequences
        Right (actions, leftover) <- return $ parseANSI $ inBuffer s ++ [c]

        -- Apply all the actions to the terminal state
        forM_ actions $ \x -> modifyIORef a $ flip applyAction x

        -- Store the actions that could not be parsed as input buffer
        modifyIORef a $ \term -> term { inBuffer = leftover }

        -- delay (1000 * 1)

keyboardMouseHandler state (Char c) Down modifiers position = do
  term <- get $ terminal state
  hIn  <- get $ inp state
  
  let ti = terminfo term
  TI.hRunTermOutput hIn ti $ TI.termText [c] 
    -- hPutChar hInWrite c
  hFlush hIn
keyboardMouseHandler state (SpecialKey c) Down modifiers position = do
  term <- get $ terminal state
  hIn  <- get $ inp state
  let ti = terminfo term
  let cmd = lookup c terminalKeys
  -- let termOut = (TI.getCapability ti cmd)
  if isJust cmd then
    TI.hRunTermOutput hIn ti $ TI.termText $ fromMaybe "" $ TI.getCapability ti $ fromJust cmd
    else return ()
  hFlush hIn

keyboardMouseHandler state chr st modifiers position = return ()

terminalKeys =
  [ (KeyUp, TI.keyUp)
  , (KeyDown, TI.keyDown)

  ]

getScreenSizePix ::  State  -> IO Size
getScreenSizePix state = do
    fs@(fontHeight',fontWidth') <- get $ fontSizeF state
    -- fontWidth' <- get $ fontWidth state
    -- fontHeight' <- get $ fontHeight state
    term <- readIORef $ terminal state
    let
      rws = rows term
      cls = cols term
    -- print $ "screensize " ++ (show rws) ++ ", " ++ (show cls)
    return (Size (round (fontWidth' * realToFrac (cls))) (round (fontHeight' *  realToFrac (rws))))



runHsterm :: TerminalConfig -> IO ()
runHsterm cfg = do
    
    initDisplay cfg 
    ss@ (Size w h) <- get windowSize
    -- Size c r = winSizeToRowCol ss 
    -- environment variables
    let environment = [
            ("TERM", "xterm")
            ]
        cmdParams =[]
    
    rawenv <- getEnvironment
    let shell = fromMaybe "bash" $ lookup "SHELL" rawenv
    let env' = override rawenv environment


    -- create pseudo terminal and handles
    let 
      toH fd = FD.mkHandleFromFD fd Dev.Stream "mypath" ReadWriteMode False Nothing 
      getHandles :: (Fd,Fd) -> IO (Handle, Handle)
      getHandles pseudoterm =
          (uncurry ap . (fmap (,) . fdToHandle *** fdToHandle ) ) pseudoterm

    pt@(masterFd, slaveFd) <- openPseudoTerminal
    (master, slave) <- getHandles pt

    -- create process
    (inp',out',err',ph') <- createProcess_ "Mehhhh" $ (proc shell cmdParams){
      env = Just env',
      create_group = True,
      delegate_ctlc = True,

      new_session = True,
      std_in  = (UseHandle slave),
      std_out = (UseHandle slave),
      std_err =  (UseHandle slave)}

    let
      inp = master
      out = master
      err = master
  -- | returns Just pid or Nothing if process has already exited
    let getPid ph = withProcessHandle ph go
        go ph_ = case ph_ of
           OpenHandle x   -> return $ Just x
           ClosedHandle _ -> return Nothing
    pid' <- getPid ph'
    let pid'' = fromJust pid'

    -- run gui
    do
      hFlush inp
      state <- makeState cfg inp out err pid'' masterFd
      forkIO $ runTerminal (terminal state) inp out
      initUI cfg state  
      signalProcess sigINT pid''
      `catch` \(SomeException e) -> do -- Have you ever seen ZOMBI?
        print e
        signalProcess sigINT pid''


initUI cfg state = do 

    ss <- getScreenSizePix state
 
    -- windowSize $= ss

    displayCallback $= displayHandler state
    idleCallback $= Just (postRedisplay Nothing)
    reshapeCallback $= Just (reshapeHandler state)

    keyboardMouseCallback $= Just (keyboardMouseHandler state)
      
    -- print $ "hOutRead" ++ show hOutRead
    -- print $ "hOutWrite" ++ show hOutWrite
    -- hPutChar inp 's'
    mainLoop 

mp (x:xs) = show x ++ "\n" ++ mp xs
mp [] = ""

override :: [(String,String)] ->  [(String,String)] -> [(String,String)]
override source rider =
  let val key zipped = lookup key zipped
      (skeys, svals) = unzip source
      (rkeys, rvals) = unzip rider
      keys = unionBy (\x -> \y -> (x <= y) && (x >= y)) skeys rkeys
      pick  ss rs key = if isJust (val key rs) then (key , fromJust(val key rs))
                        else (key , fromJust(val key ss))
 
  in  map (pick source rider) keys 

