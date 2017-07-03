{-# LANGUAGE Rank2Types #-}
module Hsterm.Hsterm where
import System.Process hiding (createPipe)
import Data.Array.Diff
import Data.IORef
import Data.List
import Data.Char
import Data.Maybe (fromJust, isJust, fromMaybe)
import Data.Ord ((<=),(>=)) 
-- import Control.Monad
import Control.Monad.State hiding (state, get, State)
import System.IO

import Data.Time.Clock
import Data.Time.Calendar
import Data.Colour.SRGB (RGB(..), toSRGB)
import Data.Colour (Colour(..))

import Graphics.UI.GLUT hiding (Bool, Float, fontWidth, fontHeight, RGB)
import Graphics.Rendering.OpenGL hiding (Bool, Float, get, RGB)
import Graphics.Rendering.OpenGL.GL.Shaders.Uniform
import Graphics.Rendering.OpenGL.GLU (perspective)
import Graphics.Rendering.OpenGL.GL.FramebufferObjects
import Graphics.Rendering.OpenGL.GL.Texturing.Environments
import Graphics.Rendering.FTGL (createTextureFont, renderFont, setFontFaceSize, RenderMode(..), getFontAdvance, getFontBBox)
import System.Posix.IO
import System.Posix.Terminal hiding (TerminalState)
import Control.Concurrent
import Control.Applicative hiding (many)
import System.Environment

import qualified System.Console.Terminfo  as TI
import qualified System.Console.Terminfo.Keys  as TI
import Terminal.Parser (parseANSI)
import Terminal.Terminal
import Terminal.Types
import qualified Terminal.Types as T



import Hsterm.State
import Hsterm.ShaderUtils
import Hsterm.Config

-- Constants (for now)
numColumns = 80
numRows = 24
dpi = 72
initDisplay bgc = do
  _ <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered, RGBAMode]
  createWindow "Haskell terminal emulator"
  -- TODO
  clearColor $= bgc 
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

reshapeHandler state size@(Size w_ h_) = do
  fh <- get $ fontHeight state
  fw <- get $ fontWidth state
  let
    w' = fromIntegral w_
    h' = fromIntegral h_
    --fh' = fromIntegral fh
    --fw' = fromIntegral fw
  term <- readIORef $ terminal state
  let
    newterm = setSize (truncate (h' / (fh))::Int , truncate(w' / (fw))::Int) term

  print $ "re shape screensize to " ++ (show w') ++ (show h') 
  print $ "re shape screensize in " ++ (show fh) ++ (show fw) 
  writeIORef (terminal state) newterm
  -- set state {terminal = setSize (truncate (h' / (fh'))::Int , truncate(w' / (fw'))::Int) term}

  ss@(Size w h) <- getScreenSize state
  windowSize $= size
  let
    dx = w_ - w
    dy = h_ - h
  print $ "re shape screensize " ++ (show size) ++ (show ss) 
  viewport $= (Position 0 dy, size)
  matrixMode $= Projection
  loadIdentity
  ortho (0::GLdouble) (fromIntegral w_) 0 (fromIntegral h_) 0 1

getColor :: TerminalConfig -> Bool -> TerminalColor -> Color3 GLfloat
getColor cfg bright c = glColor (cm cfg c)
   where cm = if bright then colorMapBright else colorMap




displayHandler :: State -> IO ()
displayHandler state = do
  term <- get $ terminal state

  clear [ColorBuffer]
  matrixMode $= Modelview 0

  loadIdentity

  Just font <- get $ currentFont state
  fontHeight' <- get $ fontHeight state
  fontWidth' <- get $ fontWidth state
  cfg <- get $ config state


  let fontWidth'' = realToFrac fontWidth'
      fontHeight'' = realToFrac fontHeight'
      setColor :: Bool -> TerminalColor -> IO ()
      setColor bright c = do color (getColor cfg bright c)
      toScreenCoordinates :: Int -> Int -> Vector3 GLfloat
      toScreenCoordinates x y = Vector3 sx sy sz
        where sx = fontWidth'' * (fromIntegral x - 1)
              sy = fontHeight'' * fromIntegral ((rows term) - y)
              sz = 0 :: GLfloat
      blendQuad =
        preservingMatrix (scale fontWidth'' fontHeight'' 1.0 >> unitQuad)

  forM_ (indices $ screen term) $ \idx@(y, x) ->
    preservingMatrix $ do
        let tc = screen term ! idx
            bgc = if isInverse tc then foregroundColor tc else backgroundColor tc
            fgc = if isInverse tc then backgroundColor tc else foregroundColor tc
        translate $ toScreenCoordinates x y

        -- Render a quad in the background color
        if ( bgc /= (defaultBackgroundColor cfg))
          then do
            setColor False bgc
            blendQuad
          else return ()
        -- Render a font in the foreground color
        setColor (isBright tc) fgc
        translate $ Vector3 0 (fontHeight'' * 0.2) (0 :: GLfloat)
        renderFont font [character tc] All

  -- Cursor
  case (optionShowCursor term) of
    True -> do
      let (y, x) = cursorPos term
      translate $ toScreenCoordinates x y
      color $ glColor $ cursorColor cfg
      blendQuad
    _ -> return ()

  swapBuffers

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

getScreenSize :: State -> IO Size
getScreenSize state = do
    fontWidth' <- get $ fontWidth state
    fontHeight' <- get $ fontHeight state
    term <- readIORef $ terminal state
    let
      rws = rows term
      cls = cols term
    -- print $ "screensize " ++ (show rws) ++ ", " ++ (show cls)
    return (Size (round (fontWidth' * realToFrac (cls))) (round (fontHeight' *  realToFrac (rws))))



runHsterm :: TerminalConfig -> IO ()
runHsterm cfg = do
    -- (pOutRead, pOutWrite) <- createPipe
    -- (pInRead, pInWrite) <- createPipe
    -- (pErrRead, pErrWrite) <- createPipe

    -- hInRead <- fdToHandle pInRead
    -- hInWrite <- fdToHandle pInWrite
    -- hOutRead <- fdToHandle pOutRead
    -- hOutWrite <- fdToHandle pOutWrite

    -- hSetBuffering stdin NoBuffering
    -- hSetBuffering stdout NoBuffering
    -- hSetBuffering stderr NoBuffering
    -- hSetBuffering hInRead NoBuffering
    -- hSetBuffering hInWrite NoBuffering
    -- hSetBuffering hOutRead NoBuffering
    -- hSetBuffering hOutWrite NoBuffering
    let environment = [
            ("TERM", "xterm"),
            ("COLUMS", "79"),
            ("ROWS", "24")]
        cmd = "script"
        cmdParams = ["-f", "/dev/null"]
        -- cmdParams = ["-c", "sh --init-file " ++ (initScriptPath cfg), "-f", "/dev/null"]
    --process <- runProcess cmd cmdParams Nothing (Just environment)
    --        (Just hInRead) (Just hOutWrite) Nothing
    --forkIO $ runTerminal (terminal state) hInWrite hOutRead
    --process <- createProcess_ "aasl"
    rawenv <- getEnvironment
    let env = override rawenv environment
    (inp,out,err,pid) <- runInteractiveProcess cmd cmdParams Nothing (Just env)
    hSetBuffering inp NoBuffering
    hSetBuffering out NoBuffering
    hSetBuffering err NoBuffering


    state <- makeState cfg inp out err

    let
      Color3 r g b = (getColor cfg) False (defaultBackgroundColor cfg)
      bg = Color4 r g b 1.0 
    initDisplay bg
    font <- createTextureFont (fontPath cfg)
    let
      fontsize = fontSize cfg
    setFontFaceSize font (fromIntegral fontsize) dpi
    currentFont state $= Just font
    (sx:sy:sz:ex:ey:ez) <- getFontBBox font "X" 
    advance <- getFontAdvance font "_"
    fontWidth state $= advance
    fontHeight state $= (fromIntegral fontsize) * (1.5 :: Float)
      
    ss <- getScreenSize state
    windowSize $= ss

    displayCallback $= displayHandler state
    idleCallback $= Just (postRedisplay Nothing)
    reshapeCallback $= Just (reshapeHandler state)

    keyboardMouseCallback $= Just (keyboardMouseHandler state)
      
    -- print $ "getEnv"
    -- putStrLn $ mp env
    -- print $ "inp" ++ show inp
    -- print $ "out" ++ show out
    -- print $ "hInRead" ++ show hInRead
    -- print $ "hInWrite" ++ show hInWrite
    -- print $ "hOutRead" ++ show hOutRead
    -- print $ "hOutWrite" ++ show hOutWrite
    -- hPutChar inp 's'
    hFlush inp
    forkIO $ runTerminal (terminal state) inp out

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

