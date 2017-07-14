module Hsterm.State where
import Data.IORef
import Data.Maybe
import Graphics.Rendering.OpenGL hiding (Bool, Float, renderer)
import Graphics.Rendering.OpenGL.GLU (perspective)
import qualified GHC.IO.Handle.FD as FD
import System.IO
import Graphics.GLU
-- import Graphics.Rendering.GLU.Raw
import Graphics.Rendering.OpenGL.GL.FramebufferObjects
-- #import Graphics.GL.
-- #import Graphics.Rendering.OpenGL.Raw.ARB.Compatibility (glPushMatrix, glPopMatrix)
import Graphics.UI.GLUT hiding (Bool, Float, Font, fontHeight, fontWidth, renderer, cursor)
import Graphics.Rendering.FTGL (Font)

import Data.Time.Clock (getCurrentTime)
import qualified Data.Time.Clock as C

import Terminal.Terminal
import Terminal.Types
import qualified Hsterm.Config as Conf
import qualified System.Console.Terminfo as TI

import System.IO

import qualified Hsterm.Renderer as R
import System.Posix.Types (CPid, Fd)
import Graphics.Rendering.OpenGL.GL.Shaders.ProgramObjects 
import Graphics.Rendering.FTGL (createTextureFont, renderFont, setFontFaceSize, RenderMode(..), getFontAdvance, getFontBBox)


data State  = State {
    terminal :: IORef Terminal,
    lastKeystrokeTime :: IORef C.UTCTime,
    currentFont :: IORef (Maybe Font),
    
    startupTime :: IORef C.UTCTime,
    fontSizeF :: IORef (Float, Float),
    -- should i make these Handles IORef?
    inp :: IORef Handle,
    out :: IORef Handle,
    err :: IORef Handle,
    
    bg :: R.Render,
    cursor :: R.Render, 
    terminators ::  [R.Terminate],
    pid :: CPid,
    pseudoterminal :: Fd,
    config :: Conf.TerminalConfig

}

makeState :: Conf.TerminalConfig -> Handle -> Handle -> Handle -> CPid -> Fd -> IO (State )
makeState cfg inp'' out'' err'' pid' pterm = do
    let terminators' = []
    terminfo' <- TI.setupTermFromEnv
    terminal' <- newIORef $ defaultTerm terminfo'

    startupTime' <- getCurrentTime >>= newIORef
    lastKeystrokeTime' <- getCurrentTime >>= newIORef
  
    -- currentFont' <- newIORef Nothing
    -- fontSize' <- newIORef (0.0,0.0)
    config' <- newIORef cfg

    
    (bgr,bgt) <- Conf.backgroundRenderer cfg
    bg' <- newIORef bgr 


    (cur,cut) <- Conf.cursorRenderer cfg
    cu' <- newIORef cur 


    hSetBuffering inp'' NoBuffering
    hSetBuffering out'' $ BlockBuffering $ Just 10
    hSetBuffering err'' $ BlockBuffering $ Just 10
    inp' <- newIORef inp''
    out' <- newIORef out''
    err' <- newIORef err''

    font <- createTextureFont (Conf.fontPath cfg)
    let
      fontsize = Conf.fontSize cfg
    setFontFaceSize font (fromIntegral fontsize) 72
    currentFont' <- newIORef $ Just font
    (sx:sy:sz:ex:ey:ez) <- getFontBBox font "X" 
    advance <- getFontAdvance font "_"
    -- fs@(fh,fw) <- get $ fontSizeF state
    fontSize' <- newIORef ((fromIntegral fontsize) * (1.5 :: Float), advance)
    -- fontHeight state $= (fromIntegral fontsize) * (1.5 :: Float)

  
    return State {
        terminal = terminal',

        startupTime = startupTime',
        lastKeystrokeTime = lastKeystrokeTime',

        currentFont = currentFont',
        fontSizeF = fontSize',
        config = cfg,
        inp = inp',
        out = out',
        err = err',
        bg = bgr,
        pid = pid',
        pseudoterminal = pterm,
        cursor = cur
        }
