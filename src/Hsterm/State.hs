module Hsterm.State where
import Data.IORef
import Graphics.Rendering.OpenGL hiding (Bool, Float)
import Graphics.Rendering.OpenGL.GLU (perspective)


import Graphics.GLU
-- import Graphics.Rendering.GLU.Raw
import Graphics.Rendering.OpenGL.GL.FramebufferObjects
-- #import Graphics.GL.
-- #import Graphics.Rendering.OpenGL.Raw.ARB.Compatibility (glPushMatrix, glPopMatrix)
import Graphics.UI.GLUT hiding (Bool, Float, Font, fontHeight, fontWidth)
import Graphics.Rendering.FTGL (Font)

import Data.Time.Clock (getCurrentTime)
import qualified Data.Time.Clock as C

import Terminal.Terminal
import Terminal.Types
import Hsterm.Config
import qualified System.Console.Terminfo as TI

import System.IO

data State = State {
    terminal :: IORef Terminal,
    startupTime :: IORef C.UTCTime,
    lastKeystrokeTime :: IORef C.UTCTime,
    backgroundProgram :: IORef (Maybe Program),
    foregroundProgram :: IORef (Maybe Program),
    cursorProgram :: IORef (Maybe Program),
    currentFont :: IORef (Maybe Font),
    fontWidth :: IORef Float,
    fontHeight :: IORef Float,
    inp :: IORef Handle,
    out :: IORef Handle,
    err :: IORef Handle,
    config :: IORef TerminalConfig
}

makeState :: TerminalConfig -> Handle -> Handle -> Handle -> IO State
makeState cfg inp'' out'' err'' = do
    terminfo' <- TI.setupTermFromEnv
    terminal' <- newIORef $ defaultTerm terminfo'

    startupTime' <- getCurrentTime >>= newIORef
    lastKeystrokeTime' <- getCurrentTime >>= newIORef

    backgroundProgram' <- newIORef Nothing
    foregroundProgram' <- newIORef Nothing
    cursorProgram' <- newIORef Nothing
    currentFont' <- newIORef Nothing
    fontWidth' <- newIORef 0.0
    fontHeight' <- newIORef 0.0
    config' <- newIORef cfg
    
    inp' <- newIORef inp''
    out' <- newIORef out''
    err' <- newIORef err''

    return State {
        terminal = terminal',

        startupTime = startupTime',
        lastKeystrokeTime = lastKeystrokeTime',

        backgroundProgram = backgroundProgram',
        foregroundProgram = foregroundProgram',
        cursorProgram = cursorProgram',
        currentFont = currentFont',
        fontHeight = fontHeight',
        fontWidth = fontWidth',
        config = config',
        inp = inp',
        out = out',
        err = err'
    }
