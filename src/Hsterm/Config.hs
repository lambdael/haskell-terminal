module Hsterm.Config where
import Data.Colour.SRGB (sRGB)
import Data.Colour (Colour)
import Terminal.Types (TerminalColor(..))
import Hsterm.Renderer
import Hsterm.Renderer.DefaultRenderer
import Hsterm.Renderer.FlatColor
import qualified Hsterm.Renderer.CursorRenderer as CR
import qualified Hsterm.Renderer.Background as BR
import Graphics.Rendering.OpenGL.GL.Shaders.ProgramObjects 
import Graphics.Rendering.OpenGL (Color4, GLfloat)
import Data.Colour 
-- These get overwritten with nicer colors in Main.hs
defaultColor Black   = sRGB 0 0 0
defaultColor Green   = sRGB 0 1 0
defaultColor Yellow  = sRGB 0 1 1
defaultColor Blue    = sRGB 0 0 1
defaultColor Magenta = sRGB 1 1 0
defaultColor Cyan    = sRGB 1 0 1
defaultColor White   = sRGB 1 1 1


data   TerminalConfig  = TerminalConfig {
    defaultForegroundColor :: TerminalColor,
    defaultBackgroundColor :: TerminalColor,
    cursorColor :: Colour Double,
    colorMap :: TerminalColor -> Colour Double,
    colorMapBright :: TerminalColor -> Colour Double,
    fontPath :: FilePath,
    initScriptPath :: FilePath,
    fontSize :: Integer,
    backgroundRenderer :: Renderer,
    cursorRenderer :: Renderer
}

defaultTerminalConfig :: TerminalConfig 
defaultTerminalConfig = TerminalConfig {
    defaultForegroundColor = White,
    defaultBackgroundColor = Black,
    cursorColor = defaultColor White,
    fontPath = "data/fonts/monofur/monof55.ttf",
    colorMap = defaultColor,
    colorMapBright = defaultColor,
    initScriptPath = "data/init.sh",
    fontSize = 20,
    backgroundRenderer = BR.background,
    -- cursorRenderer = defaultRenderer
    cursorRenderer = CR.cursorRenderer $ (sRGB (1.0::GLfloat) (0.2::GLfloat) (0.9::GLfloat)) `withOpacity` (1.0::GLfloat) 
}


