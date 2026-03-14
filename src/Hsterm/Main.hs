module Main where
import Hsterm.Hsterm (runHsterm)
import Hsterm.Config (defaultTerminalConfig, TerminalConfig(..))
import Data.Colour.SRGB
import Terminal.Types (TerminalColor(..))
import Graphics.Rendering.OpenGL.GL.Shaders.ShaderObjects
import System.Process (readProcess)
import Control.Exception (try, SomeException(..))

import qualified Hsterm.LoadShaders as LS

-- | fontconfig を使ってモノスペースフォントのパスを取得する
findMonospaceFont :: IO FilePath
findMonospaceFont = do
    result <- try $ readProcess "fc-match" ["-f", "%{file}", "monospace:style=Regular"] ""
    case (result :: Either SomeException String) of
        Right path -> return $ head $ lines path
        Left _     -> error "fc-match failed: no monospace font found"
myConfig = defaultTerminalConfig {
    colorMap = \c -> sRGB24read $ case c of
        Black   -> "#330000"
        Red     -> "#ed0163"
        Green   -> "#61bb00"
        Yellow  -> "#ecb900"
        Blue    -> "#1c5dbd"
        Magenta -> "#ff8800"
        Cyan    -> "#3bcce5"
        White   -> "#cccccc"
        _       -> "#cccccc"
    ,colorMapBright = \c -> sRGB24read $ case c of
        Black   -> "#555753"
        Red     -> "#ff8d8d"
        Green   -> "#c8e7a8"
        Yellow  -> "#ffc123"
        Blue    -> "#3465a4"
        Magenta -> "#f57900"
        Cyan    -> "#46a4ff"
        White   -> "#ffffff"
        _       -> "#ffffff"
    ,fontSize = 24
    }

shaderPath = "themes/default/"
main = do
    font <- findMonospaceFont
    putStrLn $ "Using font: " ++ font
    runHsterm myConfig { fontPath = font }
