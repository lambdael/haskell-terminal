module Main where
import Hsterm.Hsterm (runHsterm)
import Hsterm.Config (defaultTerminalConfig, TerminalConfig(..))
import Data.Colour.SRGB
import Terminal.Types (TerminalColor(..))

myConfig = defaultTerminalConfig {
    colorMap = \c -> sRGB24read $ case c of
        Black   -> "#330000"
        Red     -> "#ff6565"
        Green   -> "#93d44f"
        Yellow  -> "#eab93d"
        Blue    -> "#204a87"
        Magenta -> "#ce5c00"
        Cyan    -> "#89b6e2"
        White   -> "#cccccc"
    ,colorMapBright = \c -> sRGB24read $ case c of
        Black   -> "#555753"
        Red     -> "#ff8d8d"
        Green   -> "#c8e7a8"
        Yellow  -> "#ffc123"
        Blue    -> "#3465a4"
        Magenta -> "#f57900"
        Cyan    -> "#46a4ff"
        White   -> "#ffffff"
    ,fontPath = "/home/lambdael/.local/share/fonts/Hack-Regular.ttf"
    ,fontSize = 30
}

main = runHsterm myConfig
