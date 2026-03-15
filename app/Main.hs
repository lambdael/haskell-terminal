module Main where

import Hsterm.GPipe.Config (defaultConfig)
import Hsterm.GPipe.Dyre (hsterm)
import Hsterm.GPipe.Main (runGPipeTerminal)

main :: IO ()
main = hsterm runGPipeTerminal defaultConfig
