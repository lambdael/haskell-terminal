{-# LANGUAGE MultiParamTypeClasses #-}
module Terminal.Posix.TIOCGWINSZ where
import System.Posix.IOCtl
import Terminal.Posix.Winsize

data TIOCGWINSZ = TIOCGWINSZ
instance IOControl TIOCGWINSZ Winsize where
  ioctlReq _ = 0x00005413

