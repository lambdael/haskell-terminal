{-# LANGUAGE MultiParamTypeClasses #-}
module Terminal.Posix.TIOCSWINSZ where
import System.Posix.IOCtl
import Terminal.Posix.Winsize

data TIOCSWINSZ = TIOCSWINSZ
instance IOControl TIOCSWINSZ Winsize where
  ioctlReq _ = 0x00005414

