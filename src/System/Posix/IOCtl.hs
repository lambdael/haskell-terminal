{-# LANGUAGE MultiParamTypeClasses, ForeignFunctionInterface, FunctionalDependencies #-}
-- | Vendored from the ioctl package (broken with GHC >= 9.4)
module System.Posix.IOCtl
  ( IOControl(..)
  , ioctl
  , ioctl'
  ) where

import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Alloc (alloca)
import Foreign.C.Types
import Foreign.C.Error (throwErrnoIfMinus1_)
import System.Posix.Types (Fd(..))

class (Storable d) => IOControl req d | req -> d where
  ioctlReq :: req -> CULong

foreign import ccall "sys/ioctl.h ioctl" c_ioctl :: CInt -> CULong -> Ptr a -> IO CInt

ioctl :: (IOControl req d) => Fd -> req -> d -> IO ()
ioctl f req d = alloca $ \p -> do
  poke p d
  throwErrnoIfMinus1_ "ioctl" $
    c_ioctl (fromIntegral f) (ioctlReq req) (castPtr p)

ioctl' :: (IOControl req d) => Fd -> req -> IO d
ioctl' f req = alloca $ \p -> do
  throwErrnoIfMinus1_ "ioctl" $
    c_ioctl (fromIntegral f) (ioctlReq req) (castPtr p)
  peek p
