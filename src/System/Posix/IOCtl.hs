{-# LANGUAGE MultiParamTypeClasses, ForeignFunctionInterface, FunctionalDependencies #-}
-- | POSIX ioctl(2) の型安全な Haskell ラッパー。
--
-- 壊れた @ioctl@ Hackage パッケージ（GHC >= 9.4 でビルド不可）の
-- 代替としてプロジェクト内に vendor したもの。
-- 'IOControl' 型クラスでリクエストコードとデータ型を結び付ける。
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
