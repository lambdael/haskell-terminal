{-# LANGUAGE RecordWildCards #-}
module Terminal.Posix.Winsize 
where


import Foreign.Storable
import Foreign.Ptr
import Foreign.C

data Winsize = Winsize { ws_row    :: CUShort
                       , ws_col    :: CUShort
                       , ws_xpixel :: CUShort
                       , ws_ypixel :: CUShort
                       }

instance Storable Winsize where
  sizeOf _ = 8
  alignment _ = 2
  peek p = do { ws_row    <- peekByteOff p 0
              ; ws_col    <- peekByteOff p 2
              ; ws_xpixel <- peekByteOff p 4
              ; ws_ypixel <- peekByteOff p 6
              ; return $ Winsize {..}
              }
  poke p Winsize {..} = do { pokeByteOff p 0 ws_row
                           ; pokeByteOff p 2 ws_col
                           ; pokeByteOff p 4 ws_xpixel
                           ; pokeByteOff p 6 ws_ypixel
                           }
