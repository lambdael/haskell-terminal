module Hsterm.Renderer.Utils
    where


import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL
import System.IO
import qualified Hsterm.LoadShaders as LS
import Hsterm.Renderer

setShaderVal :: (Uniform a)=> Program -> String -> a -> IO ()
setShaderVal program name val = do
  loc <- get (uniformLocation program name) 
  let valVar = uniform loc
  valVar $= val
  return ()


-- | Checks OpenGL errors, and Writes to stderr when errors occur.
checkError
    :: String -- ^ a function name that called this
    -> IO ()
checkError functionName = get errors >>= mapM_ reportError
  where
    reportError (Error category message) = do
        hPutStrLn stderr $ (show category) ++ " in " ++ functionName ++ ": " ++ message


-- | Converts an offset value to the Ptr value.
bufferOffset :: Integral a
    => a -- ^ an offset value
    -> Ptr b -- ^ the Ptr value
bufferOffset = plusPtr nullPtr . fromIntegral


-- | The byte size of a memory area that is converted from a list.
arrayByteSize :: (Storable a)
    => [a] -- ^ a list
    -> Int
arrayByteSize ls = (sizeOf (head ls)) * (length ls)

-- | Initializes a buffer object.
initializeBuffer :: (Storable a) => BufferTarget -> [a] -> IO BufferObject
initializeBuffer t array = do
    buffer <- genObjectName
    bindBuffer t $= Just buffer

    withArray array $ \ptr -> do
        bufferData t $= (fromIntegral $ arrayByteSize array, ptr, StaticDraw)

    bindBuffer ElementArrayBuffer $= Nothing
    return buffer


