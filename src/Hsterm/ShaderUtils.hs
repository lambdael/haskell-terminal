-- Utils for loading, compiling and linking GLSL shaders
-- Largely taken from GLUT's Brick.hs example
module Hsterm.ShaderUtils where



import Prelude hiding ( sum )
import Control.Applicative
import Control.Monad
import Data.Maybe
import Control.Exception
import Data.Foldable ( Foldable, sum )
import Data.IORef
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL.GL.Shaders.ShaderObjects

-- Make sure that GLSL is supported by the driver, either directly by the core
-- or via an extension.
checkGLSLSupport :: IO ()
checkGLSLSupport = do
   version <- get (majorMinor glVersion)
   unless (version >= (2,0)) $ do
      extensions <- get glExtensions
      unless ("GL_ARB_shading_language_100" `elem` extensions) $
         ioError (userError "No GLSL support found.")

readAndCompileShader ::  FilePath -> ShaderType -> IO Shader
readAndCompileShader filePath st = do
   src <- readFile filePath
   shader <- createShader st
   shaderSource shader $= [src]
   compileShader shader
   reportErrors
   ok <- get (compileStatus shader)
   infoLog <- get (shaderInfoLog shader)
   unless ok $ do
      mapM_ putStrLn ["Notice: Loaded shader '" ++ filePath ++ "': " ++ infoLog]
      deleteObjectNames [shader]
      ioError (userError "shader compilation failed")
   return shader

linkShaders :: [Shader] -> [Shader] -> IO (Maybe Program)
linkShaders vs fs = do
   -- [prog] <- genObjectNames 1
   mprog <- get currentProgram
   if isJust mprog then do
     let prog = fromJust mprog
     attachedShaders prog $= vs
     attachedShaders prog $= fs
     linkProgram prog
     reportErrors
     ok <- get (linkStatus prog)
     unless ok $ do
       infoLog <- get (programInfoLog prog)
       putStrLn infoLog
       deleteObjectNames [prog]
       ioError (userError "linking failed")
       return ()
     else return ()
   return mprog

readCompileAndLink :: String -> String -> IO (Maybe Program)
readCompileAndLink vspath fspath = do
  vs <- readAndCompileShader  vspath VertexShader
  fs <- readAndCompileShader fspath FragmentShader
  linkShaders [vs] [fs]

