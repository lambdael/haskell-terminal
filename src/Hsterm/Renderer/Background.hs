{-# LANGUAGE TemplateHaskell #-}
-- | Provides functionality of rendering the application model.
module Hsterm.Renderer.Background
    ( background
    ) where


import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL hiding(RGB)
import System.IO
import qualified Hsterm.LoadShaders as LS
import Hsterm.Renderer
import Hsterm.Renderer.Utils
import Hsterm.Renderer.DefaultRenderer
import Data.Colour
import Data.Colour.SRGB
import qualified Hsterm.LoadShaders as LS

background ::    Renderer
background  = do
  r <- initialize 
  return (render r, terminate r)
initialize = do

    -- meshes
    let vertices =
            -- vertex attribute format : x, y, z, r, g, b, a
            [
              (-1.0), (-1.0), 0.0, 0.0, 0.0, 0.0, 1.0
            ,    1.0, (-1.0), 0.0, 0.0, 0.3, 0.0, 1.0
            ,    1.0,    1.0, 0.0, 0.6, 0.0, 0.6, 1.0
            , (-1.0 ),   1.0, 0.0, 0.6, 0.0, 0.6, 1.0
            ] :: [GLfloat]
        numPositionElements = 3
        numColorElements = 4
        offsetPosition = 0
        offsetColor = offsetPosition + numPositionElements
        sizeElement = sizeOf (head vertices)
        sizeVertex = fromIntegral (sizeElement * (numPositionElements + numColorElements))

    let indices =
            [
              0, 1, 2
            , 2, 3, 0
            ] :: [GLushort]


    vertexBuffer <- initializeBuffer ArrayBuffer vertices


    attributes <- genObjectName
    bindVertexArrayObject $= Just attributes
    bindBuffer ArrayBuffer $= Just vertexBuffer

    let vPosition = AttribLocation 0
        vColor = AttribLocation 1

    vertexAttribPointer vPosition $=
        (ToFloat, VertexArrayDescriptor (fromIntegral numPositionElements) Float sizeVertex (bufferOffset (offsetPosition * sizeElement)))
    vertexAttribPointer vColor $=
        (ToFloat, VertexArrayDescriptor (fromIntegral numColorElements) Float sizeVertex (bufferOffset (offsetColor * sizeElement)))

    vertexAttribArray vPosition $= Enabled
    vertexAttribArray vColor $= Enabled

    bindBuffer ArrayBuffer $= Nothing
    bindVertexArrayObject $= Nothing


    indexBuffer <- initializeBuffer ElementArrayBuffer indices

    let shaderPath = "themes/default/"
    
    program <- LS.loadShaders
        [ LS.ShaderInfo VertexShader (LS.FileSource $ shaderPath ++ "colored.vert")
        , LS.ShaderInfo FragmentShader (LS.FileSource $ shaderPath ++ "simple.frag")
        ]
    currentProgram $= Just program


    checkError "initialize"

    return $ DefaultRenderer vertexBuffer attributes indexBuffer program 0 (fromIntegral $ length indices)

