{-# LANGUAGE TemplateHaskell #-}
-- | Provides functionality of rendering the application model.
module Hsterm.Renderer
where

import Graphics.Rendering.OpenGL


type Render = RenderData -> IO () -- render function
type Terminate = IO () -- terminate function 
type Renderer = IO (Render, Terminate) -- init gl and return render and terminate functions

nullRenderer :: Renderer
nullRenderer = return ((\d -> return ()), return ())

data RenderData = RenderData {
  mat :: GLmatrix GLfloat,
  time :: GLfloat
                             }
