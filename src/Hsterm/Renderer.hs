{-# LANGUAGE TemplateHaskell #-}
-- | レンダラーインターフェース。
--
-- 背景やカーソルの描画をプラグイン可能にするための
-- 抽象型を定義する。各レンダラーは @IO (Render, Terminate)@
-- の形式で、初期化時に描画コールバックとクリーンアップコールバックを返す。
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
