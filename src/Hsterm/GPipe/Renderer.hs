{-# LANGUAGE ScopedTypeVariables, TypeFamilies, FlexibleContexts #-}
-- | GPipe ベースのターミナルレンダラー。
--
-- ターミナル状態を読み取り、gpipe-freetype の FontAtlas を使って
-- テキストと背景色の頂点データを生成し、GPipe シェーダーで描画する。
module Hsterm.GPipe.Renderer
  ( -- * シェーダー環境
    TextShaderEnv(..)
  , BgShaderEnv(..)
    -- * シェーダーコンパイル
  , compileTextShader
  , compileBgShader
    -- * 頂点生成
  , buildTextVertices
  , buildBgVertices
  , buildCursorVertices
  ) where

import Data.Array (indices, (!))
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import Data.Colour.SRGB (RGB(..), toSRGB)
import Data.Colour (Colour)

import Graphics.GPipe
import Linear (V2(..), V4(..))

import Graphics.GPipe.Font (FontAtlas(..), TextVertex(..), layoutText)
import Graphics.GPipe.Font.Types (GlyphRegion(..), GlyphMetrics(..))

import Terminal.Types

-- ── シェーダー環境 ────────────────────────────────────────

-- | テキスト描画用のシェーダー環境。
data TextShaderEnv os = TextShaderEnv
  { tePrimitives :: PrimitiveArray Triangles (B2 Float, B2 Float, B4 Float)
  , teTexture    :: (Texture2D os (Format RFloat), SamplerFilter RFloat, (EdgeMode2, BorderColor RFloat))
  }

-- | 背景色・カーソル描画用のシェーダー環境。
data BgShaderEnv os = BgShaderEnv
  { bgPrimitives :: PrimitiveArray Triangles (B2 Float, B4 Float)
  }

-- ── シェーダーコンパイル ──────────────────────────────────

-- | テキスト描画シェーダーをコンパイルする。
--
-- 各頂点は (position, uv, color) を持つ。
-- フラグメントシェーダーでアトラステクスチャをサンプリングし、
-- アルファ値として使用する。
compileTextShader
  :: (ContextHandler ctx)
  => Window os RGBAFloat ()
  -> V2 Int    -- ^ ウィンドウサイズ
  -> ContextT ctx os IO (CompiledShader os (TextShaderEnv os))
compileTextShader win winSize =
  compileShader $ do
    samp <- newSampler2D (\s -> teTexture s)
    prims <- toPrimitiveStream tePrimitives

    let V2 sw sh = fmap fromIntegral winSize :: V2 VFloat
        projected = fmap (\(pos, uv, col) ->
          let V2 px py = pos
              cx = px * 2 / sw - 1
              cy = 1 - py * 2 / sh
          in  (V4 cx cy 0 1, (uv, col))
          ) prims

    frags <- rasterize
      (const ( FrontAndBack
             , ViewPort (V2 0 0) winSize
             , DepthRange 0 1
             ))
      projected

    let colored = fmap (\(uv, col) ->
          let a = sample2D samp SampleAuto Nothing Nothing uv
              V4 cr cg cb _ = col
          in  V4 cr cg cb a
          ) frags

    drawWindowColor
      (const (win, ContextColorOption (BlendRgbAlpha
        (FuncAdd, FuncAdd)
        (BlendingFactors SrcAlpha OneMinusSrcAlpha, BlendingFactors One Zero)
        (V4 0 0 0 0)) (pure True)))
      colored

-- | 背景色/カーソル描画シェーダーをコンパイルする。
--
-- 各頂点は (position, color)。テクスチャなし。
compileBgShader
  :: (ContextHandler ctx)
  => Window os RGBAFloat ()
  -> V2 Int    -- ^ ウィンドウサイズ
  -> ContextT ctx os IO (CompiledShader os (BgShaderEnv os))
compileBgShader win winSize =
  compileShader $ do
    prims <- toPrimitiveStream bgPrimitives

    let V2 sw sh = fmap fromIntegral winSize :: V2 VFloat
        projected = fmap (\(pos, col) ->
          let V2 px py = pos
              cx = px * 2 / sw - 1
              cy = 1 - py * 2 / sh
          in  (V4 cx cy 0 1, col)
          ) prims

    frags <- rasterize
      (const ( FrontAndBack
             , ViewPort (V2 0 0) winSize
             , DepthRange 0 1
             ))
      projected

    let colored = fmap (\col -> col) frags

    drawWindowColor
      (const (win, ContextColorOption (BlendRgbAlpha
        (FuncAdd, FuncAdd)
        (BlendingFactors SrcAlpha OneMinusSrcAlpha, BlendingFactors One Zero)
        (V4 0 0 0 0)) (pure True)))
      colored

-- ── 頂点生成 ──────────────────────────────────────────────

-- | ターミナル状態からテキスト頂点を生成する。
--
-- 各セルの文字を FontAtlas で検索し、
-- 位置・UV・前景色を含む頂点リストを返す。
buildTextVertices
  :: FontAtlas
  -> Terminal
  -> (Bool -> TerminalColor -> V4 Float)  -- ^ 色変換関数
  -> TerminalColor                         -- ^ デフォルト前景色
  -> [(V2 Float, V2 Float, V4 Float)]
buildTextVertices atlas term colorFn defFg =
  concatMap cellVerts (indices (screen term))
  where
    fh = fromIntegral (faLineH atlas) :: Float
    ascender = fromIntegral (faAscender atlas) :: Float

    cellVerts (y, x) =
      let tc   = screen term ! (y, x)
          ch   = character tc
          fgc  = if isInverse tc then backgroundColor tc else foregroundColor tc
          bright = isBright tc
          col  = colorFn bright fgc
          -- セルの左上ピクセル座標
          penX = fromIntegral (x - 1) * cellW
          penY = fromIntegral (y - 1) * fh
      in case Map.lookup ch (faGlyphs atlas) of
           Nothing -> []    -- グリフがアトラスにない → スキップ
           Just gr ->
             let gm  = grMetrics gr
                 x0  = penX + fromIntegral (gmBearingX gm)
                 y0  = penY + (ascender - fromIntegral (gmBearingY gm))
                 x1  = x0 + fromIntegral (gmWidth gm)
                 y1  = y0 + fromIntegral (gmHeight gm)
                 u0  = grU0 gr; v0 = grV0 gr
                 u1  = grU1 gr; v1 = grV1 gr
             in  [ (V2 x0 y0, V2 u0 v0, col)
                 , (V2 x1 y0, V2 u1 v0, col)
                 , (V2 x1 y1, V2 u1 v1, col)
                 , (V2 x0 y0, V2 u0 v0, col)
                 , (V2 x1 y1, V2 u1 v1, col)
                 , (V2 x0 y1, V2 u0 v1, col)
                 ]

    -- セル幅: アドバンスの中央値を使用、またはスペースのアドバンスを使用
    cellW :: Float
    cellW = case Map.lookup ' ' (faGlyphs atlas) of
      Just gr -> fromIntegral (gmAdvanceX (grMetrics gr))
      Nothing -> fromIntegral (faLineH atlas) * 0.5

-- | 背景色の矩形頂点を生成する。
--
-- デフォルト背景色以外のセルにのみ背景矩形を描画する。
buildBgVertices
  :: FontAtlas
  -> Terminal
  -> (Bool -> TerminalColor -> V4 Float)  -- ^ 色変換関数
  -> TerminalColor                         -- ^ デフォルト背景色
  -> [(V2 Float, V4 Float)]
buildBgVertices atlas term colorFn defBg =
  concatMap cellBg (indices (screen term))
  where
    fh = fromIntegral (faLineH atlas) :: Float

    cellBg (y, x) =
      let tc  = screen term ! (y, x)
          bgc = if isInverse tc then foregroundColor tc else backgroundColor tc
      in if bgc == defBg
           then []
           else
             let col = colorFn False bgc
                 x0 = fromIntegral (x - 1) * cellW
                 y0 = fromIntegral (y - 1) * fh
                 x1 = x0 + cellW
                 y1 = y0 + fh
             in [ (V2 x0 y0, col)
                , (V2 x1 y0, col)
                , (V2 x1 y1, col)
                , (V2 x0 y0, col)
                , (V2 x1 y1, col)
                , (V2 x0 y1, col)
                ]

    cellW :: Float
    cellW = case Map.lookup ' ' (faGlyphs atlas) of
      Just gr -> fromIntegral (gmAdvanceX (grMetrics gr))
      Nothing -> fromIntegral (faLineH atlas) * 0.5

-- | カーソル矩形の頂点を生成する。
buildCursorVertices
  :: FontAtlas
  -> Terminal
  -> V4 Float     -- ^ カーソル色
  -> [(V2 Float, V4 Float)]
buildCursorVertices atlas term cursorCol =
  if not (optionShowCursor term)
    then []
    else
      let (cy, cx) = cursorPos term
          fh = fromIntegral (faLineH atlas) :: Float
          x0 = fromIntegral (cx - 1) * cellW
          y0 = fromIntegral (cy - 1) * fh
          x1 = x0 + cellW
          y1 = y0 + fh
      in [ (V2 x0 y0, cursorCol)
         , (V2 x1 y0, cursorCol)
         , (V2 x1 y1, cursorCol)
         , (V2 x0 y0, cursorCol)
         , (V2 x1 y1, cursorCol)
         , (V2 x0 y1, cursorCol)
         ]
  where
    cellW :: Float
    cellW = case Map.lookup ' ' (faGlyphs atlas) of
      Just gr -> fromIntegral (gmAdvanceX (grMetrics gr))
      Nothing -> fromIntegral (faLineH atlas) * 0.5
