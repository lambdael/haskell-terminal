{-# LANGUAGE ScopedTypeVariables, TypeFamilies, FlexibleContexts #-}
-- | GPipe ベースのターミナルレンダラー（セルデータテクスチャ方式）。
--
-- 頂点バッファはリサイズ時のみ再生成（位置 + セルテクスチャ座標 = 固定グリッド）。
-- 各セルの文字・色情報は4枚のテクスチャに格納し、毎フレーム更新する。
-- シェーダーがテクスチャルックアップでグリフ位置・UV・色を取得するため、
-- 頂点データは画面内容に依存しない。
module Hsterm.GPipe.Renderer
  ( -- * シェーダー環境
    TextShaderEnv(..)
  , BgShaderEnv(..)
  , CursorShaderEnv(..)
    -- * シェーダーコンパイル
  , compileTextShader
  , compileBgShader
  , compileCursorShader
    -- * 固定グリッド頂点（リサイズ時のみ再生成）
  , buildBgGridVertices
  , buildTextGridVertices
    -- * セルデータ（毎フレーム更新用テクスチャデータ）
  , buildCellData
    -- * カーソル（毎フレーム）
  , buildCursorVertices
    -- * ユーティリティ
  , cellWidth
  ) where

import Data.Array (indices, (!))
import qualified Data.Map.Strict as Map

import Graphics.GPipe
import Linear (V2(..), V4(..))

import Graphics.GPipe.Font.Types (FontAtlas(..), GlyphRegion(..), GlyphMetrics(..))

import Terminal.Types

-- ── シェーダー環境 ────────────────────────────────────────

-- | テキスト描画用シェーダー環境（セルデータテクスチャ方式）。
--
-- 頂点: @(cellTopLeft, cellTexCoord, cornerLerp)@
-- 頂点シェーダーで glyphPos/glyphUV テクスチャからグリフ情報を取得し、
-- cornerLerp で頂点位置とアトラス UV を計算する。
data TextShaderEnv os = TextShaderEnv
  { tePrimitives  :: PrimitiveArray Triangles (B2 Float, B2 Float, B2 Float)
  , teFontAtlas   :: (Texture2D os (Format RFloat), SamplerFilter RFloat, (EdgeMode2, BorderColor RFloat))
  , teFgColorTex  :: (Texture2D os (Format RGBAFloat), SamplerFilter RGBAFloat, (EdgeMode2, BorderColor RGBAFloat))
  , teGlyphUVTex  :: (Texture2D os (Format RGBAFloat), SamplerFilter RGBAFloat, (EdgeMode2, BorderColor RGBAFloat))
  , teGlyphPosTex :: (Texture2D os (Format RGBAFloat), SamplerFilter RGBAFloat, (EdgeMode2, BorderColor RGBAFloat))
  }

-- | 背景色描画用シェーダー環境（セルデータテクスチャ方式）。
--
-- 頂点: @(position, cellTexCoord)@
-- フラグメントシェーダーで bgColorTex からセルの背景色を取得する。
data BgShaderEnv os = BgShaderEnv
  { bgPrimitives :: PrimitiveArray Triangles (B2 Float, B2 Float)
  , bgColorTex   :: (Texture2D os (Format RGBAFloat), SamplerFilter RGBAFloat, (EdgeMode2, BorderColor RGBAFloat))
  }

-- | カーソル描画用シェーダー環境（テクスチャなし、従来方式）。
data CursorShaderEnv os = CursorShaderEnv
  { curPrimitives :: PrimitiveArray Triangles (B2 Float, B4 Float)
  }

-- ── シェーダーコンパイル ──────────────────────────────────

-- | テキスト描画シェーダー。
--
-- 頂点シェーダー: セルデータテクスチャから glyphPos / glyphUV を読み、
-- cornerLerp で頂点位置とアトラス UV を計算する。
-- フラグメントシェーダー: fgColor テクスチャから前景色、フォントアトラスから
-- アルファ値を取得する。
compileTextShader
  :: (ContextHandler ctx)
  => Window os RGBAFloat ()
  -> V2 Int
  -> ContextT ctx os IO (CompiledShader os (TextShaderEnv os))
compileTextShader win winSize =
  compileShader $ do
    fontSamp <- newSampler2D (\s -> teFontAtlas s)
    fgSamp   <- newSampler2D (\s -> teFgColorTex s)
    uvSamp   <- newSampler2D (\s -> teGlyphUVTex s)
    posSamp  <- newSampler2D (\s -> teGlyphPosTex s)

    prims <- toPrimitiveStream tePrimitives

    let V2 sw sh = fmap fromIntegral winSize :: V2 VFloat
        projected = fmap (\(cellTL, cellUV, corner) ->
          -- セルデータテクスチャからグリフ情報を取得（頂点シェーダー内）
          let V4 ox oy gw gh = sample2D posSamp (SampleLod 0) Nothing Nothing cellUV
              V4 u0 v0 u1 v1 = sample2D uvSamp  (SampleLod 0) Nothing Nothing cellUV

              V2 lx ly = corner

              -- グリフ矩形内の実際の頂点位置
              V2 tlx tly = cellTL
              px = tlx + ox + lx * gw
              py = tly + oy + ly * gh

              -- アトラス UV
              u = u0 + lx * (u1 - u0)
              v = v0 + ly * (v1 - v0)

              -- クリップ空間へ
              cx = px * 2 / sw - 1
              cy = 1 - py * 2 / sh
          in  (V4 cx cy 0 1, (V2 u v, cellUV))
          ) prims

    frags <- rasterize
      (const ( FrontAndBack
             , ViewPort (V2 0 0) winSize
             , DepthRange 0 1
             ))
      projected

    let colored = fmap (\(uv, cellUV) ->
          let a = sample2D fontSamp SampleAuto Nothing Nothing uv
              V4 cr cg cb _ = sample2D fgSamp SampleAuto Nothing Nothing cellUV
          in  V4 cr cg cb a
          ) frags

    drawWindowColor
      (const (win, ContextColorOption (BlendRgbAlpha
        (FuncAdd, FuncAdd)
        (BlendingFactors SrcAlpha OneMinusSrcAlpha, BlendingFactors One Zero)
        (V4 0 0 0 0)) (pure True)))
      colored

-- | 背景色描画シェーダー。
--
-- フラグメントシェーダーで bgColorTex からセルの背景色を取得する。
compileBgShader
  :: (ContextHandler ctx)
  => Window os RGBAFloat ()
  -> V2 Int
  -> ContextT ctx os IO (CompiledShader os (BgShaderEnv os))
compileBgShader win winSize =
  compileShader $ do
    bgSamp <- newSampler2D (\s -> bgColorTex s)
    prims  <- toPrimitiveStream bgPrimitives

    let V2 sw sh = fmap fromIntegral winSize :: V2 VFloat
        projected = fmap (\(pos, cellUV) ->
          let V2 px py = pos
              cx = px * 2 / sw - 1
              cy = 1 - py * 2 / sh
          in  (V4 cx cy 0 1, cellUV)
          ) prims

    frags <- rasterize
      (const ( FrontAndBack
             , ViewPort (V2 0 0) winSize
             , DepthRange 0 1
             ))
      projected

    let colored = fmap (\cellUV ->
          sample2D bgSamp SampleAuto Nothing Nothing cellUV
          ) frags

    drawWindowColor
      (const (win, ContextColorOption (BlendRgbAlpha
        (FuncAdd, FuncAdd)
        (BlendingFactors SrcAlpha OneMinusSrcAlpha, BlendingFactors One Zero)
        (V4 0 0 0 0)) (pure True)))
      colored

-- | カーソル描画シェーダー（テクスチャなし、従来方式）。
compileCursorShader
  :: (ContextHandler ctx)
  => Window os RGBAFloat ()
  -> V2 Int
  -> ContextT ctx os IO (CompiledShader os (CursorShaderEnv os))
compileCursorShader win winSize =
  compileShader $ do
    prims <- toPrimitiveStream curPrimitives

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

    drawWindowColor
      (const (win, ContextColorOption (BlendRgbAlpha
        (FuncAdd, FuncAdd)
        (BlendingFactors SrcAlpha OneMinusSrcAlpha, BlendingFactors One Zero)
        (V4 0 0 0 0)) (pure True)))
      (fmap (\col -> col) frags)

-- ── ユーティリティ ────────────────────────────────────────

-- | フォントアトラスからモノスペースのセル幅を計算する。
cellWidth :: FontAtlas -> Float
cellWidth atlas = case Map.lookup ' ' (faGlyphs atlas) of
  Just gr -> fromIntegral (gmAdvanceX (grMetrics gr))
  Nothing -> fromIntegral (faLineH atlas) * 0.5

-- ── 固定グリッド頂点（リサイズ時のみ生成） ──────────────────

-- | 背景用固定グリッド頂点。
--
-- 各セルに対して 6 頂点 @(position, cellTexCoord)@ を出力する。
-- position = セル矩形の各角のピクセル座標。
-- cellTexCoord = セルデータテクスチャの対応テクセル中心（全6頂点で同一値）。
-- リサイズ時にのみ呼び出し、結果を GPU バッファに書き込む。
buildBgGridVertices
  :: FontAtlas
  -> Int    -- ^ rows
  -> Int    -- ^ cols
  -> [(V2 Float, V2 Float)]
buildBgGridVertices atlas r c =
  concatMap mkQuad [(row, col) | row <- [1..r], col <- [1..c]]
  where
    cw = cellWidth atlas
    fh = fromIntegral (faLineH atlas) :: Float
    totalCols = fromIntegral c :: Float
    totalRows = fromIntegral r :: Float

    mkQuad (row, col) =
      let x0 = fromIntegral (col - 1) * cw
          y0 = fromIntegral (row - 1) * fh
          x1 = x0 + cw
          y1 = y0 + fh
          tu = (fromIntegral col - 0.5) / totalCols
          tv = (fromIntegral row - 0.5) / totalRows
          uv = V2 tu tv
      in [ (V2 x0 y0, uv), (V2 x1 y0, uv), (V2 x1 y1, uv)
         , (V2 x0 y0, uv), (V2 x1 y1, uv), (V2 x0 y1, uv)
         ]

-- | テキスト用固定グリッド頂点。
--
-- 各セルに対して 6 頂点 @(cellTopLeft, cellTexCoord, cornerLerp)@ を出力する。
-- cellTopLeft = セル左上のピクセル座標（全6頂点で同一値）。
-- cellTexCoord = セルデータテクスチャの対応テクセル中心（全6頂点で同一値）。
-- cornerLerp = グリフ矩形内の補間パラメータ:
--   @(0,0), (1,0), (1,1), (0,0), (1,1), (0,1)@
-- リサイズ時にのみ呼び出し、結果を GPU バッファに書き込む。
buildTextGridVertices
  :: FontAtlas
  -> Int    -- ^ rows
  -> Int    -- ^ cols
  -> [(V2 Float, V2 Float, V2 Float)]
buildTextGridVertices atlas r c =
  concatMap mkQuad [(row, col) | row <- [1..r], col <- [1..c]]
  where
    cw = cellWidth atlas
    fh = fromIntegral (faLineH atlas) :: Float
    totalCols = fromIntegral c :: Float
    totalRows = fromIntegral r :: Float

    mkQuad (row, col) =
      let tl = V2 (fromIntegral (col - 1) * cw) (fromIntegral (row - 1) * fh)
          tu = (fromIntegral col - 0.5) / totalCols
          tv = (fromIntegral row - 0.5) / totalRows
          uv = V2 tu tv
      in [ (tl, uv, V2 0 0), (tl, uv, V2 1 0), (tl, uv, V2 1 1)
         , (tl, uv, V2 0 0), (tl, uv, V2 1 1), (tl, uv, V2 0 1)
         ]

-- ── セルデータ（毎フレームテクスチャに書き込む） ──────────

-- | ターミナル状態からセルデータテクスチャ用のデータを生成する。
--
-- 4本のリストを返す。各リストは @rows * cols@ 要素で、
-- 順序は @indices (screen term)@ と同一（行メジャー、(1,1) → (rows,cols)）。
-- これをそのまま @writeTexture2D@ で cols×rows テクスチャに書き込める。
--
-- * bgColor:  各セルの背景色 (RGBA)
-- * fgColor:  各セルの前景色 (RGBA)
-- * glyphUV:  各セルのアトラス UV 矩形 (u0,v0,u1,v1)
-- * glyphPos: 各セルのグリフ位置情報 (offsetX, offsetY, width, height)
buildCellData
  :: FontAtlas
  -> Terminal
  -> (Bool -> TerminalColor -> V4 Float)
  -> ( [V4 Float]   -- ^ bgColor per cell
     , [V4 Float]   -- ^ fgColor per cell
     , [V4 Float]   -- ^ glyphUV (u0,v0,u1,v1) per cell
     , [V4 Float]   -- ^ glyphPos (ox,oy,w,h) per cell
     )
buildCellData atlas term colorFn =
  unzip4 $ map cellData (indices (screen term))
  where
    asc = fromIntegral (faAscender atlas) :: Float

    cellData (y, x) =
      let tc  = screen term ! (y, x)
          bgc = if isInverse tc then foregroundColor tc else backgroundColor tc
          fgc = if isInverse tc then backgroundColor tc else foregroundColor tc
          bgCol = colorFn False bgc
          fgCol = colorFn (isBright tc) fgc
          ch = character tc
          (glyphUV, glyphPos) = glyphInfo ch
      in (bgCol, fgCol, glyphUV, glyphPos)

    glyphInfo ' ' = (V4 0 0 0 0, V4 0 0 0 0)
    glyphInfo ch  = case Map.lookup ch (faGlyphs atlas) of
      Nothing -> (V4 0 0 0 0, V4 0 0 0 0)
      Just gr ->
        let gm = grMetrics gr
        in ( V4 (grU0 gr) (grV0 gr) (grU1 gr) (grV1 gr)
           , V4 (fromIntegral (gmBearingX gm))
                (asc - fromIntegral (gmBearingY gm))
                (fromIntegral (gmWidth gm))
                (fromIntegral (gmHeight gm))
           )

    unzip4 :: [(a,b,c,d)] -> ([a],[b],[c],[d])
    unzip4 = foldr (\(a,b,c,d) ~(as,bs,cs,ds) -> (a:as,b:bs,c:cs,d:ds)) ([],[],[],[])

-- | カーソル矩形の頂点（従来方式、テクスチャなし）。
buildCursorVertices
  :: FontAtlas
  -> Terminal
  -> V4 Float
  -> [(V2 Float, V4 Float)]
buildCursorVertices atlas term cursorCol =
  if not (optionShowCursor term)
    then []
    else
      let (cy, cx) = cursorPos term
          cw = cellWidth atlas
          fh = fromIntegral (faLineH atlas) :: Float
          x0 = fromIntegral (cx - 1) * cw
          y0 = fromIntegral (cy - 1) * fh
          x1 = x0 + cw
          y1 = y0 + fh
      in [ (V2 x0 y0, cursorCol), (V2 x1 y0, cursorCol), (V2 x1 y1, cursorCol)
         , (V2 x0 y0, cursorCol), (V2 x1 y1, cursorCol), (V2 x0 y1, cursorCol)
         ]
