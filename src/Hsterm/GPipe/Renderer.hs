{-# LANGUAGE ScopedTypeVariables, TypeFamilies, FlexibleContexts #-}
-- | GPipe ベースのターミナルレンダラー（セルデータテクスチャ方式）。
--
-- 頂点バッファはリサイズ時のみ再生成（位置 + セルテクスチャ座標 = 固定グリッド）。
-- 各セルの文字・色情報は4枚のテクスチャに格納し、毎フレーム更新する。
-- シェーダーがテクスチャルックアップでグリフ位置・UV・色を取得するため、
-- 頂点データは画面内容に依存しない。
--
-- ユーザは 'ShaderConfig' でレイヤーシェーダを、
-- 'ColorScheme' で色スロット単位のシェーダをカスタマイズできる。
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

import Prelude hiding ((<*))

import Data.Array (indices, (!), bounds)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq

import Graphics.GPipe
import Linear (V2(..), V4(..))

import Graphics.GPipe.Font.Types (FontAtlas(..), GlyphRegion(..), GlyphMetrics(..))

import Terminal.Types
import Hsterm.GPipe.Shader

-- ── シェーダー環境 ────────────────────────────────────────

-- | テキスト描画用シェーダー環境（セルデータテクスチャ方式）。
data TextShaderEnv os = TextShaderEnv
  { tePrimitives  :: PrimitiveArray Triangles (B2 Float, B2 Float, B2 Float)
  , teFontAtlas   :: (Texture2D os (Format RFloat), SamplerFilter RFloat, (EdgeMode2, BorderColor RFloat))
  , teFgColorTex  :: (Texture2D os (Format RGBAFloat), SamplerFilter RGBAFloat, (EdgeMode2, BorderColor RGBAFloat))
  , teGlyphUVTex  :: (Texture2D os (Format RGBAFloat), SamplerFilter RGBAFloat, (EdgeMode2, BorderColor RGBAFloat))
  , teGlyphPosTex :: (Texture2D os (Format RGBAFloat), SamplerFilter RGBAFloat, (EdgeMode2, BorderColor RGBAFloat))
  , teTimeBuf     :: (Buffer os (Uniform (B Float)), Int)
  , teMouseBuf    :: (Buffer os (Uniform (B2 Float)), Int)
  }

-- | 背景色描画用シェーダー環境（セルデータテクスチャ方式）。
data BgShaderEnv os = BgShaderEnv
  { bgPrimitives :: PrimitiveArray Triangles (B2 Float, B2 Float)
  , bgColorTex   :: (Texture2D os (Format RGBAFloat), SamplerFilter RGBAFloat, (EdgeMode2, BorderColor RGBAFloat))
  , bgTimeBuf    :: (Buffer os (Uniform (B Float)), Int)
  , bgMouseBuf   :: (Buffer os (Uniform (B2 Float)), Int)
  }

-- | カーソル描画用シェーダー環境。
data CursorShaderEnv os = CursorShaderEnv
  { curPrimitives :: PrimitiveArray Triangles (B2 Float, B4 Float, B2 Float)
  , curTimeBuf    :: (Buffer os (Uniform (B Float)), Int)
  , curMouseBuf   :: (Buffer os (Uniform (B2 Float)), Int)
  }

-- ── シェーダーコンパイル ──────────────────────────────────

-- | テキスト描画シェーダー。
compileTextShader
  :: (ContextHandler ctx)
  => Window os RGBAFloat ()
  -> V2 Int             -- ^ ウィンドウサイズ
  -> ShaderConfig       -- ^ ユーザシェーダ設定
  -> ColorScheme        -- ^ カラースキーム（ColorShader ディスパッチ用）
  -> Float              -- ^ cellWidth
  -> Float              -- ^ cellHeight
  -> Int                -- ^ cols
  -> Int                -- ^ rows
  -> ContextT ctx os IO (CompiledShader os (TextShaderEnv os))
compileTextShader win winSize shaderCfg colorScheme cellW cellH numCols numRows =
  compileShader $ do
    time  <- getUniform teTimeBuf
    mouse <- getUniform teMouseBuf

    fontSamp <- newSampler2D (\s -> teFontAtlas s)
    fgSamp   <- newSampler2D (\s -> teFgColorTex s)
    uvSamp   <- newSampler2D (\s -> teGlyphUVTex s)
    posSamp  <- newSampler2D (\s -> teGlyphPosTex s)

    prims <- toPrimitiveStream tePrimitives

    let V2 sw sh = fmap fromIntegral winSize :: V2 VFloat
        projected = fmap (\(cellTL, cellUV, corner) ->
          let V4 ox oy gw gh = sample2D posSamp (SampleLod 0) Nothing Nothing cellUV
              V4 u0 v0 u1 v1 = sample2D uvSamp  (SampleLod 0) Nothing Nothing cellUV
              V2 lx ly = corner
              V2 tlx tly = cellTL
              px = tlx + ox + lx * gw
              py = tly + oy + ly * gh
              u = u0 + lx * (u1 - u0)
              v = v0 + ly * (v1 - v0)
              cx = px * 2 / sw - 1
              cy = 1 - py * 2 / sh
          in  (V4 cx cy 0 1, (V2 u v, cellUV, V2 px py))
          ) prims

    frags <- rasterize
      (const ( FrontAndBack
             , ViewPort (V2 0 0) winSize
             , DepthRange 0 1
             ))
      projected

    let colorShaderSlots = collectColorShaderSlots colorScheme
        globals = ShaderGlobals
          { sgTime       = time
          , sgResolution = fmap fromIntegral winSize
          , sgMouse      = mouse
          , sgGridSize   = V2 (fromIntegral numCols) (fromIntegral numRows)
          , sgCellSize   = V2 (realToFrac cellW) (realToFrac cellH)
          }
        colored = fmap (\(uv, cellUV, pixelPos) ->
          let glyphAlpha = sample2D fontSamp SampleAuto Nothing Nothing uv
              rawFg = sample2D fgSamp SampleAuto Nothing Nothing cellUV
              V4 _fr _fg _fb fa = rawFg
              -- ColorShader ディスパッチ: alpha < 0 ならスロットインデックス
              V2 cw ch = sgCellSize globals
              V2 px py = pixelPos
              cellPos = V2 (floor' (px / cw)) (floor' (py / ch))
              cellUV' = V2 (fract' (px / cw)) (fract' (py / ch))
              fgColor = ifThenElse' (fa >=* 0)
                rawFg
                (buildColorShaderDispatch colorShaderSlots globals cellPos cellUV' _fr)
          in  scTextShader shaderCfg globals pixelPos cellPos fgColor glyphAlpha
          ) frags

    drawWindowColor
      (const (win, ContextColorOption (BlendRgbAlpha
        (FuncAdd, FuncAdd)
        (BlendingFactors SrcAlpha OneMinusSrcAlpha, BlendingFactors One Zero)
        (V4 0 0 0 0)) (pure True)))
      colored

-- | 背景色描画シェーダー。
compileBgShader
  :: (ContextHandler ctx)
  => Window os RGBAFloat ()
  -> V2 Int             -- ^ ウィンドウサイズ
  -> ShaderConfig       -- ^ ユーザシェーダ設定
  -> ColorScheme        -- ^ カラースキーム
  -> Float              -- ^ cellWidth
  -> Float              -- ^ cellHeight
  -> Int                -- ^ cols
  -> Int                -- ^ rows
  -> ContextT ctx os IO (CompiledShader os (BgShaderEnv os))
compileBgShader win winSize shaderCfg colorScheme cellW cellH numCols numRows =
  compileShader $ do
    time  <- getUniform bgTimeBuf
    mouse <- getUniform bgMouseBuf

    bgSamp <- newSampler2D (\s -> bgColorTex s)
    prims  <- toPrimitiveStream bgPrimitives

    let V2 sw sh = fmap fromIntegral winSize :: V2 VFloat
        projected = fmap (\(pos, cellUV) ->
          let V2 px py = pos
              cx = px * 2 / sw - 1
              cy = 1 - py * 2 / sh
          in  (V4 cx cy 0 1, (cellUV, pos))
          ) prims

    frags <- rasterize
      (const ( FrontAndBack
             , ViewPort (V2 0 0) winSize
             , DepthRange 0 1
             ))
      projected

    let colorShaderSlots = collectColorShaderSlots colorScheme
        globals = ShaderGlobals
          { sgTime       = time
          , sgResolution = fmap fromIntegral winSize
          , sgMouse      = mouse
          , sgGridSize   = V2 (fromIntegral numCols) (fromIntegral numRows)
          , sgCellSize   = V2 (realToFrac cellW) (realToFrac cellH)
          }
        colored = fmap (\(cellUV, pixelPos) ->
          let rawColor = sample2D bgSamp SampleAuto Nothing Nothing cellUV
              V4 _cr _cg _cb ca = rawColor
              V2 cw ch = sgCellSize globals
              V2 px py = pixelPos
              cellPos = V2 (floor' (px / cw)) (floor' (py / ch))
              cellUV' = V2 (fract' (px / cw)) (fract' (py / ch))
              baseColor = ifThenElse' (ca >=* 0)
                rawColor
                (buildColorShaderDispatch colorShaderSlots globals cellPos cellUV' _cr)
          in  scBgShader shaderCfg globals pixelPos cellPos cellUV' baseColor
          ) frags

    drawWindowColor
      (const (win, ContextColorOption (BlendRgbAlpha
        (FuncAdd, FuncAdd)
        (BlendingFactors SrcAlpha OneMinusSrcAlpha, BlendingFactors One Zero)
        (V4 0 0 0 0)) (pure True)))
      colored

-- | カーソル描画シェーダー。
compileCursorShader
  :: (ContextHandler ctx)
  => Window os RGBAFloat ()
  -> V2 Int             -- ^ ウィンドウサイズ
  -> ShaderConfig       -- ^ ユーザシェーダ設定
  -> Float              -- ^ cellWidth
  -> Float              -- ^ cellHeight
  -> Int                -- ^ cols
  -> Int                -- ^ rows
  -> ContextT ctx os IO (CompiledShader os (CursorShaderEnv os))
compileCursorShader win winSize shaderCfg cellW cellH numCols numRows =
  compileShader $ do
    time  <- getUniform curTimeBuf
    mouse <- getUniform curMouseBuf

    prims <- toPrimitiveStream curPrimitives

    let V2 sw sh = fmap fromIntegral winSize :: V2 VFloat
        projected = fmap (\(pos, col, corner) ->
          let V2 px py = pos
              cx = px * 2 / sw - 1
              cy = 1 - py * 2 / sh
          in  (V4 cx cy 0 1, (col, corner))
          ) prims

    frags <- rasterize
      (const ( FrontAndBack
             , ViewPort (V2 0 0) winSize
             , DepthRange 0 1
             ))
      projected

    let globals = ShaderGlobals
          { sgTime       = time
          , sgResolution = fmap fromIntegral winSize
          , sgMouse      = mouse
          , sgGridSize   = V2 (fromIntegral numCols) (fromIntegral numRows)
          , sgCellSize   = V2 (realToFrac cellW) (realToFrac cellH)
          }
        colored = fmap (\(col, cellUV) ->
          scCursorShader shaderCfg globals cellUV col
          ) frags

    drawWindowColor
      (const (win, ContextColorOption (BlendRgbAlpha
        (FuncAdd, FuncAdd)
        (BlendingFactors SrcAlpha OneMinusSrcAlpha, BlendingFactors One Zero)
        (V4 0 0 0 0)) (pure True)))
      colored

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
--
-- @scrollOffset@ が正の場合、スクロールバック履歴を表示する。
-- @selection@ が @Just ((r1,c1),(r2,c2))@ の場合、選択範囲の前景色・背景色を反転する。
buildCellData
  :: FontAtlas
  -> Terminal
  -> (Bool -> TerminalColor -> V4 Float)
  -> Int    -- ^ scrollOffset (0 = 通常表示, >0 = 履歴をスクロール表示)
  -> Maybe ((Int, Int), (Int, Int))  -- ^ selection range (normalized: start <= end)
  -> ( [V4 Float]   -- ^ bgColor per cell
     , [V4 Float]   -- ^ fgColor per cell
     , [V4 Float]   -- ^ glyphUV (u0,v0,u1,v1) per cell
     , [V4 Float]   -- ^ glyphPos (ox,oy,w,h) per cell
     )
buildCellData atlas term colorFn scrollOffset selection =
  unzip4 $ map cellData (indices (screen term))
  where
    asc = fromIntegral (faAscender atlas) :: Float
    sbuf = scrollbackBuffer term
    sbLen = Seq.length sbuf
    r = rows term
    c = cols term

    -- | スクロールオフセットを考慮してセルの文字を取得する。
    lookupCell (y, x)
      | scrollOffset <= 0 = screen term ! (y, x)
      | otherwise =
          let vIdx = sbLen - scrollOffset + (y - 1)
          in if vIdx < 0
             then emptyChar
             else if vIdx < sbLen
                  then let line = Seq.index sbuf vIdx
                       in if x >= 1 && x <= length line
                          then line !! (x - 1)
                          else emptyChar
                  else let screenRow = vIdx - sbLen + 1
                       in if screenRow >= 1 && screenRow <= r && x >= 1 && x <= c
                          then screen term ! (screenRow, x)
                          else emptyChar

    emptyChar = TerminalChar ' ' (currentForeground term) (currentBackground term) False False False False

    cellData (y, x) =
      let tc  = lookupCell (y, x)
          bgc = if isInverse tc then foregroundColor tc else backgroundColor tc
          fgc = if isInverse tc then backgroundColor tc else foregroundColor tc
          bgCol = colorFn False bgc
          fgCol = colorFn (isBright tc) fgc
          -- 選択範囲内なら前景色と背景色を反転する
          selected = case selection of
            Nothing -> False
            Just ((r1, c1), (r2, c2))
              | r1 == r2  -> y == r1 && x >= c1 && x <= c2
              | otherwise -> (y == r1 && x >= c1)
                          || (y > r1 && y < r2)
                          || (y == r2 && x <= c2)
          (bgCol', fgCol') = if selected then (fgCol, bgCol) else (bgCol, fgCol)
          ch = character tc
          (glyphUV, glyphPos) = glyphInfo ch
      in (bgCol', fgCol', glyphUV, glyphPos)

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

-- | カーソル矩形の頂点。
buildCursorVertices
  :: FontAtlas
  -> Terminal
  -> V4 Float
  -> [(V2 Float, V4 Float, V2 Float)]
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
      in [ (V2 x0 y0, cursorCol, V2 0 0), (V2 x1 y0, cursorCol, V2 1 0), (V2 x1 y1, cursorCol, V2 1 1)
         , (V2 x0 y0, cursorCol, V2 0 0), (V2 x1 y1, cursorCol, V2 1 1), (V2 x0 y1, cursorCol, V2 0 1)
         ]
