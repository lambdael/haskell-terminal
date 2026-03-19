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
  , WallpaperShaderEnv(..)
    -- * シェーダーコンパイル
  , compileTextShader
  , compileBgShader
  , compileCursorShader
  , compileWallpaperShader
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

import Graphics.GPipe.Font.Types (FontInfo(..), GlyphRegion(..), GlyphMetrics(..))

import Terminal.Types
import Hsterm.GPipe.Shader

-- ── シェーダー環境 ────────────────────────────────────────

-- | テキスト描画用シェーダー環境（セルデータテクスチャ方式）。
data TextShaderEnv os = TextShaderEnv
  { tePrimitives  :: PrimitiveArray Triangles (B2 Float, B2 Float)
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

-- | 壁紙（全画面背景）描画用シェーダー環境。
data WallpaperShaderEnv os = WallpaperShaderEnv
  { wpPrimitives :: PrimitiveArray Triangles (B2 Float)
  , wpTimeBuf    :: (Buffer os (Uniform (B Float)), Int)
  , wpMouseBuf   :: (Buffer os (Uniform (B2 Float)), Int)
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

    -- Use full-cell quads (same vertex format as bg shader: (pixelPos, cellUV))
    -- Position texture is sampled in fragment shader to avoid vertex texture
    -- fetch issues on Intel GPUs.
    let V2 sw sh = fmap fromIntegral winSize :: V2 VFloat
        projected = fmap (\(pos, cellUV) ->
          let V2 px py = pos
              cx = px * 2 / sw - 1
              cy = 1 - py * 2 / sh
          in  (V4 cx cy 0 1, (cellUV, V2 px py))
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
          let V2 cw ch = sgCellSize globals
              V2 px py = pixelPos
              -- Glyph subrect from position texture (sampled in fragment shader)
              V4 ox oy gw gh = sample2D posSamp SampleAuto Nothing Nothing cellUV
              -- Atlas UV rect
              V4 u0 v0 u1 v1 = sample2D uvSamp SampleAuto Nothing Nothing cellUV
              -- Local offset within cell
              cellTLx = floor' (px / cw) * cw
              cellTLy = floor' (py / ch) * ch
              localX = px - cellTLx
              localY = py - cellTLy
              -- Check if fragment is inside glyph subrect
              insideGlyph = localX >=* ox &&* localX <* (ox + gw)
                        &&* localY >=* oy &&* localY <* (oy + gh)
              -- Compute atlas UV for this fragment
              t = ifThenElse' (gw >* 0) ((localX - ox) / gw) 0
              s = ifThenElse' (gh >* 0) ((localY - oy) / gh) 0
              u = u0 + t * (u1 - u0)
              v = v0 + s * (v1 - v0)
              glyphAlpha = sample2D fontSamp SampleAuto Nothing Nothing (V2 u v)
              -- Foreground color
              rawFg = sample2D fgSamp SampleAuto Nothing Nothing cellUV
              V4 _fr _fg _fb fa = rawFg
              cellPos = V2 (floor' (px / cw)) (floor' (py / ch))
              cellUV' = V2 (fract' (px / cw)) (fract' (py / ch))
              fgColor = ifThenElse' (fa >=* 0)
                rawFg
                (buildColorShaderDispatch colorShaderSlots globals cellPos cellUV' _fr)
              V4 fR fG fB _ = fgColor
              finalAlpha = ifThenElse' insideGlyph glyphAlpha 0
          in  V4 fR fG fB finalAlpha
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
          in  baseColor
          ) frags
        -- scBgAlpha でアルファを調整（壁紙を透かせるため）
        -- DEBUG: 全セルを緑色で強制描画
        alphaAdjusted = fmap (\c ->
          let V4 cr cg cb ca = c
          in V4 cr cg cb (scBgAlpha shaderCfg ca)
          ) colored

    drawWindowColor
      (const (win, ContextColorOption (BlendRgbAlpha
        (FuncAdd, FuncAdd)
        (BlendingFactors SrcAlpha OneMinusSrcAlpha, BlendingFactors One Zero)
        (V4 0 0 0 0)) (pure True)))
      alphaAdjusted

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

-- | 壁紙（全画面背景）シェーダー。
--
-- 画面全体をカバーする 1 枚の quad を描画し、ユーザ定義シェーダで色を決定する。
compileWallpaperShader
  :: (ContextHandler ctx)
  => Window os RGBAFloat ()
  -> V2 Int             -- ^ ウィンドウサイズ
  -> WallpaperShaderDef -- ^ ユーザ壁紙シェーダ
  -> Float              -- ^ cellWidth
  -> Float              -- ^ cellHeight
  -> Int                -- ^ cols
  -> Int                -- ^ rows
  -> ContextT ctx os IO (CompiledShader os (WallpaperShaderEnv os))
compileWallpaperShader win winSize wallpaperFn cellW cellH numCols numRows =
  compileShader $ do
    time  <- getUniform wpTimeBuf
    mouse <- getUniform wpMouseBuf

    prims <- toPrimitiveStream wpPrimitives

    -- 頂点は正規化座標 (0..1) の quad、クリップ空間に変換
    let projected = fmap (\uv ->
          let V2 u v = uv
              cx = u * 2 - 1
              cy = 1 - v * 2
          in  (V4 cx cy 0 1, uv)
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
        colored = fmap (wallpaperFn globals) frags

    drawWindowColor
      (const (win, ContextColorOption NoBlending (pure True)))
      colored

-- ── ユーティリティ ────────────────────────────────────────

-- | フォント情報からモノスペースのセル幅を計算する。
cellWidth :: FontInfo -> Float
cellWidth fi = case Map.lookup ' ' (fiGlyphs fi) of
  Just gr -> fromIntegral (gmAdvanceX (grMetrics gr))
  Nothing -> fromIntegral (fiLineH fi) * 0.5

-- ── 固定グリッド頂点（リサイズ時のみ生成） ──────────────────

-- | 背景用固定グリッド頂点。
--
-- 各セルに対して 6 頂点 @(position, cellTexCoord)@ を出力する。
-- position = セル矩形の各角のピクセル座標。
-- cellTexCoord = セルデータテクスチャの対応テクセル中心（全6頂点で同一値）。
-- リサイズ時にのみ呼び出し、結果を GPU バッファに書き込む。
buildBgGridVertices
  :: FontInfo
  -> Int    -- ^ rows
  -> Int    -- ^ cols
  -> [(V2 Float, V2 Float)]
buildBgGridVertices fi r c =
  concatMap mkQuad [(row, col) | row <- [1..r], col <- [1..c]]
  where
    cw = cellWidth fi
    fh = fromIntegral (fiLineH fi) :: Float
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
  :: FontInfo
  -> Int    -- ^ rows
  -> Int    -- ^ cols
  -> [(V2 Float, V2 Float, V2 Float)]
buildTextGridVertices fi r c =
  concatMap mkQuad [(row, col) | row <- [1..r], col <- [1..c]]
  where
    cw = cellWidth fi
    fh = fromIntegral (fiLineH fi) :: Float
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
  :: FontInfo
  -> Terminal
  -> (Bool -> TerminalColor -> V4 Float)
  -> Int    -- ^ scrollOffset (0 = 通常表示, >0 = 履歴をスクロール表示)
  -> Maybe ((Int, Int), (Int, Int))  -- ^ selection range (normalized: start <= end)
  -> ( [V4 Float]   -- ^ bgColor per cell
     , [V4 Float]   -- ^ fgColor per cell
     , [V4 Float]   -- ^ glyphUV (u0,v0,u1,v1) per cell
     , [V4 Float]   -- ^ glyphPos (ox,oy,w,h) per cell
     )
buildCellData fi term colorFn scrollOffset selection =
  unzip4 $ map cellData (indices (screen term))
  where
    asc = fromIntegral (fiAscender fi) :: Float
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
          (glyphUV, glyphPos)
            | ch == '\0', x > 1 =
                -- ワイド文字の継続セル: 前セルのグリフの右半分を描画
                let prevCh = character (lookupCell (y, x - 1))
                in case Map.lookup prevCh (fiGlyphs fi) of
                  Nothing -> (V4 0 0 0 0, V4 0 0 0 0)
                  Just gr ->
                    let gm = grMetrics gr
                        cw' = cellWidth fi
                    in ( V4 (grU0 gr) (grV0 gr) (grU1 gr) (grV1 gr)
                       , V4 (fromIntegral (gmBearingX gm) - cw')
                            (asc - fromIntegral (gmBearingY gm))
                            (fromIntegral (gmWidth gm))
                            (fromIntegral (gmHeight gm))
                       )
            | otherwise = glyphInfo ch
      in (bgCol', fgCol', glyphUV, glyphPos)

    glyphInfo ' ' = (V4 0 0 0 0, V4 0 0 0 0)
    glyphInfo ch  = case Map.lookup ch (fiGlyphs fi) of
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
  :: FontInfo
  -> Terminal
  -> V4 Float
  -> [(V2 Float, V4 Float, V2 Float)]
buildCursorVertices fi term cursorCol =
  if not (optionShowCursor term)
    then []
    else
      let (cy, cx) = cursorPos term
          cw = cellWidth fi
          fh = fromIntegral (fiLineH fi) :: Float
          x0 = fromIntegral (cx - 1) * cw
          y0 = fromIntegral (cy - 1) * fh
          x1 = x0 + cw
          y1 = y0 + fh
      in [ (V2 x0 y0, cursorCol, V2 0 0), (V2 x1 y0, cursorCol, V2 1 0), (V2 x1 y1, cursorCol, V2 1 1)
         , (V2 x0 y0, cursorCol, V2 0 0), (V2 x1 y1, cursorCol, V2 1 1), (V2 x0 y1, cursorCol, V2 0 1)
         ]
