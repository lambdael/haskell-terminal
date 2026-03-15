{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}
-- | シェーダカスタマイズ用の型定義とデフォルト実装。
--
-- ユーザが @config.hs@ でシェーダや色スロットをカスタマイズするための型、
-- デフォルト値、および色解決関数を提供する。
module Hsterm.GPipe.Shader
  ( -- * シェーダグローバル
    ShaderGlobals(..)
    -- * レイヤーシェーダ
  , BgShaderDef
  , TextShaderDef
  , CursorShaderDef
  , ShaderConfig(..)
  , defaultShaderConfig
    -- * 色スロット
  , ColorSlot(..)
  , ColorScheme(..)
  , defaultColorScheme
  , solidScheme
    -- * 色ヘルパー
  , hexColor
  , colourToColorSlot
    -- * 色解決（CPU 側）
  , mkColorResolver
  , hasAnimatedSlots
    -- * GPU ディスパッチ（Renderer 用内部 API）
  , colorSlotIndex
  , collectColorShaderSlots
  , buildColorShaderDispatch
  ) where

import Prelude hiding ((<*))

import Data.Colour (Colour)
import Data.Colour.SRGB (RGB(..), toSRGB, sRGB24read)
import Data.Word (Word8)

import Graphics.GPipe

import Terminal.Types (TerminalColor(..))

-- ── シェーダグローバル ───────────────────────────────────

-- | GPU シェーダ内で利用可能なグローバル情報。
--
-- @compileShader@ ブロック内で uniform バッファと定数から構築され、
-- ユーザ定義シェーダ関数に渡される。
data ShaderGlobals = ShaderGlobals
  { sgTime       :: FFloat         -- ^ 経過時間（秒）
  , sgResolution :: V2 FFloat      -- ^ ウィンドウサイズ（ピクセル）
  , sgMouse      :: V2 FFloat      -- ^ マウスカーソル座標（ピクセル）
  , sgGridSize   :: V2 FFloat      -- ^ グリッドサイズ（列数, 行数）
  , sgCellSize   :: V2 FFloat      -- ^ セルサイズ（ピクセル: 幅, 高さ）
  }

-- ── レイヤーシェーダ ─────────────────────────────────────

-- | 背景レイヤーシェーダ。
--
-- デフォルトはパススルー（@baseColor@ をそのまま返す）。
-- ユーザはグラデーション、パターン、エフェクト等を適用できる。
type BgShaderDef
  = ShaderGlobals
  -> V2 FFloat      -- ^ pixelPos: フラグメントのピクセル座標
  -> V2 FFloat      -- ^ cellPos: セル座標 (col, row)
  -> V2 FFloat      -- ^ cellUV: セル内正規化座標 (0..1)
  -> V4 FFloat      -- ^ baseColor: ColorSlot 解決済みの色
  -> V4 FFloat      -- ^ 出力 RGBA

-- | テキストレイヤーシェーダ。
--
-- デフォルトは @fgColor@ と @glyphAlpha@ を合成する標準的なテキスト描画。
-- ユーザはグロー、影、色変換等のエフェクトを適用できる。
type TextShaderDef
  = ShaderGlobals
  -> V2 FFloat      -- ^ pixelPos: フラグメントのピクセル座標
  -> V2 FFloat      -- ^ cellPos: セル座標 (col, row)
  -> V4 FFloat      -- ^ fgColor: 前景色
  -> FFloat          -- ^ glyphAlpha: フォントアトラスからのアルファ値
  -> V4 FFloat      -- ^ 出力 RGBA

-- | カーソルシェーダ。
--
-- デフォルトはパススルー。
-- ユーザは点滅やフェードアニメーション等を適用できる。
type CursorShaderDef
  = ShaderGlobals
  -> V2 FFloat      -- ^ cellUV: カーソルセル内座標 (0..1)
  -> V4 FFloat      -- ^ baseColor: カーソル色
  -> V4 FFloat      -- ^ 出力 RGBA

-- | レイヤー単位のシェーダカスタマイズ設定。
data ShaderConfig = ShaderConfig
  { scBgShader     :: BgShaderDef     -- ^ 背景レイヤーシェーダ
  , scTextShader   :: TextShaderDef   -- ^ テキストレイヤーシェーダ
  , scCursorShader :: CursorShaderDef -- ^ カーソルシェーダ
  }

-- | デフォルトのシェーダ設定（パススルー）。
defaultShaderConfig :: ShaderConfig
defaultShaderConfig = ShaderConfig
  { scBgShader     = \_globals _pixelPos _cellPos _cellUV baseColor -> baseColor
  , scTextShader   = \_globals _pixelPos _cellPos fgColor glyphAlpha ->
      let V4 r g b _ = fgColor
      in  V4 r g b glyphAlpha
  , scCursorShader = \_globals _cellUV baseColor -> baseColor
  }

-- ── 色スロット ──────────────────────────────────────────

-- | 各色スロットの定義。
--
-- * 'SolidColor': 従来通りの静的色（パフォーマンス影響なし）
-- * 'AnimatedColor': CPU 側で毎フレーム色を計算（テクスチャ更新が必要）
-- * 'ColorShader': GPU 側フラグメントシェーダで計算（最も柔軟）
data ColorSlot
  = SolidColor !(V4 Float)
    -- ^ 静的色 (RGBA)
  | AnimatedColor (Float -> V4 Float)
    -- ^ CPU 側アニメーション: 経過秒 → RGBA
  | ColorShader (ShaderGlobals -> V2 FFloat -> V2 FFloat -> V4 FFloat)
    -- ^ GPU シェーダ: globals → cellPos (col, row) → cellUV (0..1) → RGBA

-- | カラースキーム: 基本 ANSI 色を 'ColorSlot' にマッピングする。
--
-- 通常色 (0–7) と明るい色 (8–15) の 16 スロット。
-- 'Color256' 0–15 も対応するスロットを参照し、
-- 16–255 および 'ColorRGB' は直接 RGB 値として解決される。
data ColorScheme = ColorScheme
  { csNormal :: TerminalColor -> ColorSlot
    -- ^ 通常色パレット（基本8色 → ColorSlot）
  , csBright :: TerminalColor -> ColorSlot
    -- ^ 明るい色パレット（基本8色 → ColorSlot）
  }

-- | 'Colour Double' を 'SolidColor' に変換する。
colourToColorSlot :: Colour Double -> ColorSlot
colourToColorSlot c =
  let RGB r g b = toSRGB c
  in SolidColor (V4 (realToFrac r) (realToFrac g) (realToFrac b) 1.0)

-- | 16進数カラーコードから 'V4 Float' を作成する。
--
-- @hexColor \"#ff6565\"  →  V4 1.0 0.396 0.396 1.0@
hexColor :: String -> V4 Float
hexColor hex =
  let RGB r g b = toSRGB (sRGB24read hex :: Colour Double)
  in V4 (realToFrac r) (realToFrac g) (realToFrac b) 1.0

-- | 静的色パレットからカラースキームを作成する。
--
-- 既存の @TerminalColor -> Colour Double@ 関数を再利用する場合に便利。
solidScheme :: (TerminalColor -> Colour Double) -> (TerminalColor -> Colour Double) -> ColorScheme
solidScheme normalMap brightMap = ColorScheme
  { csNormal = colourToColorSlot . normalMap
  , csBright = colourToColorSlot . brightMap
  }

-- | デフォルトのカラースキーム。
defaultColorScheme :: ColorScheme
defaultColorScheme = solidScheme defaultNormal defaultBright
  where
    defaultNormal Black   = sRGB24read "#330000"
    defaultNormal Red     = sRGB24read "#ff6565"
    defaultNormal Green   = sRGB24read "#56a700"
    defaultNormal Yellow  = sRGB24read "#eab93d"
    defaultNormal Blue    = sRGB24read "#204a87"
    defaultNormal Magenta = sRGB24read "#c4a000"
    defaultNormal Cyan    = sRGB24read "#89b6e2"
    defaultNormal White   = sRGB24read "#cccccc"
    defaultNormal _       = sRGB24read "#cccccc"

    defaultBright Black   = sRGB24read "#555753"
    defaultBright Red     = sRGB24read "#ff8d8d"
    defaultBright Green   = sRGB24read "#c8e7a8"
    defaultBright Yellow  = sRGB24read "#ffc123"
    defaultBright Blue    = sRGB24read "#3465a4"
    defaultBright Magenta = sRGB24read "#f57900"
    defaultBright Cyan    = sRGB24read "#46a4ff"
    defaultBright White   = sRGB24read "#ffffff"
    defaultBright _       = sRGB24read "#ffffff"

-- ── 色解決（CPU 側） ────────────────────────────────────

-- | 基本8色のリスト。
basicColors :: [TerminalColor]
basicColors = [Black, Red, Green, Yellow, Blue, Magenta, Cyan, White]

-- | ColorSlot のインデックス（GPU ディスパッチ用）。
--
-- @
-- Normal: Black=0, Red=1, ..., White=7
-- Bright: Black=8, Red=9, ..., White=15
-- @
colorSlotIndex :: Bool -> TerminalColor -> Int
colorSlotIndex False tc = fromEnum tc       -- 0-7
colorSlotIndex True  tc = fromEnum tc + 8   -- 8-15

-- | カラースキームと現在時刻から色解決関数を作成する。
--
-- 'buildCellData' の @colorFn@ 引数として使用する。
-- 'SolidColor' と 'AnimatedColor' は直接 'V4 Float' を返す。
-- 'ColorShader' はセンチネル値 @V4 index 0 0 (-1)@ を返し、
-- alpha \< 0 で GPU 側にディスパッチを通知する。
mkColorResolver :: ColorScheme -> Float -> Bool -> TerminalColor -> V4 Float
mkColorResolver scheme time bright tc = case tc of
  ColorRGB r g b ->
    V4 (fromIntegral r / 255.0) (fromIntegral g / 255.0) (fromIntegral b / 255.0) 1.0
  Color256 n -> resolveColor256 scheme time n
  _ ->
    let slot = (if bright then csBright scheme else csNormal scheme) tc
    in resolveSlotCPU time slot (colorSlotIndex bright tc)

-- | 色スロットを CPU 側で解決する。
resolveSlotCPU :: Float -> ColorSlot -> Int -> V4 Float
resolveSlotCPU _    (SolidColor c)    _ = c
resolveSlotCPU time (AnimatedColor f) _ = f time
resolveSlotCPU _    (ColorShader _)   i = V4 (fromIntegral i) 0 0 (-1)

-- | 256色パレットのインデックスを V4 Float に変換する。
resolveColor256 :: ColorScheme -> Float -> Int -> V4 Float
resolveColor256 scheme time n
  | n < 0     = resolveColor256 scheme time 0
  | n < 8     = let basic = basicColors !! n
                in resolveSlotCPU time (csNormal scheme basic) (colorSlotIndex False basic)
  | n < 16    = let basic = basicColors !! (n - 8)
                in resolveSlotCPU time (csBright scheme basic) (colorSlotIndex True basic)
  | n < 232   = let idx = n - 16
                    ri = idx `div` 36
                    gi = (idx `mod` 36) `div` 6
                    bi = idx `mod` 6
                    toF i = if i == 0 then 0.0
                            else fromIntegral (55 + 40 * i) / 255.0
                in V4 (toF ri) (toF gi) (toF bi) 1.0
  | n < 256   = let g = fromIntegral (8 + 10 * (n - 232)) / 255.0 :: Float
                in V4 g g g 1.0
  | otherwise = resolveColor256 scheme time 255

-- | アニメーション付き色スロットが存在するかチェック。
--
-- いずれかが存在する場合、毎フレームテクスチャ更新が必要。
hasAnimatedSlots :: ColorScheme -> Bool
hasAnimatedSlots scheme = any isAnimated normals || any isAnimated brights
  where
    normals = map (csNormal scheme) basicColors
    brights = map (csBright scheme) basicColors
    isAnimated (SolidColor _)    = False
    isAnimated (AnimatedColor _) = True
    isAnimated (ColorShader _)   = True

-- ── GPU ディスパッチ ────────────────────────────────────

-- | 'ColorShader' スロットを収集する（GPU ディスパッチチェーン構築用）。
collectColorShaderSlots :: ColorScheme
  -> [(Int, ShaderGlobals -> V2 FFloat -> V2 FFloat -> V4 FFloat)]
collectColorShaderSlots scheme =
  [ (colorSlotIndex False c, fn) | c <- basicColors, ColorShader fn <- [csNormal scheme c] ]
  ++ [ (colorSlotIndex True c, fn) | c <- basicColors, ColorShader fn <- [csBright scheme c] ]

-- | ColorShader スロットの GPU ディスパッチチェーンを構築する。
--
-- フラグメントシェーダ内で、スロットインデックスに基づいて
-- 対応する ColorShader 関数を呼び出す @ifThenElse'@ チェーン。
buildColorShaderDispatch
  :: [(Int, ShaderGlobals -> V2 FFloat -> V2 FFloat -> V4 FFloat)]
  -> ShaderGlobals
  -> V2 FFloat      -- ^ cellPos
  -> V2 FFloat      -- ^ cellUV
  -> FFloat          -- ^ slotIdx (色テクスチャの R チャネル)
  -> V4 FFloat
buildColorShaderDispatch [] _globals _cellPos _cellUV _slotIdx =
  V4 1 0 1 1  -- フォールバック: magenta（エラー表示）
buildColorShaderDispatch ((idx, shaderFn) : rest) globals cellPos cellUV slotIdx =
  let fi = fromIntegral idx :: FFloat
  in ifThenElse' (abs (slotIdx - fi) <* 0.5)
       (shaderFn globals cellPos cellUV)
       (buildColorShaderDispatch rest globals cellPos cellUV slotIdx)
