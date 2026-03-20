-- | ターミナルの設定型とデフォルト値。
--
-- 現在ハードコードされている値を一箇所に集約し、
-- 将来 Dyre によるホットリロードで上書きできるようにする。
module Hsterm.GPipe.Config
  ( TerminalConfig(..)
  , CursorStyle(..)
  , KeyCombo(..)
  , KeyBindings
  , defaultConfig
  , defaultKeyBindings
    -- * シェーダ設定（re-export）
  , module Hsterm.GPipe.Shader
    -- * スクロール操作
  , scrollPageUp
  , scrollPageDown
  , scrollToTop
  , scrollToBottom
    -- * クリップボード操作
  , doCopy
  ) where

import Data.Array ((!))
import Data.List (dropWhileEnd)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Linear (V4(..))

import Graphics.GPipe.Context.GLFW (Key(..))

import Terminal.Types (TerminalColor(..), Terminal(..), TerminalChar(..))
import Hsterm.GPipe.Monad
import Hsterm.GPipe.Shader

-- | カーソルの表示スタイル。
data CursorStyle
  = CursorBlock      -- ^ ブロック型（セル全体を塗りつぶし）
  | CursorUnderline  -- ^ 下線型
  | CursorBar        -- ^ バー型（セル左端の縦線）
  deriving (Show, Eq)

-- | キーコンボ（修飾キー + キー）。
--
-- @KeyCombo ctrl shift alt key@ で修飾キーの組み合わせを表す。
data KeyCombo = KeyCombo
  { kcCtrl  :: !Bool
  , kcShift :: !Bool
  , kcAlt   :: !Bool
  , kcKey   :: !Key
  } deriving (Show, Eq, Ord)

-- | キーバインドマップ。
-- キーコンボに対応するアクションを定義する。
type KeyBindings = Map KeyCombo (HstermM ())

-- | ターミナルエミュレータの設定。
--
-- ユーザが @~\/.config\/haskell-terminal\/config.hs@ で
-- @defaultConfig@ を上書きしてカスタマイズする想定。
data TerminalConfig = TerminalConfig
  { -- 外観
    tcFontFamily      :: !String
    -- ^ fontconfig パターン（例: @\"monospace:style=Regular\"@）。
    -- 空文字列の場合は @fc-match@ で自動検出する。
  , tcFontSize        :: !Int
    -- ^ フォントサイズ（ピクセル）
  , tcColorScheme     :: !ColorScheme
    -- ^ カラースキーム（8色×2 のスロット）
  , tcShaderConfig    :: !ShaderConfig
    -- ^ シェーダカスタマイズ設定
  , tcDefaultFg       :: !TerminalColor
    -- ^ デフォルト前景色
  , tcDefaultBg       :: !TerminalColor
    -- ^ デフォルト背景色
  , tcCursorColor     :: !(V4 Float)
    -- ^ カーソルの色 (RGBA)
  , tcCursorStyle     :: !CursorStyle
    -- ^ カーソルの表示スタイル

    -- ターミナル
  , tcInitialCols     :: !Int
    -- ^ 初期ウィンドウ幅（文字数）
  , tcInitialRows     :: !Int
    -- ^ 初期ウィンドウ高さ（行数）
  , tcScrollback      :: !Int
    -- ^ スクロールバック履歴の最大行数
  , tcShell           :: !(Maybe FilePath)
    -- ^ シェル実行ファイルのパス。@Nothing@ の場合は @$SHELL@ 環境変数を使用

    -- レンダリング
  , tcFrameDelay      :: !Int
    -- ^ フレーム間隔（マイクロ秒）。16000 ≒ 60fps

    -- 入力
  , tcKeyBindings     :: KeyBindings
    -- ^ カスタムキーバインド。デフォルトのキー処理より優先される。

    -- Dyre
  , tcErrorMsg        :: !(Maybe String)
    -- ^ Dyre コンパイルエラーメッセージ。リコンパイル失敗時に Dyre がセットする。
  }

-- | デフォルトの設定値。
defaultConfig :: TerminalConfig
defaultConfig = TerminalConfig
  { tcFontFamily      = ""
  , tcFontSize        = 24
  , tcColorScheme     = defaultColorScheme
  , tcDefaultFg       = White
  , tcDefaultBg       = Black
  , tcCursorColor     = V4 1.0 0.2 0.9 0.8
  , tcCursorStyle     = CursorBlock
  , tcInitialCols     = 80
  , tcInitialRows     = 24
  , tcScrollback      = 10000
  , tcShell           = Nothing
  , tcFrameDelay      = 16000
  , tcShaderConfig    = defaultShaderConfig
  , tcKeyBindings     = defaultKeyBindings
  , tcErrorMsg        = Nothing
  }

-- | デフォルトのキーバインド。
--
-- ユーザはこれをベースに @Map.union@ や @Map.insert@ で上書きできる:
--
-- @
-- myBindings = Map.insert (KeyCombo True True False Key'R) reloadAction
--            $ defaultKeyBindings
-- @
defaultKeyBindings :: KeyBindings
defaultKeyBindings = Map.fromList
  [ -- Ctrl+Q: 強制終了
    (KeyCombo True False False Key'Q, exitTerminal)

    -- Ctrl+Shift+C: 選択テキストをコピー
  , (KeyCombo True True False Key'C, doCopy)

    -- Ctrl+Shift+V: クリップボードからペースト
  , (KeyCombo True True False Key'V, requestPaste)

    -- Shift+PageUp/Down/Home/End: スクロールバック操作
  , (KeyCombo False True False Key'PageUp, scrollPageUp)
  , (KeyCombo False True False Key'PageDown, scrollPageDown)
  , (KeyCombo False True False Key'Home, scrollToTop)
  , (KeyCombo False True False Key'End, scrollToBottom)
  ]

-- | 選択テキストをクリップボードにコピーする。
doCopy :: HstermM ()
doCopy = do
  sel <- getSelection
  case sel of
    Just s -> do
      term <- getTerminal
      scrollOff <- getScrollOffset
      -- 選択座標は絶対行座標なので画面行座標に変換
      let ((r1, c1), (r2, c2)) = selectionRange s
          txt = extractSelectedText term scrollOff (r1 + scrollOff, c1) (r2 + scrollOff, c2)
      if null txt then return () else requestCopy txt
    Nothing -> return ()

-- | 1ページ分上にスクロールする。
scrollPageUp :: HstermM ()
scrollPageUp = do
  term <- getTerminal
  let maxOff = Seq.length (scrollbackBuffer term)
      pageSize = max 1 (rows term - 1)
  modifyScrollOffset (\off -> min maxOff (off + pageSize))
  setDirty

-- | 1ページ分下にスクロールする。
scrollPageDown :: HstermM ()
scrollPageDown = do
  term <- getTerminal
  let pageSize = max 1 (rows term - 1)
  modifyScrollOffset (\off -> max 0 (off - pageSize))
  setDirty

-- | スクロールバックの一番上まで移動する。
scrollToTop :: HstermM ()
scrollToTop = do
  term <- getTerminal
  setScrollOffset (Seq.length (scrollbackBuffer term))
  setDirty

-- | スクロールバックの一番下（最新）まで移動する。
scrollToBottom :: HstermM ()
scrollToBottom = do
  setScrollOffset 0
  setDirty

-- ── テキスト抽出ヘルパー ─────────────────────────────────

-- | ターミナル状態から選択範囲のテキストを抽出する。
extractSelectedText :: Terminal -> Int -> (Int, Int) -> (Int, Int) -> String
extractSelectedText term scrollOffset (ar, ac) (cr, cc) =
  let ((r1, c1), (r2, c2)) = if (ar, ac) <= (cr, cc)
                              then ((ar, ac), (cr, cc))
                              else ((cr, cc), (ar, ac))
      r = rows term
      c = cols term
      sbuf = scrollbackBuffer term
      sbLen = Seq.length sbuf
      lookupChar (y, x) =
        if scrollOffset <= 0
          then character (screen term ! (y, x))
          else let vIdx = sbLen - scrollOffset + (y - 1)
               in if vIdx < 0 then ' '
                  else if vIdx < sbLen
                       then let line = Seq.index sbuf vIdx
                            in if x >= 1 && x <= length line
                               then character (line !! (x - 1))
                               else ' '
                       else let screenRow = vIdx - sbLen + 1
                            in if screenRow >= 1 && screenRow <= r && x >= 1 && x <= c
                               then character (screen term ! (screenRow, x))
                               else ' '
      trimEnd = dropWhileEnd (== ' ')
      -- ワイド文字の継続セル ('\0') を除外する
      stripNulls = filter (/= '\0')
  in stripNulls $ if r1 == r2
     then trimEnd [lookupChar (r1, cx) | cx <- [c1..c2]]
     else let firstLine = trimEnd [lookupChar (r1, cx) | cx <- [c1..c]]
              midLines  = [trimEnd [lookupChar (ry, cx) | cx <- [1..c]] | ry <- [r1+1..r2-1]]
              lastLine  = trimEnd [lookupChar (r2, cx) | cx <- [1..c2]]
          in unlines (firstLine : midLines) ++ lastLine
