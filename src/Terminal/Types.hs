{-# LANGUAGE DeriveGeneric #-}
-- | VT100/ANSI ターミナルエミュレータのコアデータ型。
--
-- ターミナルの状態（画面バッファ、カーソル、色、属性）、
-- パース済みアクション、ANSI SGR 属性モードを定義する。
module Terminal.Types (
  module Terminal.Types,
  ) where
import Data.Array
import Data.Binary (Binary(..), putWord8, getWord8)
import Data.Char
import Data.Maybe (fromJust, fromMaybe)
import Data.Sequence (Seq)
import Data.Tuple (swap)
import Data.Word (Word8)
import GHC.Generics (Generic)
import qualified System.Console.Terminfo as TI

-- | スクリーン上の座標。@(行, 列)@ で 1-indexed。
type ScreenIndex = (Int, Int)

-- | ワイド文字の右半分セルに使用するマーカー。
-- この文字が character フィールドに入っている場合、
-- セルは左隣のワイド文字の継続部分であり、描画をスキップする。
wideCharContinuation :: Char
wideCharContinuation = '\0'

-- | ワイド文字の継続セルかどうかを判定する。
isWideContinuation :: TerminalChar -> Bool
isWideContinuation tc = character tc == wideCharContinuation

-- | Unicode East Asian Width に基づき、文字が全角（2セル幅）かどうかを判定する。
isWideChar :: Char -> Bool
isWideChar c = let cp = ord c in
  -- CJK Unified Ideographs
     (cp >= 0x4E00  && cp <= 0x9FFF)
  -- CJK Extension A
  || (cp >= 0x3400  && cp <= 0x4DBF)
  -- CJK Extension B-I (Supplementary)
  || (cp >= 0x20000 && cp <= 0x323AF)
  -- CJK Compatibility Ideographs
  || (cp >= 0xF900  && cp <= 0xFAFF)
  -- CJK Radicals Supplement
  || (cp >= 0x2E80  && cp <= 0x2EFF)
  -- Kangxi Radicals
  || (cp >= 0x2F00  && cp <= 0x2FDF)
  -- CJK Symbols and Punctuation
  || (cp >= 0x3000  && cp <= 0x303F)
  -- Hiragana
  || (cp >= 0x3040  && cp <= 0x309F)
  -- Katakana
  || (cp >= 0x30A0  && cp <= 0x30FF)
  -- Katakana Phonetic Extensions
  || (cp >= 0x31F0  && cp <= 0x31FF)
  -- Bopomofo
  || (cp >= 0x3100  && cp <= 0x312F)
  -- Hangul Compatibility Jamo
  || (cp >= 0x3130  && cp <= 0x318F)
  -- Hangul Syllables
  || (cp >= 0xAC00  && cp <= 0xD7A3)
  -- Fullwidth Forms
  || (cp >= 0xFF01  && cp <= 0xFF60)
  || (cp >= 0xFFE0  && cp <= 0xFFE6)
  -- Enclosed Alphanumerics (①-⑳ etc.)
  || (cp >= 0x2460  && cp <= 0x24FF)
  -- Enclosed CJK Letters and Months (㈠-㈩, ㊀-㊉ etc.)
  || (cp >= 0x3200  && cp <= 0x32FF)
  -- Enclosed Ideographic Supplement (🈁-🈺 etc.)
  || (cp >= 0x1F200 && cp <= 0x1F2FF)
  -- Box Drawing (ambiguous but commonly wide in CJK terminals)
  -- || (cp >= 0x2500  && cp <= 0x257F)
  -- Miscellaneous Symbols (★☆♠♣♥♦ etc.)
  || (cp >= 0x2600  && cp <= 0x26FF)
  -- Dingbats
  || (cp >= 0x2700  && cp <= 0x27BF)
  -- Geometric Shapes (■□▲△ etc.)
  || (cp >= 0x25A0  && cp <= 0x25FF)
  -- Arrows (→←↑↓ etc.)
  || (cp >= 0x2190  && cp <= 0x21FF)
  -- Miscellaneous Technical
  || (cp >= 0x2300  && cp <= 0x23FF)

-- | ターミナル画面の1セルを表す。
-- 文字そのものに加え、前景色・背景色・表示属性を保持する。
data TerminalChar = TerminalChar {
    character :: !Char,              -- ^ 表示文字
    foregroundColor :: !TerminalColor, -- ^ 前景色
    backgroundColor :: !TerminalColor, -- ^ 背景色
    isBright :: !Bool,               -- ^ ボールド/ブライト属性
    isUnderlined :: !Bool,           -- ^ 下線属性
    isBlinking :: !Bool,             -- ^ 点滅属性
    isInverse :: !Bool               -- ^ 反転属性
} deriving (Show, Generic)

-- | 'ScreenIndex' でインデックスされる配列。
type TerminalArray = Array ScreenIndex

-- | ターミナル画面全体を表す 'TerminalChar' の2次元配列。
type TerminalScreen = TerminalArray TerminalChar

-- | 色情報用の整数配列（内部使用）。
type TerminalColorArray = TerminalArray Int

-- | スクロールバック履歴の1行分。各列の 'TerminalChar' のリスト。
type ScrollbackLine = [TerminalChar]

-- | 代替画面バッファに切り替わる前の通常画面の状態を保存する。
data AltScreenState = AltScreenState
  { asScreen       :: !TerminalScreen
  , asCursorPos    :: !ScreenIndex
  , asScrollRegion :: !(Int, Int)
  } deriving (Generic)

-- | ターミナル全体の状態。
--
-- 画面バッファ（'DiffArray' ベース）、カーソル位置、現在の描画属性、
-- スクロール領域、入力バッファ、terminfo ハンドルなどを含む。
data Terminal = Terminal {
    cursorPos :: !ScreenIndex,         -- ^ 現在のカーソル位置 @(行, 列)@
    screen :: !TerminalScreen,         -- ^ 画面バッファ
    inBuffer :: String,               -- ^ パース途中の入力バッファ
    responseBuffer :: String,         -- ^ ターミナルからの応答バッファ
    terminalTitle :: String,          -- ^ ウィンドウタイトル
    scrollingRegion :: !(Int, Int),    -- ^ スクロール領域 @(開始行, 終了行)@
    rows :: !Int,                      -- ^ ターミナルの行数
    cols :: !Int,                      -- ^ ターミナルの列数
    currentForeground :: !TerminalColor, -- ^ 現在の前景色
    currentBackground :: !TerminalColor, -- ^ 現在の背景色
    terminfo :: Maybe TI.Terminal,    -- ^ terminfo ハンドル（GUI使用時は 'Just'、テスト時は 'Nothing'）
    optionShowCursor :: !Bool,         -- ^ カーソル表示フラグ
    optionBright :: !Bool,             -- ^ ブライト/ボールド属性
    optionUnderlined :: !Bool,         -- ^ 下線属性
    optionInverse :: !Bool,            -- ^ 反転属性
    optionBlinking :: !Bool,           -- ^ 点滅属性
    mouseMode :: !MouseMode,           -- ^ マウストラッキングモード
    mouseEncoding :: !MouseEncoding,   -- ^ マウスイベントのエンコーディング
    pendingWrap :: !Bool,              -- ^ 行末で次の文字入力まで折り返しを保留するフラグ (DECAWM)
    scrollbackBuffer :: Seq ScrollbackLine, -- ^ スクロールバック履歴
    scrollbackMax :: !Int,             -- ^ スクロールバックの最大行数
    altScreen :: Maybe AltScreenState, -- ^ 代替画面バッファ（'Just' なら代替画面中）
    savedCursor :: Maybe ScreenIndex  -- ^ DECSC で保存したカーソル位置
}

-- | パース済みのターミナルアクション。
--
-- ANSI エスケープシーケンスおよび制御文字から変換された、
-- ターミナル状態マシンへの入力を表す代数的データ型。
data TerminalAction =
       CharInput Char                  -- ^ 通常の文字入力

     -- カーソル移動
     | CursorUp Int                     -- ^ カーソルを n 行上へ (CSI A)
     | CursorDown Int                   -- ^ カーソルを n 行下へ (CSI B)
     | CursorForward Int                -- ^ カーソルを n 列右へ (CSI C)
     | CursorBackward Int               -- ^ カーソルを n 列左へ (CSI D)
     | SetCursor Int Int                -- ^ カーソルを絶対位置 @(行, 列)@ へ移動 (CSI H / CSI f)
     | CursorAbsoluteColumn Int         -- ^ カーソルを指定列へ移動 (CSI G)
     | CursorAbsoluteRow Int            -- ^ カーソルを指定行へ移動 (CSI d)

     -- スクロール
     | SetScrollingRegion Int Int       -- ^ スクロール領域の設定 (CSI r)
     | ScrollUp Int                     -- ^ n 行上スクロール (CSI S)
     | ScrollDown Int                   -- ^ n 行下スクロール (CSI T)

     | ANSIAction [Int] Char            -- ^ 未解釈の CSI シーケンス（パラメータ + 終端文字）
     | DECAction [Int] Char             -- ^ DEC プライベート CSI シーケンス (CSI ? params... letter)
     | SetMouseMode MouseMode           -- ^ マウストラッキングモードの設定
     | SetMouseEncoding MouseEncoding   -- ^ マウスエンコーディングの設定
     | KeypadKeysApplicationsMode       -- ^ キーパッドをアプリケーションモードに設定 (ESC =)
     | KeypadKeysNumericMode            -- ^ キーパッドを数値モードに設定 (ESC >)
     | SetAttributeMode [AttributeMode] -- ^ SGR 属性の設定 (CSI m)
     | SetTerminalTitle String          -- ^ ウィンドウタイトルの設定 (OSC 0;...BEL)
     | ShowCursor Bool                  -- ^ カーソルの表示/非表示 (CSI ?25h / CSI ?25l)
     | SaveCursorPos                    -- ^ カーソル位置を保存 (DECSC / ESC 7)
     | RestoreCursorPos                 -- ^ カーソル位置を復元 (DECRC / ESC 8)
     | Ignored                          -- ^ 無視するシーケンス
     deriving (Show, Eq, Generic)

-- | ターミナルの色。
--
-- 基本8色（SGR 30-37 / 40-47）、256色パレット（SGR 38;5;N / 48;5;N）、
-- TrueColor（SGR 38;2;R;G;B / 48;2;R;G;B）に対応。
data TerminalColor =
      Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White
    | Color256 !Int             -- ^ 256色パレット (0-255)
    | ColorRGB !Word8 !Word8 !Word8  -- ^ TrueColor 24bit (R, G, B)
    deriving (Show, Eq, Generic)

instance Enum TerminalColor where
    fromEnum = fromJust . flip lookup tableTC
    toEnum = fromJust . flip lookup (map swap tableTC)
tableTC = [ (Black, 0)
          , (Red, 1)
          , (Green, 2)
          , (Yellow, 3)
          , (Blue, 4)
          , (Magenta, 5)
          , (Cyan, 6)
          , (White, 7)
          ]

-- | SGR (Select Graphic Rendition) 属性モード。
-- ANSI SGR パラメータの数値から 'toEnum' で変換される。
data AttributeMode =
       InvalidAttributeMode             -- ^ 不明な属性コード
     | ResetAllAttributes               -- ^ 全属性リセット (SGR 0)
     | Bright                            -- ^ ボールド/ブライト (SGR 1)
     | Dim                               -- ^ 薄暗い表示 (SGR 2)
     | Underlined                         -- ^ 下線 (SGR 4)
     | Blinking                           -- ^ 点滅 (SGR 5)
     | Inverse                            -- ^ 反転 (SGR 7)
     | Hidden                             -- ^ 非表示 (SGR 8)
     | Normal                             -- ^ ブライトをリセット (SGR 22)
     | NotUnderlined                      -- ^ 下線をリセット (SGR 24)
     | NotBlinking                        -- ^ 点滅をリセット (SGR 25)
     | NotInverse                         -- ^ 反転をリセット (SGR 27)
     | NotHidden                          -- ^ 非表示をリセット (SGR 28)
     | Foreground TerminalColor           -- ^ 前景色の設定 (SGR 30-37)
     | Background TerminalColor           -- ^ 背景色の設定 (SGR 40-47)
     | ResetForeground                    -- ^ 前景色をデフォルトに戻す (SGR 39)
     | ResetBackground                    -- ^ 背景色をデフォルトに戻す (SGR 49)
     deriving (Show, Eq, Generic)

instance Enum AttributeMode where
    fromEnum = fromJust . flip lookup tableAM
    toEnum = (fromMaybe InvalidAttributeMode) . flip lookup (map swap tableAM)
tableAM = [ (ResetAllAttributes, 0)
          , (Bright, 1)
          , (Dim, 2)
          , (Underlined, 4)
          , (Blinking, 5)
          , (Inverse, 7)
          , (Hidden, 8)
          , (Normal, 22)
          , (NotUnderlined, 24)
          , (NotBlinking, 25)
          , (NotInverse, 27)
          , (NotHidden, 28)
          ] ++ [(Foreground tcol, 30 + fromEnum tcol) | tcol <- [Black .. White]]
            ++ [(Background tcol, 40 + fromEnum tcol) | tcol <- [Black .. White]]
            ++ [(ResetForeground, 39), (ResetBackground, 49)]            -- SGR 90-97: 明るい前景色（Color256 8-15 にマッピング）
            ++ [(Foreground (Color256 (8 + fromEnum tcol)), 90 + fromEnum tcol) | tcol <- [Black .. White]]
            -- SGR 100-107: 明るい背景色（Color256 8-15 にマッピング）
            ++ [(Background (Color256 (8 + fromEnum tcol)), 100 + fromEnum tcol) | tcol <- [Black .. White]]

-- | マウストラッキングモード。
--
-- アプリケーションが CSI ?1000h 等で有効化する。
data MouseMode
  = MouseNone       -- ^ マウストラッキング無効
  | MouseNormal     -- ^ ボタン押下/離上のみ報告 (CSI ?1000h)
  | MouseButton     -- ^ ボタン押下中のドラッグも報告 (CSI ?1002h)
  | MouseAll        -- ^ 全モーション報告 (CSI ?1003h)
  deriving (Show, Eq, Generic)

-- | マウスイベントのエンコーディング方式。
data MouseEncoding
  = MouseEncodingX10   -- ^ デフォルト X10 互換: ESC [ M Cb Cx Cy
  | MouseEncodingSGR   -- ^ SGR 拡張: ESC [ < Cb;Cx;Cy M/m (座標 > 223 対応)
  deriving (Show, Eq, Generic)

-- ── Binary インスタンス ──────────────────────────────────

-- Generic から自動導出
instance Binary TerminalChar
instance Binary TerminalColor
instance Binary MouseMode
instance Binary MouseEncoding
instance Binary AltScreenState

-- | Terminal の Binary インスタンス。
-- terminfo フィールドはシリアライズ不可（opaque ハンドル）なので除外する。
-- デシリアライズ後は 'Nothing' になり、呼び出し側で再構築する必要がある。
instance Binary Terminal where
  put t = do
    put (cursorPos t)
    put (screen t)
    put (inBuffer t)
    put (responseBuffer t)
    put (terminalTitle t)
    put (scrollingRegion t)
    put (rows t)
    put (cols t)
    put (currentForeground t)
    put (currentBackground t)
    -- terminfo は除外（opaque ハンドル）
    put (optionShowCursor t)
    put (optionBright t)
    put (optionUnderlined t)
    put (optionInverse t)
    put (optionBlinking t)
    put (mouseMode t)
    put (mouseEncoding t)
    put (pendingWrap t)
    put (scrollbackBuffer t)
    put (scrollbackMax t)
    put (altScreen t)
    put (savedCursor t)
  get = Terminal
    <$> get              -- cursorPos
    <*> get              -- screen
    <*> get              -- inBuffer
    <*> get              -- responseBuffer
    <*> get              -- terminalTitle
    <*> get              -- scrollingRegion
    <*> get              -- rows
    <*> get              -- cols
    <*> get              -- currentForeground
    <*> get              -- currentBackground
    <*> pure Nothing     -- terminfo（デシリアライズ後に再構築）
    <*> get              -- optionShowCursor
    <*> get              -- optionBright
    <*> get              -- optionUnderlined
    <*> get              -- optionInverse
    <*> get              -- optionBlinking
    <*> get              -- mouseMode
    <*> get              -- mouseEncoding
    <*> get              -- pendingWrap
    <*> get              -- scrollbackBuffer
    <*> get              -- scrollbackMax
    <*> get              -- altScreen
    <*> get              -- savedCursor
