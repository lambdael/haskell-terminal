-- | VT100/ANSI ターミナルエミュレータのコアデータ型。
--
-- ターミナルの状態（画面バッファ、カーソル、色、属性）、
-- パース済みアクション、ANSI SGR 属性モードを定義する。
module Terminal.Types where
import Data.Array.Diff
import Data.Char
import Data.Maybe (fromJust, fromMaybe)
import Data.Tuple (swap)
import qualified System.Console.Terminfo as TI

-- | スクリーン上の座標。@(行, 列)@ で 1-indexed。
type ScreenIndex = (Int, Int)

-- | ターミナル画面の1セルを表す。
-- 文字そのものに加え、前景色・背景色・表示属性を保持する。
data TerminalChar = TerminalChar {
    character :: Char,              -- ^ 表示文字
    foregroundColor :: TerminalColor, -- ^ 前景色
    backgroundColor :: TerminalColor, -- ^ 背景色
    isBright :: Bool,               -- ^ ボールド/ブライト属性
    isUnderlined :: Bool,           -- ^ 下線属性
    isBlinking :: Bool,             -- ^ 点滅属性
    isInverse :: Bool               -- ^ 反転属性
} deriving (Show)

-- | 'ScreenIndex' でインデックスされる差分配列。
type TerminalArray = DiffArray ScreenIndex

-- | ターミナル画面全体を表す 'TerminalChar' の2次元配列。
type TerminalScreen = TerminalArray TerminalChar

-- | 色情報用の整数配列（内部使用）。
type TerminalColorArray = TerminalArray Int

-- | ターミナル全体の状態。
--
-- 画面バッファ（'DiffArray' ベース）、カーソル位置、現在の描画属性、
-- スクロール領域、入力バッファ、terminfo ハンドルなどを含む。
data Terminal = Terminal {
    cursorPos :: ScreenIndex,         -- ^ 現在のカーソル位置 @(行, 列)@
    screen :: TerminalScreen,         -- ^ 画面バッファ
    inBuffer :: String,               -- ^ パース途中の入力バッファ
    allBuffer :: [TerminalAction],    -- ^ 適用された全アクションの履歴（リサイズ時のリプレイ用）
    responseBuffer :: String,         -- ^ ターミナルからの応答バッファ
    terminalTitle :: String,          -- ^ ウィンドウタイトル
    scrollingRegion :: (Int, Int),    -- ^ スクロール領域 @(開始行, 終了行)@
    rows :: Int,                      -- ^ ターミナルの行数
    cols :: Int,                      -- ^ ターミナルの列数
    currentForeground :: TerminalColor, -- ^ 現在の前景色
    currentBackground :: TerminalColor, -- ^ 現在の背景色
    terminfo :: Maybe TI.Terminal,    -- ^ terminfo ハンドル（GUI使用時は 'Just'、テスト時は 'Nothing'）
    optionShowCursor :: Bool,         -- ^ カーソル表示フラグ
    optionBright :: Bool,             -- ^ ブライト/ボールド属性
    optionUnderlined :: Bool,         -- ^ 下線属性
    optionInverse :: Bool,            -- ^ 反転属性
    optionBlinking :: Bool            -- ^ 点滅属性
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
     | KeypadKeysApplicationsMode       -- ^ キーパッドをアプリケーションモードに設定 (ESC =)
     | KeypadKeysNumericMode            -- ^ キーパッドを数値モードに設定 (ESC >)
     | SetAttributeMode [AttributeMode] -- ^ SGR 属性の設定 (CSI m)
     | SetTerminalTitle String          -- ^ ウィンドウタイトルの設定 (OSC 0;...BEL)
     | ShowCursor Bool                  -- ^ カーソルの表示/非表示 (CSI ?25h / CSI ?25l)
     | Ignored                          -- ^ 無視するシーケンス
     deriving (Show, Eq)

-- | ANSI 基本8色。SGR コード 30-37（前景）/ 40-47（背景）に対応。
data TerminalColor =
      Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White
    deriving (Show, Eq)

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
     deriving (Show, Eq)

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
            ++ [(ResetForeground, 39), (ResetBackground, 49)]

