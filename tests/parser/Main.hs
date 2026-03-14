module Main where
import Data.Monoid
import System.Random (getStdGen)
import Control.Applicative ((<$>))

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck hiding ((==>))

import Terminal.Parser
import Terminal.Terminal
import Terminal.Types

import Data.Word (Word8)

-- ── ヘルパー ────────────────────────────────────────────

-- | パーサーの結果を期待値と比較するアサーション
parsesTo :: String -> [TerminalAction] -> Assertion
parsesTo str is = let Right result = parseANSI str in result @?= (is, "")

(==>) = parsesTo

-- ── カーソル移動 ────────────────────────────────────────

testSetCursor = "A\ESC[H\ESC[2;2H"
        ==> [CharInput 'A', SetCursor 1 1, SetCursor 2 2]

testInvalidSetCursor = "\ESC[H\ESC[2;2;X\ESC[5;1H"
        ==> [SetCursor 1 1, Ignored, SetCursor 5 1]

testMoveCursor = "A\ESC[A\ESCA\ESC[10B"
        ==> [CharInput 'A', CursorUp 1, Ignored, CursorDown 10]

testCursorForward = "\ESC[C\ESC[5C"
        ==> [CursorForward 1, CursorForward 5]

testCursorBackward = "\ESC[D\ESC[3D"
        ==> [CursorBackward 1, CursorBackward 3]

testCursorAbsoluteColumn = "\ESC[10G"
        ==> [CursorAbsoluteColumn 10]

testCursorAbsoluteRow = "\ESC[5d"
        ==> [CursorAbsoluteRow 5]

testCursorHome = "\ESC[H\ESC[f"
        ==> [SetCursor 1 1, SetCursor 1 1]

testCursorPosition = "\ESC[10;20H\ESC[3;4f"
        ==> [SetCursor 10 20, SetCursor 3 4]

-- ── スクロール ──────────────────────────────────────────

-- CSI T = SD (content DOWN) = internal ScrollUp
-- CSI S = SU (content UP)   = internal ScrollDown
testScrolling = "\ESC[T\ESC[2S\ESC[4T\ESC[S"
        ==> [ScrollUp 1, ScrollDown 2, ScrollUp 4, ScrollDown 1]

testScrollRegion = "\ESC[1;23r"
        ==> [SetScrollingRegion 1 23]

testScrollRegionReset = "\ESC[r"
        ==> [ANSIAction [] 'r']

-- ── 文字入力 ────────────────────────────────────────────

testCharInput = "A2$?"
        ==> [CharInput 'A', CharInput '2', CharInput '$', CharInput '?']

testCharInputIg = "\ESC[;nw\ESC[5652;7974;10;;;xA"
        ==> [Ignored, CharInput 'w', Ignored, CharInput 'A']

-- ── SGR 属性 ────────────────────────────────────────────

testSetDisplayAttributes1 = "\ESC[0m"
        ==> [SetAttributeMode [ResetAllAttributes]]

testSetDisplayAttributes2 = "\ESC[31;40m\ESC[25m"
        ==> [SetAttributeMode [Foreground Red, Background Black], SetAttributeMode [NotBlinking]]

testSetDisplayAttributes3 = "\ESC[37;4mU\ESC[0m"
        ==> [SetAttributeMode [Foreground White, Underlined],
                    CharInput 'U',
                    SetAttributeMode [ResetAllAttributes]]

testSetDisplayAttributes4 = "\ESC[30;5;43m"
        ==> [SetAttributeMode [Foreground Black, Blinking, Background Yellow]]

testSetDisplayAttributes5 = "\ESC[1111m\ESC[50m"
        ==> [SetAttributeMode [InvalidAttributeMode], SetAttributeMode [InvalidAttributeMode]]

testSetTerminalTitle = "\ESC]0;Chickens don't clap!\007b"
        ==> [SetTerminalTitle "Chickens don't clap!", CharInput 'b']

-- ── 消去操作 ────────────────────────────────────────────

testEraseDisplay0 = "\ESC[J"
        ==> [ANSIAction [] 'J']

testEraseDisplay1 = "\ESC[1J"
        ==> [ANSIAction [1] 'J']

testEraseDisplay2 = "\ESC[2J"
        ==> [ANSIAction [2] 'J']

testEraseLine0 = "\ESC[K"
        ==> [ANSIAction [] 'K']

testEraseLine1 = "\ESC[1K"
        ==> [ANSIAction [1] 'K']

testEraseLine2 = "\ESC[2K"
        ==> [ANSIAction [2] 'K']

-- ── 文字操作 ────────────────────────────────────────────

testDCH = "\ESC[P"
        ==> [ANSIAction [] 'P']

testDCHn = "\ESC[3P"
        ==> [ANSIAction [3] 'P']

testICH = "\ESC[@"
        ==> [ANSIAction [] '@']

testICHn = "\ESC[4@"
        ==> [ANSIAction [4] '@']

testECH = "\ESC[X"
        ==> [ANSIAction [] 'X']

testECHn = "\ESC[5X"
        ==> [ANSIAction [5] 'X']

-- ── 行操作 ──────────────────────────────────────────────

testDL = "\ESC[M"
        ==> [ANSIAction [] 'M']

testDLn = "\ESC[3M"
        ==> [ANSIAction [3] 'M']

testIL = "\ESC[L"
        ==> [ANSIAction [] 'L']

testILn = "\ESC[2L"
        ==> [ANSIAction [2] 'L']

-- ── モード設定 ──────────────────────────────────────────

testShowCursor = "\ESC[?25h\ESC[?25l"
        ==> [ShowCursor True, ShowCursor False]

testPrivateMode = "\ESC[?1h\ESC[?1l"
        ==> [DECAction [1] 'h', DECAction [1] 'l']

testAltScreen = "\ESC[?1049h\ESC[?1049l"
        ==> [DECAction [1049] 'h', DECAction [1049] 'l']

-- ── ESC シーケンス ──────────────────────────────────────

testSaveCursor = "\ESC7A"
        ==> [Ignored, CharInput 'A']

testRestoreCursor = "\ESC8B"
        ==> [Ignored, CharInput 'B']

-- ★ ESC 7 の後の CSI シーケンスが食われないことを確認
-- (以前のバグ: catch-all が次の ESC を消費していた)
testSaveCursorPreservesNext = "\ESC7\ESC[24;1H"
        ==> [Ignored, SetCursor 24 1]

testRestoreCursorPreservesNext = "\ESC8\ESC[1m"
        ==> [Ignored, SetAttributeMode [Bright]]

testKeypadMode = "\ESC=\ESC>"
        ==> [KeypadKeysApplicationsMode, KeypadKeysNumericMode]

testDesignateCharset = "\ESC(B"
        ==> [Ignored]

-- ── 256色・TrueColor ───────────────────────────────────

test256ColorForeground = "\ESC[38;5;196m"
        ==> [SetAttributeMode [Foreground (Color256 196)]]

test256ColorBackground = "\ESC[48;5;82m"
        ==> [SetAttributeMode [Background (Color256 82)]]

test256ColorBoth = "\ESC[38;5;196;48;5;232m"
        ==> [SetAttributeMode [Foreground (Color256 196), Background (Color256 232)]]

testTrueColorForeground = "\ESC[38;2;255;128;0m"
        ==> [SetAttributeMode [Foreground (ColorRGB 255 128 0)]]

testTrueColorBackground = "\ESC[48;2;0;64;128m"
        ==> [SetAttributeMode [Background (ColorRGB 0 64 128)]]

testTrueColorWithAttrs = "\ESC[1;38;2;255;0;0;4m"
        ==> [SetAttributeMode [Bright, Foreground (ColorRGB 255 0 0), Underlined]]

testBrightForeground = "\ESC[91m"
        ==> [SetAttributeMode [Foreground (Color256 9)]]

testBrightBackground = "\ESC[100m"
        ==> [SetAttributeMode [Background (Color256 8)]]

testBrightFgBg = "\ESC[97;104m"
        ==> [SetAttributeMode [Foreground (Color256 15), Background (Color256 12)]]

testResetColors = "\ESC[39;49m"
        ==> [SetAttributeMode [ResetForeground, ResetBackground]]

-- ── 複合シーケンス（実際のアプリ出力に近いもの） ────────

-- htop のような出力: スクロール領域設定 → 色設定 → 文字出力 → カーソル移動
testCompositeHtopLike =
    "\ESC[1;23r\ESC[24;1H\ESC[7m F1\ESC[0mHelp"
    ==> [ SetScrollingRegion 1 23
        , SetCursor 24 1
        , SetAttributeMode [Inverse]
        , CharInput ' '
        , CharInput 'F'
        , CharInput '1'
        , SetAttributeMode [ResetAllAttributes]
        , CharInput 'H'
        , CharInput 'e'
        , CharInput 'l'
        , CharInput 'p'
        ]

-- ncurses: save cursor → draw → restore cursor (ESC 7/8 がパーサーを壊さない)
testCompositeNcursesSaveRestore =
    "\ESC7\ESC[1;1Hstatus\ESC8"
    ==> [ Ignored              -- ESC 7 (save cursor)
        , SetCursor 1 1
        , CharInput 's', CharInput 't', CharInput 'a', CharInput 't', CharInput 'u', CharInput 's'
        , Ignored              -- ESC 8 (restore cursor)
        ]

-- ── ユニットテストリスト ────────────────────────────────

unitTests =
       -- カーソル移動
       [ testCase "testSetCursor" testSetCursor
       , testCase "testInvalidSetCursor" testInvalidSetCursor
       , testCase "testMoveCursor" testMoveCursor
       , testCase "testCursorForward" testCursorForward
       , testCase "testCursorBackward" testCursorBackward
       , testCase "testCursorAbsoluteColumn" testCursorAbsoluteColumn
       , testCase "testCursorAbsoluteRow" testCursorAbsoluteRow
       , testCase "testCursorHome" testCursorHome
       , testCase "testCursorPosition" testCursorPosition
       -- スクロール
       , testCase "testScrolling" testScrolling
       , testCase "testScrollRegion" testScrollRegion
       , testCase "testScrollRegionReset" testScrollRegionReset
       -- 文字入力
       , testCase "testCharInput" testCharInput
       , testCase "testCharInputIg" testCharInputIg
       -- SGR
       , testCase "testSetDisplayAttributes1" testSetDisplayAttributes1
       , testCase "testSetDisplayAttributes2" testSetDisplayAttributes2
       , testCase "testSetDisplayAttributes3" testSetDisplayAttributes3
       , testCase "testSetDisplayAttributes4" testSetDisplayAttributes4
       , testCase "testSetDisplayAttributes5" testSetDisplayAttributes5
       , testCase "testSetTerminalTitle" testSetTerminalTitle
       -- 消去
       , testCase "testEraseDisplay0" testEraseDisplay0
       , testCase "testEraseDisplay1" testEraseDisplay1
       , testCase "testEraseDisplay2" testEraseDisplay2
       , testCase "testEraseLine0" testEraseLine0
       , testCase "testEraseLine1" testEraseLine1
       , testCase "testEraseLine2" testEraseLine2
       -- 文字操作
       , testCase "testDCH" testDCH
       , testCase "testDCHn" testDCHn
       , testCase "testICH" testICH
       , testCase "testICHn" testICHn
       , testCase "testECH" testECH
       , testCase "testECHn" testECHn
       -- 行操作
       , testCase "testDL" testDL
       , testCase "testDLn" testDLn
       , testCase "testIL" testIL
       , testCase "testILn" testILn
       -- モード
       , testCase "testShowCursor" testShowCursor
       , testCase "testPrivateMode" testPrivateMode
       , testCase "testAltScreen" testAltScreen
       -- ESC シーケンス
       , testCase "testSaveCursor" testSaveCursor
       , testCase "testRestoreCursor" testRestoreCursor
       , testCase "testSaveCursorPreservesNext" testSaveCursorPreservesNext
       , testCase "testRestoreCursorPreservesNext" testRestoreCursorPreservesNext
       , testCase "testKeypadMode" testKeypadMode
       , testCase "testDesignateCharset" testDesignateCharset
       -- 色
       , testCase "testResetColors" testResetColors
       , testCase "test256ColorForeground" test256ColorForeground
       , testCase "test256ColorBackground" test256ColorBackground
       , testCase "test256ColorBoth" test256ColorBoth
       , testCase "testTrueColorForeground" testTrueColorForeground
       , testCase "testTrueColorBackground" testTrueColorBackground
       , testCase "testTrueColorWithAttrs" testTrueColorWithAttrs
       , testCase "testBrightForeground" testBrightForeground
       , testCase "testBrightBackground" testBrightBackground
       , testCase "testBrightFgBg" testBrightFgBg
       -- 複合
       , testCase "testCompositeHtopLike" testCompositeHtopLike
       , testCase "testCompositeNcursesSaveRestore" testCompositeNcursesSaveRestore
       ]

-- ── プロパティテスト ────────────────────────────────────

newtype InputStream = InputStream String deriving Show

instance Arbitrary InputStream where
    arbitrary = InputStream . concat <$> (listOf1 $ oneof [
        return "\ESC[",
        return ";",
        listOf1 $ choose ('a', 'z'),
        show <$> (choose (0, 100) :: Gen Int),
        listOf1 $ choose ('\x00', '\xFF')
        ])

prop_NotManyCharsLeftOver (InputStream str) =
    let Right (_, s) = parseANSI str in
    -- 未消費の残りは入力の半分以下、ただし短い入力は許容
    length s <= max 20 (length str `div` 2)

propertyTests =
       [ testProperty "parseNotManyCharsLeftOver" prop_NotManyCharsLeftOver
       ]

main :: IO ()
main = defaultMainWithOpts (unitTests ++ propertyTests) mempty

