module Main where
import Data.Monoid
import Control.Applicative ((<$>))

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

import Data.Array.IArray ((!))
import Data.List (foldl')

import Terminal.Parser
import Terminal.Terminal
import Terminal.Types

-- ── ヘルパー ────────────────────────────────────────────

handleActions [] t = t
handleActions (a : as) t = handleActions as (applyAction t a)

-- | アクションリストをデフォルトターミナルに適用
applyDef actions = handleActions actions defaultTerm

-- | 生の ANSI 文字列をデフォルト 24x80 ターミナルに適用
feedStr :: String -> Terminal
feedStr s = let Right (actions, _) = parseANSI s
            in  foldl' applyAction defaultTerm actions

-- | 既存ターミナルに生の ANSI 文字列を追加入力
feedStrTo :: Terminal -> String -> Terminal
feedStrTo t s = let Right (actions, _) = parseANSI s
                in  foldl' applyAction t actions

-- | 指定位置の文字を取得
charAt :: Terminal -> (Int, Int) -> Char
charAt t pos = character (screen t ! pos)

-- | 指定行の文字列を取得（列 c1 から c2 まで）
rowStr :: Terminal -> Int -> Int -> Int -> String
rowStr t row c1 c2 = [character (screen t ! (row, col)) | col <- [c1..c2]]

-- ── 既存テスト（色と属性） ──────────────────────────────

testColors :: Assertion
testColors = let term = applyDef [CharInput 'a',
                                  SetAttributeMode [Foreground Green],
                                  CharInput 'b',
                                  SetAttributeMode [Background Yellow, Blinking]
                                 ] in
             [currentForeground term, currentBackground term] @?= [Green, Yellow]

testBackgroundDel = TestCase (do
                    let term = applyDef [
                            SetAttributeMode [Background Yellow],
                            ANSIAction [2] 'J',
                            SetAttributeMode [Background White],
                            CharInput 'a',
                            SetAttributeMode [Background Green],
                            ANSIAction [] 'K'
                            ]
                    assertEqual "background color in the middle is yellow"
                        (backgroundColor $ (screen term) ! (10, 10)) (Yellow)
                    assertEqual "background color in the middle is yellow"
                        (backgroundColor $ (screen term) ! (10, 1)) (Yellow)
                    assertEqual "background color is white at (1,1)"
                        (backgroundColor $ (screen term) ! (1, 1)) (White)
                    assertEqual "background color is green till the end of line"
                        (backgroundColor $ (screen term) ! (1, 2)) (Green)
                    assertEqual "background color is green till the end of line"
                        (backgroundColor $ (screen term) ! (1, 50)) (Green))

testColors2 = TestCase (do
                let term = applyDef [SetAttributeMode [Foreground Green],
                                   SetAttributeMode [Background Magenta],
                                   CharInput 'H',
                                   SetAttributeMode [Background White],
                                   CharInput 'i',
                                   CharInput '\n',
                                   SetAttributeMode [Foreground Yellow, Blinking],
                                   CharInput 'X',
                                   SetAttributeMode  [ResetAllAttributes],
                                   CharInput '\n',
                                   CharInput 'Y',
                                   CharInput '\r'
                                  ]
                assertEqual "cursor is at the beginning of third row"
                    (cursorPos term) (3, 1)
                assertEqual "foreground color is correct in first char"
                    (foregroundColor $ (screen term) ! (1, 1)) (Green)
                assertEqual "background color is correct in first char"
                    (backgroundColor $ (screen term) ! (1, 1)) (Magenta)
                assertEqual "background color is correct in second char"
                    (backgroundColor $ (screen term) ! (1, 2)) (White)
                assertEqual "foreground color is correct in second row"
                    (foregroundColor $ (screen term) ! (2, 1)) (Yellow)
                assertEqual "background color is default in third row"
                    (backgroundColor $ (screen term) ! (3, 1)) (Black)
                )

testSetTerminalTitle = TestCase (do
                let s = "There is always money in the banana stand"
                    term = applyDef [SetTerminalTitle "x", CharInput 'a', SetTerminalTitle s]
                assertEqual "title is set"
                    (terminalTitle term) s)

testTabCharacter = TestCase (do
                let term = applyDef [CharInput '\t', CharInput 'i']
                assertEqual "cursor is at (1, 9)" (cursorPos term) (1, 9))

testClearSreen = TestCase (do
                let term = applyDef [CharInput 'H',
                                     CharInput 'i',
                                     CharInput '\n',
                                     ANSIAction [2] 'J'
                                    ]
                assertEqual "cursor is at the beginning of first row"
                    (cursorPos term) (1, 1)
                assertEqual "first char in first row is empty"
                    (character $ screen term ! (1, 1)) ' ')

testColorsDoScroll = TestCase (do
                let term = applyDef ([
                                   SetAttributeMode [Foreground Yellow, Background Green],
                                   CharInput 'X',
                                   SetAttributeMode  [ResetAllAttributes]
                                   ] ++ (take 50 $ repeat (CharInput '\n')))
                assertEqual "cursor is at the beginning of last row"
                    (cursorPos term) (rows term, 1)
                assertEqual "background color is default in (1, 1)"
                    (backgroundColor $ (screen term) ! (1, 1)) Black
                assertEqual "foreground color is default in (1, 1)"
                    (foregroundColor $ (screen term) ! (1, 1)) White
                )

testColorsBright = TestCase (do
                let term = applyDef ([
                                   SetAttributeMode [Foreground Yellow, Bright],
                                   CharInput '$', CharInput '\n',
                                   SetAttributeMode [Foreground Yellow, Underlined],
                                   CharInput '^', CharInput '\n',
                                   SetAttributeMode [ResetAllAttributes],
                                   CharInput 'o'
                                   ])
                assertEqual "cursor is at the second position in the second row"
                    (cursorPos term) (3, 2)
                assertEqual "character is bright (1, 1)"
                    (isBright $ (screen term) ! (1, 1)) True
                assertEqual "character is bright (2, 1)"
                    (isBright $ (screen term) ! (2, 1)) True
                assertEqual "character is not bright (3, 1)"
                    (isBright $ (screen term) ! (3, 1)) False
                )

-- ── DCH (CSI P): 文字削除 ──────────────────────────────

testDCHBasic = TestCase (do
    let term = feedStr "ABCDE\ESC[1;2H\ESC[P"
    assertEqual "char at (1,2) shifted left" (charAt term (1,2)) 'C'
    assertEqual "char at (1,3) shifted left" (charAt term (1,3)) 'D'
    assertEqual "char at (1,4) shifted left" (charAt term (1,4)) 'E'
    assertEqual "char at (1,5) is blank" (charAt term (1,5)) ' '
    )

testDCHMultiple = TestCase (do
    let term = feedStr "ABCDE\ESC[1;2H\ESC[2P"
    assertEqual "char at (1,2) is D" (charAt term (1,2)) 'D'
    assertEqual "char at (1,3) is E" (charAt term (1,3)) 'E'
    assertEqual "char at (1,4) is blank" (charAt term (1,4)) ' '
    assertEqual "char at (1,5) is blank" (charAt term (1,5)) ' '
    )

testDCHAtEnd = TestCase (do
    let term = feedStr "AB\ESC[1;2H\ESC[P"
    assertEqual "char at (1,1) unchanged" (charAt term (1,1)) 'A'
    assertEqual "char at (1,2) is blank" (charAt term (1,2)) ' '
    )

-- ── ICH (CSI @): 文字挿入 ──────────────────────────────

testICHBasic = TestCase (do
    let term = feedStr "ABCDE\ESC[1;2H\ESC[@"
    assertEqual "char at (1,1) unchanged" (charAt term (1,1)) 'A'
    assertEqual "char at (1,2) is blank (inserted)" (charAt term (1,2)) ' '
    assertEqual "char at (1,3) is B (shifted)" (charAt term (1,3)) 'B'
    assertEqual "char at (1,4) is C (shifted)" (charAt term (1,4)) 'C'
    )

testICHMultiple = TestCase (do
    let term = feedStr "ABCDE\ESC[1;2H\ESC[3@"
    assertEqual "char at (1,2) blank" (charAt term (1,2)) ' '
    assertEqual "char at (1,3) blank" (charAt term (1,3)) ' '
    assertEqual "char at (1,4) blank" (charAt term (1,4)) ' '
    assertEqual "char at (1,5) is B" (charAt term (1,5)) 'B'
    )

-- ── ECH (CSI X): 文字消去 ──────────────────────────────

testECHBasic = TestCase (do
    let term = feedStr "ABCDE\ESC[1;2H\ESC[X"
    assertEqual "char at (1,1) unchanged" (charAt term (1,1)) 'A'
    assertEqual "char at (1,2) erased" (charAt term (1,2)) ' '
    assertEqual "char at (1,3) unchanged" (charAt term (1,3)) 'C'
    )

testECHMultiple = TestCase (do
    let term = feedStr "ABCDE\ESC[1;2H\ESC[3X"
    assertEqual "char at (1,1) unchanged" (charAt term (1,1)) 'A'
    assertEqual "char at (1,2) erased" (charAt term (1,2)) ' '
    assertEqual "char at (1,3) erased" (charAt term (1,3)) ' '
    assertEqual "char at (1,4) erased" (charAt term (1,4)) ' '
    assertEqual "char at (1,5) unchanged" (charAt term (1,5)) 'E'
    )

-- ── DL (CSI M): 行削除 ─────────────────────────────────

testDLBasic = TestCase (do
    let term = feedStr "\ESC[1;1HRow1\ESC[2;1HRow2\ESC[3;1HRow3\ESC[2;1H\ESC[M"
    assertEqual "row 2 now has Row3's content" (rowStr term 2 1 4) "Row3"
    assertEqual "row 3 is blank" (rowStr term 3 1 4) "    "
    )

testDLWithScrollRegion = TestCase (do
    let term = feedStr "\ESC[1;1HLine1\ESC[2;1HLine2\ESC[3;1HLine3\ESC[4;1HLine4\ESC[1;3r\ESC[2;1H\ESC[M"
    -- スクロール領域 (1,3) 内で行2を削除: 行3が上へ、行3が空白に
    -- 行4はスクロール領域外なので不変
    assertEqual "row 1 unchanged" (rowStr term 1 1 5) "Line1"
    assertEqual "row 2 now Line3" (rowStr term 2 1 5) "Line3"
    assertEqual "row 3 blank" (rowStr term 3 1 5) "     "
    assertEqual "row 4 outside region, unchanged" (rowStr term 4 1 5) "Line4"
    )

-- ── IL (CSI L): 行挿入 ─────────────────────────────────

testILBasic = TestCase (do
    let term = feedStr "\ESC[1;1HRow1\ESC[2;1HRow2\ESC[3;1HRow3\ESC[2;1H\ESC[L"
    assertEqual "row 1 unchanged" (rowStr term 1 1 4) "Row1"
    assertEqual "row 2 is blank (inserted)" (rowStr term 2 1 4) "    "
    assertEqual "row 3 has old Row2" (rowStr term 3 1 4) "Row2"
    )

testILWithScrollRegion = TestCase (do
    let term = feedStr "\ESC[1;1HLine1\ESC[2;1HLine2\ESC[3;1HLine3\ESC[4;1HLine4\ESC[1;3r\ESC[2;1H\ESC[L"
    assertEqual "row 1 unchanged" (rowStr term 1 1 5) "Line1"
    assertEqual "row 2 blank (inserted)" (rowStr term 2 1 5) "     "
    assertEqual "row 3 has old Line2" (rowStr term 3 1 5) "Line2"
    -- Line3 はスクロール領域の底からはみ出して消える
    assertEqual "row 4 outside region, unchanged" (rowStr term 4 1 5) "Line4"
    )

-- ── ED (CSI J): 画面消去 ────────────────────────────────

testED0 = TestCase (do
    let term = feedStr "\ESC[2J\ESC[1;1HABCDE\ESC[2;1HFGHIJ\ESC[1;3H\ESC[J"
    -- カーソル (1,3) 以降を消去、(1,1) と (1,2) は保存
    assertEqual "char at (1,1) preserved" 'A' (charAt term (1,1))
    assertEqual "char at (1,2) preserved" 'B' (charAt term (1,2))
    assertEqual "char at (1,3) erased" ' ' (charAt term (1,3))
    assertEqual "char at (1,5) erased" ' ' (charAt term (1,5))
    assertEqual "row 2 erased" ' ' (charAt term (2,1))
    )

testED1 = TestCase (do
    let term = feedStr "ABCDE\ESC[2;1HFGHIJ\ESC[2;3H\ESC[1J"
    -- カーソル (2,3) までを消去、行1は全消去、行2は列3まで消去
    assertEqual "row 1 erased" ' ' (charAt term (1,1))
    assertEqual "row 2 col 1 erased" ' ' (charAt term (2,1))
    assertEqual "row 2 col 3 erased" ' ' (charAt term (2,3))
    assertEqual "row 2 col 4 preserved" 'I' (charAt term (2,4))
    assertEqual "row 2 col 5 preserved" 'J' (charAt term (2,5))
    )

testED2 = TestCase (do
    let term = feedStr "ABCDE\ESC[2;1HFGHIJ\ESC[2J"
    assertEqual "screen cleared" (charAt term (1,1)) ' '
    assertEqual "screen cleared" (charAt term (2,1)) ' '
    assertEqual "cursor at home" (cursorPos term) (1, 1)
    )

-- ── EL (CSI K): 行消去 ─────────────────────────────────

testEL0 = TestCase (do
    let term = feedStr "ABCDE\ESC[1;3H\ESC[K"
    assertEqual "char preserved" (charAt term (1,1)) 'A'
    assertEqual "char preserved" (charAt term (1,2)) 'B'
    assertEqual "char erased" (charAt term (1,3)) ' '
    assertEqual "char erased" (charAt term (1,5)) ' '
    )

testEL1 = TestCase (do
    let term = feedStr "ABCDE\ESC[1;3H\ESC[1K"
    assertEqual "char erased" (charAt term (1,1)) ' '
    assertEqual "char erased" (charAt term (1,2)) ' '
    assertEqual "char erased" (charAt term (1,3)) ' '
    assertEqual "char preserved" (charAt term (1,4)) 'D'
    assertEqual "char preserved" (charAt term (1,5)) 'E'
    )

testEL2 = TestCase (do
    let term = feedStr "ABCDE\ESC[1;3H\ESC[2K"
    assertEqual "entire line erased" (charAt term (1,1)) ' '
    assertEqual "entire line erased" (charAt term (1,5)) ' '
    )

-- ── スクロール領域 ─────────────────────────────────────

testScrollRegionSet = TestCase (do
    let term = feedStr "\ESC[5;20r"
    assertEqual "scrolling region set" (scrollingRegion term) (5, 20)
    )

testScrollRegionResetTerm = TestCase (do
    let term = feedStr "\ESC[5;20r\ESC[r"
    assertEqual "scrolling region reset to full screen" (scrollingRegion term) (1, rows term)
    )

testScrollWithinRegion = TestCase (do
    let term = feedStr "\ESC[1;1HHead\ESC[2;1HLine1\ESC[3;1HLine2\ESC[4;1HFoot\ESC[2;3r\ESC[3;1H\ESC[S"
    -- スクロール領域 (2,3) 内で上スクロール: 領域内だけ影響
    assertEqual "row 1 outside region, unchanged" (rowStr term 1 1 4) "Head"
    assertEqual "row 4 outside region, unchanged" (rowStr term 4 1 4) "Foot"
    )

-- ── LF: スクロール領域外でのスクロール防止 ─────────────

testLFAtBottomOutsideRegion = TestCase (do
    -- htop シナリオ: スクロール領域 (1,23)、カーソルが行24（メニュー行）
    let term = feedStr "\ESC[1;23r\ESC[24;1HMenu\ESC[24;5H"
    -- 行24で LF を送信
    let term2 = feedStrTo term "\n"
    assertEqual "cursor stays at bottom row" (fst (cursorPos term2)) 24
    assertEqual "menu text preserved" (rowStr term2 24 1 4) "Menu"
    )

testLFAtBottomDoesNotScroll = TestCase (do
    -- スクロール領域外の最終行で LF → スクロール領域内のコンテンツが影響されない
    let term = feedStr "\ESC[1;23r\ESC[1;1HTop\ESC[23;1HBottom\ESC[24;1HStatus"
    let term2 = feedStrTo term "\n"
    assertEqual "row 1 unchanged" (rowStr term2 1 1 3) "Top"
    assertEqual "row 23 unchanged" (rowStr term2 23 1 6) "Bottom"
    assertEqual "row 24 (status) preserved" (rowStr term2 24 1 6) "Status"
    )

-- ── モード設定が副作用なし ─────────────────────────────

testModeSetNoSideEffects = TestCase (do
    let term = feedStr "ABCDE\ESC[1;3H\ESC[?1h"
    assertEqual "char preserved after mode set" (charAt term (1,1)) 'A'
    assertEqual "char preserved after mode set" (charAt term (1,3)) 'C'
    assertEqual "char preserved after mode set" (charAt term (1,5)) 'E'
    )

testModeResetNoSideEffects = TestCase (do
    let term = feedStr "ABCDE\ESC[1;3H\ESC[?1l"
    assertEqual "char preserved after mode reset" (charAt term (1,1)) 'A'
    assertEqual "char preserved after mode reset" (charAt term (1,5)) 'E'
    )

-- ── カーソル表示切替 ───────────────────────────────────

testShowCursorToggle = TestCase (do
    let term = feedStr "\ESC[?25l"
    assertEqual "cursor hidden" (optionShowCursor term) False
    let term2 = feedStrTo term "\ESC[?25h"
    assertEqual "cursor shown" (optionShowCursor term2) True
    )

-- ── カーソル移動 ────────────────────────────────────────

testCursorAbsoluteColumn = TestCase (do
    let term = feedStr "ABCDE\ESC[10G"
    assertEqual "cursor at column 10" (cursorPos term) (1, 10)
    )

testCursorAbsoluteRow = TestCase (do
    let term = feedStr "\ESC[5d"
    assertEqual "cursor at row 5" (cursorPos term) (5, 1)
    )

testCursorClampBounds = TestCase (do
    let term = feedStr "\ESC[999;999H"
    -- safeCursor clamps to bottom-right corner (no wrap on cursor movement)
    assertEqual "cursor clamped to bottom-right"
        (24, 80) (cursorPos term)
    )
-- ── htop メニューバーシナリオ ──────────────────────────

testHtopMenuBarPreserved = TestCase (do
    -- htop の典型的な動作をシミュレート
    let term = feedStr $
            -- 1. スクロール領域設定（最終行を除く）
            "\ESC[1;23r"
            -- 2. プロセスリスト描画（行1-23を埋める）
            ++ concat ["\ESC[" ++ show i ++ ";1H" ++ "Process " ++ show i | i <- [1..23]]
            -- 3. メニューバー描画（行24、反転表示）
            ++ "\ESC[24;1H\ESC[7m F1 \ESC[0mHelp \ESC[7m F2 \ESC[0mSetup"
    -- 4. スクロール領域内でスクロール
    let term2 = feedStrTo term "\ESC[23;1H\n"
    -- メニューバーが保存されていることを確認
    -- メニュー: " F1 Help  F2 Setup"
    -- 位置:   1234 56789...
    assertEqual "menu bar F preserved" 'F' (charAt term2 (24, 2))
    assertEqual "menu bar 1 preserved" '1' (charAt term2 (24, 3))
    assertEqual "menu bar H preserved" 'H' (charAt term2 (24, 5))
    assertEqual "menu bar e preserved" 'e' (charAt term2 (24, 6))
    )

-- ── ESC 7/8 がパーサーを壊さない ──────────────────────

testESC78DoesNotMangleParser = TestCase (do
    -- ESC 7 → CSI 24;1H → 文字描画 → ESC 8
    -- 以前のバグ: ESC 7 の catch-all が次の ESC を食って CSI を壊していた
    let term = feedStr "\ESC7\ESC[24;1HMenu\ESC8"
    assertEqual "Menu drawn at row 24" (rowStr term 24 1 4) "Menu"
    )

-- ── プロパティテスト ────────────────────────────────────

instance Arbitrary TerminalAction where
    arbitrary = oneof [
        CharInput <$> choose ('a', 'Z'),
        CursorUp <$> choose (1,50),
        CursorDown <$> choose (1,50),
        CursorForward <$> choose (1,50),
        CursorBackward <$> choose (1,50),
        SetCursor 34 <$> choose (1, 112)
        ]

-- | カーソルは常に画面内に収まる
prop_SafeCursor a =
    let t = (handleActions a defaultTerm)
        (y, x) = cursorPos t in
    x >= 1 && y >= 1 && x <= cols t && y <= rows t

-- ── テスト集約 ──────────────────────────────────────────

main :: IO ()
main = defaultMainWithOpts (
        concat (hUnitTestToTests <$> hUnitTests)
        ++
       [ testCase "testColors" testColors
       , testProperty "safeCursor" prop_SafeCursor
       ]) mempty
       where hUnitTests =
               [ testColors2, testClearSreen, testColorsDoScroll
               , testSetTerminalTitle, testTabCharacter
               , testBackgroundDel, testColorsBright
               -- DCH
               , testDCHBasic, testDCHMultiple, testDCHAtEnd
               -- ICH
               , testICHBasic, testICHMultiple
               -- ECH
               , testECHBasic, testECHMultiple
               -- DL
               , testDLBasic, testDLWithScrollRegion
               -- IL
               , testILBasic, testILWithScrollRegion
               -- ED
               , testED0, testED1, testED2
               -- EL
               , testEL0, testEL1, testEL2
               -- スクロール領域
               , testScrollRegionSet, testScrollRegionResetTerm
               , testScrollWithinRegion
               -- LF 挙動
               , testLFAtBottomOutsideRegion, testLFAtBottomDoesNotScroll
               -- モード
               , testModeSetNoSideEffects, testModeResetNoSideEffects
               , testShowCursorToggle
               -- カーソル
               , testCursorAbsoluteColumn, testCursorAbsoluteRow
               , testCursorClampBounds
               -- htop シナリオ
               , testHtopMenuBarPreserved, testESC78DoesNotMangleParser
               ]
