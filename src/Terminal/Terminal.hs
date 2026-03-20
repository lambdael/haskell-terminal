{-# LANGUAGE Rank2Types #-}
-- | ターミナル状態マシン。
--
-- 'TerminalAction' を 'Terminal' に適用して状態を遷移させる純粋関数を提供する。
-- カーソル移動、スクロール、文字書き込み、属性変更、画面消去、
-- リサイズなどのターミナル操作を実装する。
module Terminal.Terminal (newTerminal, defaultTerm, applyAction, applyActionsBatched, testTerm, scrollTerminalDown, scrollTerminalUp, setSize) where
import System.Process
import Data.Array
import Data.Array.ST (STArray, thaw, freeze, readArray, writeArray)
import Data.Array.Unsafe (unsafeFreeze)
import Data.Char
import Data.List (foldl')
import Control.Monad
import Control.Monad.ST (ST, runST)
import Control.Monad.State hiding (state)
import Data.Maybe (isNothing)
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import System.IO
import System.Posix.IO
import System.Posix.Terminal hiding (TerminalState)
import GHC.IO.Handle
import Debug.Trace
import Control.Concurrent
import Control.Applicative hiding (many)

import Terminal.Parser
import Terminal.Types

-- | デフォルトの前景色。
defaultForegroundColor = White
-- | デフォルトの背景色。
defaultBackgroundColor = Black

-- | 指定した文字と現在のターミナル属性から 'TerminalChar' を作成する。
mkChar c term = TerminalChar {
                    character = c,
                    foregroundColor = currentForeground term,
                    backgroundColor = currentBackground term,
                    isBright = optionBright term,
                    isBlinking = optionBlinking term,
                    isUnderlined = optionUnderlined term,
                    isInverse = optionInverse term
                }
-- | 空白文字の 'TerminalChar' を作成する。
mkEmptyChar = mkChar ' '

-- | テスト用のターミナル。'defaultTerm' と同じ。
testTerm = defaultTerm

-- | 24行×80列のデフォルトターミナル（terminfo なし）。
defaultTerm = newTerminal (24, 80) Nothing

-- | ターミナルを新しいサイズに変更する。
--
-- 旧画面から新画面へセルを直接コピーする。
-- 重なる範囲だけコピーし、新しい部分は空白で埋める。
setSize :: (Int, Int) -> Terminal -> Terminal
setSize s@(r, c) term = let
  newterm = (newTerminal s $ terminfo term) {
    currentForeground = currentForeground term,
    currentBackground = currentBackground term,
    optionShowCursor = optionShowCursor term,
    optionBright = optionBright term,
    optionUnderlined = optionUnderlined term,
    optionBlinking = optionBlinking term,
    optionInverse = optionInverse term,
    terminalTitle = terminalTitle term,
    mouseMode = mouseMode term,
    mouseEncoding = mouseEncoding term,
    pendingWrap = False,
    scrollbackBuffer = scrollbackBuffer term,
    scrollbackMax = scrollbackMax term,
    altScreen = fmap resizeAltScreen (altScreen term),
    savedCursor = savedCursor term
  }
  or_ = rows term
  oc = cols term
  -- 旧画面の重なる範囲をコピー
  overlap = [ ((y, x), screen term ! (y, x))
            | y <- [1..min r or_]
            , x <- [1..min c oc]
            ]
  -- カーソルは新しい範囲内にクランプ
  (cy, cx) = cursorPos term
  newCursor = (min r (max 1 cy), min c (max 1 cx))
  in newterm {
    screen = screen newterm // overlap,
    cursorPos = newCursor
  }
  where
    resizeAltScreen as =
      let oldScr = asScreen as
          (_, (oldR, oldC)) = bounds oldScr
          blankScr = array ((1,1), (r,c))
                       [((y_,x_), blankChar) | y_ <- [1..r], x_ <- [1..c]]
          ovlp = [((y_,x_), oldScr ! (y_,x_))
                 | y_ <- [1..min r oldR], x_ <- [1..min c oldC]]
          (cy_, cx_) = asCursorPos as
      in as { asScreen = blankScr // ovlp
            , asCursorPos = (min r (max 1 cy_), min c (max 1 cx_))
            , asScrollRegion = (1, r)
            }
    blankChar = TerminalChar ' ' defaultForegroundColor defaultBackgroundColor False False False False
  -- ixmap ((1::Int,1::Int) , (r, c)) f s
  -- where f (y, x) = let
  --         (_ , (oldr, oldc) ) = bounds s
  --         total = y * oldc + x
                            --         in (total `quot` c, total `rem` c)
          
-- | 指定したサイズの新しいターミナルを作成する。
--
-- @newTerminal (rows, cols) mterm@ で、@rows@ 行 @cols@ 列の空白ターミナルを生成する。
-- GUI 使用時は @mterm = Just terminfo@ 。テストやリプレイ時は @Nothing@ 。
newTerminal s@(rows, cols) mterm = Terminal {
    cursorPos = (1, 1),
    rows = rows,
    cols = cols,
    inBuffer = "",
    responseBuffer = "",
    scrollingRegion = (1, rows),
    screen = array ((1, 1), s)
        [((y, x), e) | x <- [1..cols], y <- [1..rows]],
    currentForeground = defaultForegroundColor,
    currentBackground = defaultBackgroundColor,
    optionShowCursor = True,
    terminalTitle = "",
    terminfo = mterm,
    optionBright = False,
    optionUnderlined = False,
    optionBlinking = False,
    optionInverse = False,
    mouseMode = MouseNone,
    mouseEncoding = MouseEncodingX10,
    pendingWrap = False,
    scrollbackBuffer = Seq.empty,
    scrollbackMax = 10000,
    altScreen = Nothing,
    savedCursor = Nothing
} where e = mkEmptyChar (newTerminal s mterm) -- Hail laziness

up t@Terminal {cursorPos = (y, x)} = safeCursor $ t { cursorPos = (y - 1, x), pendingWrap = False }
down t@Terminal {cursorPos = (y, x)} = safeCursor $ t { cursorPos = (y + 1, x), pendingWrap = False }
left t@Terminal {cursorPos = (y, x)} = safeCursor $ t { cursorPos = (y, x - 1), pendingWrap = False }
right t@Terminal {cursorPos = (y, x)} = safeCursor $ t { cursorPos = (y, x + 1), pendingWrap = False }

-- | カーソル位置を画面範囲内にクランプする。
-- 折り返しは行わない（deferred wrap は CharInput で処理）。
safeCursor t@Terminal {cursorPos = (y, x), cols = c , rows = r } =
  t { cursorPos = (min r (max 1 y), min c (max 1 x)) }


scrollIndexUp :: (Int, Int) -> ScreenIndex -> ScreenIndex
scrollIndexUp (startrow, endrow) (y, x)
    | y > startrow && y <= endrow  = (y - 1, x)
    | y == startrow               = (endrow, x)
    | otherwise                   = (y, x)

scrollIndexDown :: (Int, Int) -> ScreenIndex -> ScreenIndex
scrollIndexDown (startrow, endrow) (y, x)
    | y >= startrow && y < endrow  = (y + 1, x)
    | y == endrow                  = (startrow, x)
    | otherwise                   = (y, x)

scrollScreenUp r@(startrow, endrow) screen =
  let (first,last) = bounds screen
  in  ixmap (first, last) (scrollIndexUp r) screen

scrollScreenDown r@(startrow, endrow) screen =
  let (first,last) = bounds screen
  in    ixmap (first, last) (scrollIndexDown r) screen

scrollTerminalUp :: Terminal -> Terminal
scrollTerminalUp term@Terminal { screen = s, scrollingRegion = r@(startrow, endrow) } =
    clearRows [startrow] $ term {
        screen = scrollScreenUp (scrollingRegion term) s
    }

scrollTerminalDown :: Terminal -> Terminal
scrollTerminalDown term@Terminal { screen = s, scrollingRegion = r@(startrow, endrow) } =
    clearRows [endrow] $ term' {
        screen = scrollScreenDown r s
    }
  where
    term' = if startrow == 1 && isNothing (altScreen term)
            then pushScrollbackLine term
            else term

-- | スクロール領域の先頭行をスクロールバックに保存する。
pushScrollbackLine :: Terminal -> Terminal
pushScrollbackLine term =
  let s = screen term
      c = cols term
      topLine = V.generate c (\i -> s ! (1, i + 1))
      buf = scrollbackBuffer term Seq.|> topLine
      buf' = if Seq.length buf > scrollbackMax term
             then Seq.drop 1 buf
             else buf
  in term { scrollbackBuffer = buf' }

clearRows :: [Int] -> Terminal -> Terminal
clearRows rows term@Terminal { screen = s, rows = r, cols = c} =
    write term [((y_,x_), mkEmptyChar term)|x_<-[1..c],y_<-rows] 


clearColumns :: Int -> [Int] -> Terminal -> Terminal
clearColumns row cols term@Terminal { screen = s } =
    write term [((row,x_), mkEmptyChar term)|x_<-cols] 

-- | DCH: カーソル位置から n 文字を削除し、残りを左にシフト。
-- 右端に空白を埋める。
deleteChars :: Int -> Int -> Int -> Int -> Terminal -> Terminal
deleteChars n row cx maxCol term@Terminal { screen = s } =
    let shifted = [((row, col), s ! (row, col + n)) | col <- [cx .. maxCol - n], col + n <= maxCol]
        blanks  = [((row, col), mkEmptyChar term) | col <- [max cx (maxCol - n + 1) .. maxCol]]
    in  write term (shifted ++ blanks)

-- | ICH: カーソル位置に n 個の空白を挿入し、既存文字を右にシフト。
-- 右端からはみ出した文字は消える。
insertChars :: Int -> Int -> Int -> Int -> Terminal -> Terminal
insertChars n row cx maxCol term@Terminal { screen = s } =
    let shifted = [((row, col + n), s ! (row, col)) | col <- reverse [cx .. maxCol - n]]
        blanks  = [((row, col), mkEmptyChar term) | col <- [cx .. min (cx + n - 1) maxCol]]
    in  write term (shifted ++ blanks)

-- | DL: カーソル行から n 行を削除し、残りを上にシフト。
-- スクロール領域の下端に空白行を埋める。
deleteLines :: Int -> Terminal -> Terminal
deleteLines n term@Terminal { screen = s, scrollingRegion = (srTop, srBot), cols = maxCol } =
    let cy = fst (cursorPos term)
        shifted = [((row, col), s ! (row + n, col))
                  | row <- [cy .. srBot - n], row + n <= srBot, col <- [1..maxCol]]
        blanks  = [((row, col), mkEmptyChar term)
                  | row <- [max cy (srBot - n + 1) .. srBot], col <- [1..maxCol]]
    in  write term (shifted ++ blanks)

-- | IL: カーソル行に n 行の空白を挿入し、既存行を下にシフト。
-- スクロール領域の下端からはみ出した行は消える。
insertLines :: Int -> Terminal -> Terminal
insertLines n term@Terminal { screen = s, scrollingRegion = (srTop, srBot), cols = maxCol } =
    let cy = fst (cursorPos term)
        shifted = [((row + n, col), s ! (row, col))
                  | row <- reverse [cy .. srBot - n], col <- [1..maxCol]]
        blanks  = [((row, col), mkEmptyChar term)
                  | row <- [cy .. min (cy + n - 1) srBot], col <- [1..maxCol]]
    in  write term (shifted ++ blanks)

write :: Terminal -> [((Int,Int), TerminalChar)] -> Terminal
write term@Terminal {screen=s, rows=r, cols=c} l = 
    term {
      screen = s // [(pos, ch)| (pos, ch)<-l , isin_ pos (r,c)]
    }
isin_ (y, x) (r, c) = isin y x r c
isin y x r c = (0 < x) && (x <= c) && (0 < y) && (y <= r)






-- Attribute mode handling
applyAttributeMode :: Terminal -> AttributeMode -> Terminal
applyAttributeMode term ResetAllAttributes =
    term {
        currentForeground = defaultForegroundColor,
        currentBackground = defaultBackgroundColor,
        optionBright = False,
        optionUnderlined = False,
        optionBlinking = False,
        optionInverse = False
        
    }
applyAttributeMode term (Foreground c) = term { currentForeground = c }
applyAttributeMode term (Background c) = term { currentBackground = c }
applyAttributeMode term ResetForeground = term { currentForeground = defaultForegroundColor }
applyAttributeMode term ResetBackground = term { currentBackground = defaultBackgroundColor }
applyAttributeMode term Bright = term { optionBright = True }
applyAttributeMode term Normal = term { optionBright = False }
applyAttributeMode term Underlined = term { optionUnderlined = True }
applyAttributeMode term NotUnderlined = term { optionUnderlined = False }
applyAttributeMode term Blinking = term { optionBlinking = True }
applyAttributeMode term NotBlinking = term { optionBlinking = False }
applyAttributeMode term Inverse = term { optionInverse = True}
applyAttributeMode term NotInverse = term { optionInverse = False }

applyAttributeMode term other = trace ("\nUnimplemented attribute mode: " ++ show other) term

-- | DECSET: DEC プライベートモードの個別パラメータ適用。
applyDECSet :: Terminal -> Int -> Terminal
applyDECSet term 25   = term { optionShowCursor = True }
applyDECSet term 47   = switchToAltScreen term
applyDECSet term 1047 = switchToAltScreen term
applyDECSet term 1048 = term { savedCursor = Just (cursorPos term) }
applyDECSet term 1049 = switchToAltScreen $ term { savedCursor = Just (cursorPos term) }
applyDECSet term 1000 = term { mouseMode = MouseNormal }
applyDECSet term 1002 = term { mouseMode = MouseButton }
applyDECSet term 1003 = term { mouseMode = MouseAll }
applyDECSet term 1006 = term { mouseEncoding = MouseEncodingSGR }
applyDECSet term _    = term

-- | DECRST: DEC プライベートモードの個別パラメータリセット。
applyDECReset :: Terminal -> Int -> Terminal
applyDECReset term 25   = term { optionShowCursor = False }
applyDECReset term 47   = switchToNormalScreen term
applyDECReset term 1047 = switchToNormalScreen term
applyDECReset term 1048 = restoreSavedCursor term
applyDECReset term 1049 = restoreSavedCursor $ switchToNormalScreen term
applyDECReset term 1000 = term { mouseMode = MouseNone }
applyDECReset term 1002 = term { mouseMode = MouseNone }
applyDECReset term 1003 = term { mouseMode = MouseNone }
applyDECReset term 1006 = term { mouseEncoding = MouseEncodingX10 }
applyDECReset term _    = term

-- | 代替画面バッファに切り替える。
-- 通常画面の状態を保存し、空の画面を作成する。
switchToAltScreen :: Terminal -> Terminal
switchToAltScreen term
  | isJust_ (altScreen term) = term  -- 既に代替画面
  | otherwise =
      let saved = AltScreenState
            { asScreen = screen term
            , asCursorPos = cursorPos term
            , asScrollRegion = scrollingRegion term
            }
          r = rows term
          c = cols term
          blank = array ((1,1), (r,c))
                    [((y_,x_), e) | y_ <- [1..r], x_ <- [1..c]]
          e = TerminalChar ' ' defaultForegroundColor defaultBackgroundColor False False False False
      in term { altScreen = Just saved
              , screen = blank
              , cursorPos = (1, 1)
              , scrollingRegion = (1, r)
              , pendingWrap = False
              }

-- | 通常画面バッファに戻る。
-- 保存した画面の状態を復元する。
switchToNormalScreen :: Terminal -> Terminal
switchToNormalScreen term = case altScreen term of
  Nothing -> term  -- 既に通常画面
  Just saved ->
    let r = rows term
        c = cols term
        oldScreen = asScreen saved
        (_, (oldR, oldC)) = bounds oldScreen
        newScreen = if oldR == r && oldC == c
                    then oldScreen
                    else let blank = array ((1,1), (r,c))
                               [((y_,x_), e) | y_ <- [1..r], x_ <- [1..c]]
                             e = TerminalChar ' ' defaultForegroundColor defaultBackgroundColor False False False False
                             ovlp = [((y_,x_), oldScreen ! (y_,x_))
                                    | y_ <- [1..min r oldR], x_ <- [1..min c oldC]]
                         in blank // ovlp
        (cy_, cx_) = asCursorPos saved
    in term { altScreen = Nothing
            , screen = newScreen
            , cursorPos = (min r (max 1 cy_), min c (max 1 cx_))
            , scrollingRegion = (1, r)
            , pendingWrap = False
            }

-- | 保存したカーソル位置を復元する。
restoreSavedCursor :: Terminal -> Terminal
restoreSavedCursor term = case savedCursor term of
  Nothing  -> term
  Just pos -> safeCursor $ term { cursorPos = pos, savedCursor = Nothing, pendingWrap = False }

-- | Maybe が Just か判定する（Data.Maybe.isJust を使わず定義）。
isJust_ :: Maybe a -> Bool
isJust_ (Just _) = True
isJust_ Nothing  = False


applyAction :: Terminal -> TerminalAction -> Terminal
applyAction term'@Terminal { screen = s, cursorPos = pos_, inBuffer = inb  } act =
    -- safeCursor t
    t
    -- where t = case (trace ("Action" ++ show act) act) of
    where
      pos@(y, x) = pos_
      term = term'
      add s c = s ++ [ mkChar c term]
      r = rows term
      c = cols term
      write' l term = write term l
      curpos p term = safeCursor $ term {cursorPos = p, pendingWrap = False}

      -- | pending wrap を解決する。保留中なら折り返してから返す。
      resolvePending t
        | pendingWrap t =
            let (py, _) = cursorPos t
                (_, srBot) = scrollingRegion t
            in if py == srBot
               then scrollTerminalDown $ t { cursorPos = (srBot, 1), pendingWrap = False }
               else if py >= rows t
                 then t { cursorPos = (rows t, 1), pendingWrap = False }
                 else t { cursorPos = (py + 1, 1), pendingWrap = False }
        | otherwise = t

      t = case act of
            Ignored             -> term

            -- C0 制御文字で無視するもの
            CharInput c | c < ' ', c /= '\a', c /= '\t', c /= '\n', c /= '\r', c /= '\b'
                        -> term

            -- Bell
            CharInput '\a'      -> term 

            -- Tab
            CharInput '\t'      -> curpos (y, (x `div` 8 + 1) * 8) term

            -- Newline (LF)
            -- スクロール領域の底にいる場合は領域内スクロール
            -- 画面の絶対底にいてスクロール領域外の場合はスクロールしない
            CharInput '\n'      ->
              let term0 = term { pendingWrap = False }
                  (_, srBot) = scrollingRegion term0
              in if y == srBot
                 then scrollTerminalDown $ term0 { cursorPos = (srBot, 1) }
                 else if y >= r
                   then term0 { cursorPos = (r, 1) }  -- 絶対底: スクロールしない
                   else curpos (y + 1, 1) term0
            CharInput '\r'      -> curpos (y, 1) term
            CharInput '\b'      -> curpos (y, max 1 (x - 1)) $ write' [(pos, mkEmptyChar term)] term
            -- 通常文字入力: deferred wrap を実装
            CharInput ch        ->
              let term0 = resolvePending term
                  (y0, x0) = cursorPos term0
                  pos0 = (y0, x0)
                  wide = isWideChar ch
              in if wide
                 then -- ワイド文字: 2セル分を使用
                   if x0 >= c
                   then -- 最終列: ワイド文字は収まらないのでpending wrap
                     (write term0 [(pos0, mkChar ch term0)]) { pendingWrap = True }
                   else if x0 >= c - 1
                   then -- 残り1セル: ワイド文字は収まらないのでpending wrap（右側は画面外）
                     let contCell = (mkEmptyChar term0) { character = wideCharContinuation }
                     in (write term0 [(pos0, mkChar ch term0),
                                      ((y0, x0 + 1), contCell)]) { pendingWrap = True }
                   else -- 通常: 2セル書いてカーソルを2つ進める
                     let contCell = (mkEmptyChar term0) { character = wideCharContinuation }
                     in (write term0 [(pos0, mkChar ch term0),
                                      ((y0, x0 + 1), contCell)]) { cursorPos = (y0, x0 + 2) }
                 else -- 半角文字: 従来の処理
                   if x0 >= c
                   -- 最終列: 文字を書いて pending wrap 状態にする
                   then (write term0 [(pos0, mkChar ch term0)]) { pendingWrap = True }
                   -- 通常: 文字を書いてカーソルを右に進める
                   else (write term0 [(pos0, mkChar ch term0)]) { cursorPos = (y0, x0 + 1) }

            -- Cursor movements (pending wrap をキャンセル)
            CursorUp n          -> (iterate up (term { pendingWrap = False })) !! n
            CursorDown n        -> (iterate down (term { pendingWrap = False })) !! n
            CursorForward n     -> (iterate right (term { pendingWrap = False })) !! n
            CursorBackward n    -> (iterate left (term { pendingWrap = False })) !! n
            CursorAbsoluteColumn col -> safeCursor $ term { cursorPos = (y, col), pendingWrap = False }
            CursorAbsoluteRow row  -> safeCursor $ term { cursorPos = (row, x), pendingWrap = False }
            SetCursor row col   -> safeCursor $ term { cursorPos = (row, col), pendingWrap = False }

            -- Cursor visibility
            ShowCursor s        -> term { optionShowCursor = s }

            -- Cursor save/restore (DECSC/DECRC)
            SaveCursorPos       -> term { savedCursor = Just pos }
            RestoreCursorPos    -> restoreSavedCursor term

            -- Colors, yay!
            ANSIAction _ 'm'    -> term

            -- Scrolling
            SetScrollingRegion start end -> term { scrollingRegion = (start, end) }
            -- CSI r (パラメータなし): スクロール領域を画面全体にリセット
            ANSIAction [] 'r'   -> term { scrollingRegion = (1, r) }
            ScrollUp n          -> (iterate scrollTerminalUp term) !! n
            ScrollDown n        -> (iterate scrollTerminalDown term) !! n

            -- Erases the screen with the background color and moves the cursor to home.
            ANSIAction [2] 'J'  -> clearRows [1..r] $ term { cursorPos = (1, 1), pendingWrap = False }

            -- Erases the screen from the beginning to the cursor position.
            ANSIAction [1] 'J'  ->
              clearRows [1..y-1] $ clearColumns y [1..x] term

            -- Erases the screen from the cursor position to the end of the screen.
            ANSIAction _ 'J'    ->
              clearRows [y+1..r] $ clearColumns y [x..c] term

            -- Erases the entire current line.
            ANSIAction [2] 'K'  -> clearColumns y [1..c] term

            -- Erases from the current cursor position to the start of the current line.
            ANSIAction [1] 'K'  -> clearColumns y [1..x] term

            -- Erases from the current cursor position to the end of the current line. 
            ANSIAction _ 'K'    -> clearColumns y [x..c] term

            -- ECH: カーソル位置から n 文字を消去
            ANSIAction [] 'X'   -> clearColumns y [x..x] term
            ANSIAction [n] 'X'  -> clearColumns y [x..min (x+n-1) c] term

            -- DCH: カーソル位置から n 文字を削除し残りを左シフト
            ANSIAction [] 'P'   -> deleteChars 1 y x c term
            ANSIAction [n] 'P'  -> deleteChars n y x c term

            -- ICH: カーソル位置に n 個の空白を挿入し既存文字を右シフト
            ANSIAction [] '@'   -> insertChars 1 y x c term
            ANSIAction [n] '@'  -> insertChars n y x c term

            -- DL: カーソル行から n 行を削除し残りを上シフト
            ANSIAction [] 'M'   -> deleteLines 1 term
            ANSIAction [n] 'M'  -> deleteLines n term

            -- IL: カーソル行に n 行の空白を挿入し既存行を下シフト
            ANSIAction [] 'L'   -> insertLines 1 term
            ANSIAction [n] 'L'  -> insertLines n term

            -- DEC Private Mode Set (DECSET) — 各パラメータを個別適用
            DECAction ps 'h'   -> foldl' applyDECSet term ps

            -- DEC Private Mode Reset (DECRST) — 各パラメータを個別適用
            DECAction ps 'l'   -> foldl' applyDECReset term ps

            -- マウストラッキングモード（simplify 経由で単一パラメータから変換済みの場合）
            SetMouseMode m     -> term { mouseMode = m }
            SetMouseEncoding e -> term { mouseEncoding = e }

            -- ANSI save/restore cursor (CSI s / CSI u)
            ANSIAction [] 's'   -> term { savedCursor = Just pos }
            ANSIAction [] 'u'   -> restoreSavedCursor term
         
            -- Set the terminal title
            SetTerminalTitle t  -> term { terminalTitle = t }

            -- Attribute mode / color handling
            SetAttributeMode ms -> foldl' applyAttributeMode term ms
            -- SetAttributeMode [] -> applyAttributeMode term $ ResetAllAttributes

            -- Keypad mode switches (no-op for rendering; affects key encoding)
            KeypadKeysApplicationsMode -> term
            KeypadKeysNumericMode      -> term

            _                   -> trace ("\nTerminal.hs Unimplemented action: " ++ show act) term

-- | 'applyAction' のバッチ最適化版（STArray ベース）。
--
-- イミュータブル配列の @(//)@ は呼び出しごとに配列全体をコピーするため、
-- アクションごとに呼ぶと @O(actions × cells)@ のコストがかかる。
-- この関数は画面バッファを @STArray@ に @thaw@ し、全アクションを
-- @writeArray@/@readArray@ で直接処理してから @unsafeFreeze@ で戻す。
-- これにより @O(actions + cells)@ で済む。
--
-- 外部シグネチャは純粋関数のまま維持する。
applyActionsBatched :: Terminal -> [TerminalAction] -> Terminal
applyActionsBatched term [] = term
applyActionsBatched term actions = runST $ do
  arr0 <- thaw (screen term)
  (finalTerm, finalArr) <- goST arr0 term actions
  finalScreen <- unsafeFreeze finalArr
  return $ finalTerm { screen = finalScreen }

-- ── ST ヘルパー関数 ────────────────────────────────────────

-- | スクロール領域内の行を上にシフト（内容が上に移動、下端が空く）。
scrollDownST :: (Int, Int) -> Int -> STArray s ScreenIndex TerminalChar -> ST s ()
scrollDownST (startrow, endrow) c arr =
  forM_ [startrow .. endrow - 1] $ \y ->
    forM_ [1..c] $ \x -> do
      val <- readArray arr (y + 1, x)
      writeArray arr (y, x) val

-- | スクロール領域内の行を下にシフト（内容が下に移動、上端が空く）。
scrollUpST :: (Int, Int) -> Int -> STArray s ScreenIndex TerminalChar -> ST s ()
scrollUpST (startrow, endrow) c arr =
  forM_ (reverse [startrow + 1 .. endrow]) $ \y ->
    forM_ [1..c] $ \x -> do
      val <- readArray arr (y - 1, x)
      writeArray arr (y, x) val

-- | 指定行を空白文字で埋める。
clearRowsST :: STArray s ScreenIndex TerminalChar -> TerminalChar -> Int -> [Int] -> ST s ()
clearRowsST arr emptyC c rs =
  forM_ rs $ \y ->
    forM_ [1..c] $ \x ->
      writeArray arr (y, x) emptyC

-- | 指定行の指定列を空白文字で埋める。
clearColumnsST :: STArray s ScreenIndex TerminalChar -> TerminalChar -> Int -> [Int] -> ST s ()
clearColumnsST arr emptyC row cs =
  forM_ cs $ \x ->
    writeArray arr (row, x) emptyC

-- | DCH: カーソル位置から n 文字を削除し残りを左シフト。
deleteCharsST :: STArray s ScreenIndex TerminalChar -> TerminalChar -> Int -> Int -> Int -> Int -> ST s ()
deleteCharsST arr emptyC n row cx maxCol = do
  -- 左にシフト
  forM_ [cx .. maxCol - n] $ \col ->
    when (col + n <= maxCol) $ do
      val <- readArray arr (row, col + n)
      writeArray arr (row, col) val
  -- 右端を空白で埋める
  forM_ [max cx (maxCol - n + 1) .. maxCol] $ \col ->
    writeArray arr (row, col) emptyC

-- | ICH: カーソル位置に n 個の空白を挿入し既存文字を右シフト。
insertCharsST :: STArray s ScreenIndex TerminalChar -> TerminalChar -> Int -> Int -> Int -> Int -> ST s ()
insertCharsST arr emptyC n row cx maxCol = do
  -- 右にシフト（逆順）
  forM_ (reverse [cx .. maxCol - n]) $ \col ->
    when (col + n <= maxCol) $ do
      val <- readArray arr (row, col)
      writeArray arr (row, col + n) val
  -- カーソル位置に空白
  forM_ [cx .. min (cx + n - 1) maxCol] $ \col ->
    writeArray arr (row, col) emptyC

-- | DL: カーソル行から n 行を削除し残りを上シフト。
deleteLinesST :: STArray s ScreenIndex TerminalChar -> TerminalChar -> Int -> Int -> Int -> Int -> ST s ()
deleteLinesST arr emptyC n cy srBot maxCol = do
  forM_ [cy .. srBot - n] $ \row ->
    when (row + n <= srBot) $
      forM_ [1..maxCol] $ \col -> do
        val <- readArray arr (row + n, col)
        writeArray arr (row, col) val
  forM_ [max cy (srBot - n + 1) .. srBot] $ \row ->
    forM_ [1..maxCol] $ \col ->
      writeArray arr (row, col) emptyC

-- | IL: カーソル行に n 行の空白を挿入し既存行を下シフト。
insertLinesST :: STArray s ScreenIndex TerminalChar -> TerminalChar -> Int -> Int -> Int -> Int -> ST s ()
insertLinesST arr emptyC n cy srBot maxCol = do
  forM_ (reverse [cy .. srBot - n]) $ \row ->
    when (row + n <= srBot) $
      forM_ [1..maxCol] $ \col -> do
        val <- readArray arr (row, col)
        writeArray arr (row + n, col) val
  forM_ [cy .. min (cy + n - 1) srBot] $ \row ->
    forM_ [1..maxCol] $ \col ->
      writeArray arr (row, col) emptyC

-- | 先頭行を STArray から読み取ってスクロールバック用 Vector を作成する。
pushScrollbackST :: STArray s ScreenIndex TerminalChar -> Int -> ST s (V.Vector TerminalChar)
pushScrollbackST arr c = V.generateM c $ \i -> readArray arr (1, i + 1)

-- ── goST: STArray ベースのアクション処理ループ ──────────────

goST :: STArray s ScreenIndex TerminalChar -> Terminal -> [TerminalAction]
     -> ST s (Terminal, STArray s ScreenIndex TerminalChar)
goST arr term [] = return (term, arr)
goST arr term (act : rest) = case act of
  Ignored -> goST arr term rest

  -- C0 制御文字で無視するもの
  CharInput c | c < ' ', c /= '\a', c /= '\t', c /= '\n', c /= '\r', c /= '\b'
              -> goST arr term rest

  CharInput '\a' -> goST arr term rest

  -- Tab
  CharInput '\t' ->
    let (y, x) = cursorPos term
        x' = min (cols term) ((x `div` 8 + 1) * 8)
    in goST arr (term { cursorPos = (y, x'), pendingWrap = False }) rest

  -- Newline (LF)
  CharInput '\n' -> do
    let term0 = term { pendingWrap = False }
        (y, _) = cursorPos term0
        (startrow, srBot) = scrollingRegion term0
        r = rows term0
        c = cols term0
        emptyC = mkEmptyChar term0
    term1 <- if y == srBot
             then do
               term0' <- if startrow == 1 && isNothing (altScreen term0)
                 then do
                   topLine <- pushScrollbackST arr c
                   let buf = scrollbackBuffer term0 Seq.|> topLine
                       buf' = if Seq.length buf > scrollbackMax term0
                              then Seq.drop 1 buf else buf
                   return $ term0 { scrollbackBuffer = buf', cursorPos = (srBot, 1) }
                 else return $ term0 { cursorPos = (srBot, 1) }
               scrollDownST (scrollingRegion term0') c arr
               clearRowsST arr emptyC c [srBot]
               return term0'
             else if y >= r
               then return $ term0 { cursorPos = (r, 1) }
               else return $ safeCursor $ term0 { cursorPos = (y + 1, 1) }
    goST arr term1 rest

  -- Carriage return
  CharInput '\r' ->
    let (y, _) = cursorPos term
    in goST arr (term { cursorPos = (y, 1), pendingWrap = False }) rest

  -- Backspace
  CharInput '\b' -> do
    let (y, x) = cursorPos term
        emptyC = mkEmptyChar term
    writeArray arr (y, x) emptyC
    goST arr (safeCursor $ term { cursorPos = (y, max 1 (x - 1)), pendingWrap = False }) rest

  -- 通常文字入力
  CharInput ch -> do
    let c = cols term
        r = rows term
    -- pendingWrap を解決
    term1 <- resolvePendingST arr term
    let (y0, x0) = cursorPos term1
        wide = isWideChar ch
        charCell = mkChar ch term1
    if y0 >= 1 && y0 <= r && x0 >= 1
      then do
        if wide
          then do
            let contCell = (mkEmptyChar term1) { character = wideCharContinuation }
            if x0 >= c
              then do
                writeArray arr (y0, x0) charCell
                goST arr (term1 { pendingWrap = True }) rest
              else if x0 >= c - 1
              then do
                writeArray arr (y0, x0) charCell
                when (x0 + 1 <= c) $ writeArray arr (y0, x0 + 1) contCell
                goST arr (term1 { pendingWrap = True }) rest
              else do
                writeArray arr (y0, x0) charCell
                writeArray arr (y0, x0 + 1) contCell
                goST arr (term1 { cursorPos = (y0, x0 + 2) }) rest
          else do
            writeArray arr (y0, x0) charCell
            if x0 >= c
              then goST arr (term1 { pendingWrap = True }) rest
              else goST arr (term1 { cursorPos = (y0, x0 + 1) }) rest
      else goST arr term1 rest

  -- SGR (色・属性変更): 画面に触れない
  SetAttributeMode ms ->
    goST arr (foldl' applyAttributeMode term ms) rest

  -- カーソル移動: 画面に触れない
  SetCursor row col ->
    let y' = min (rows term) (max 1 row)
        x' = min (cols term) (max 1 col)
    in goST arr (term { cursorPos = (y', x'), pendingWrap = False }) rest

  CursorUp n ->
    let (y, x) = cursorPos term
    in goST arr (safeCursor $ term { cursorPos = (y - n, x), pendingWrap = False }) rest

  CursorDown n ->
    let (y, x) = cursorPos term
    in goST arr (safeCursor $ term { cursorPos = (y + n, x), pendingWrap = False }) rest

  CursorForward n ->
    let (y, x) = cursorPos term
    in goST arr (safeCursor $ term { cursorPos = (y, x + n), pendingWrap = False }) rest

  CursorBackward n ->
    let (y, x) = cursorPos term
    in goST arr (safeCursor $ term { cursorPos = (y, x - n), pendingWrap = False }) rest

  CursorAbsoluteColumn col ->
    let (y, _) = cursorPos term
    in goST arr (safeCursor $ term { cursorPos = (y, col), pendingWrap = False }) rest

  CursorAbsoluteRow row ->
    let (_, x) = cursorPos term
    in goST arr (safeCursor $ term { cursorPos = (row, x), pendingWrap = False }) rest

  ShowCursor s -> goST arr (term { optionShowCursor = s }) rest
  SaveCursorPos -> goST arr (term { savedCursor = Just (cursorPos term) }) rest
  RestoreCursorPos -> goST arr (restoreSavedCursor term) rest
  SetTerminalTitle t -> goST arr (term { terminalTitle = t }) rest
  KeypadKeysApplicationsMode -> goST arr term rest
  KeypadKeysNumericMode -> goST arr term rest
  SetScrollingRegion s e -> goST arr (term { scrollingRegion = (s, e), pendingWrap = False }) rest
  SetMouseMode m -> goST arr (term { mouseMode = m }) rest
  SetMouseEncoding e -> goST arr (term { mouseEncoding = e }) rest

  ANSIAction [] 'r' ->
    goST arr (term { scrollingRegion = (1, rows term), pendingWrap = False }) rest
  ANSIAction _ 'm' -> goST arr term rest
  ANSIAction [] 's' -> goST arr (term { savedCursor = Just (cursorPos term) }) rest
  ANSIAction [] 'u' -> goST arr (restoreSavedCursor term) rest

  -- Scroll
  ScrollUp n -> do
    let c = cols term
        r = scrollingRegion term
        emptyC = mkEmptyChar term
    forM_ [1..n] $ \_ -> do
      scrollUpST r c arr
      clearRowsST arr emptyC c [fst r]
    goST arr term rest

  ScrollDown n -> do
    let c = cols term
        r = scrollingRegion term
        emptyC = mkEmptyChar term
    forM_ [1..n] $ \_ -> do
      scrollDownST r c arr
      clearRowsST arr emptyC c [snd r]
    goST arr term rest

  -- Erase screen
  ANSIAction [2] 'J' -> do
    let emptyC = mkEmptyChar term
    clearRowsST arr emptyC (cols term) [1..rows term]
    goST arr (term { cursorPos = (1, 1), pendingWrap = False }) rest

  ANSIAction [1] 'J' -> do
    let (y, x) = cursorPos term
        emptyC = mkEmptyChar term
    clearRowsST arr emptyC (cols term) [1..y-1]
    clearColumnsST arr emptyC y [1..x]
    goST arr term rest

  ANSIAction _ 'J' -> do
    let (y, x) = cursorPos term
        emptyC = mkEmptyChar term
    clearRowsST arr emptyC (cols term) [y+1..rows term]
    clearColumnsST arr emptyC y [x..cols term]
    goST arr term rest

  -- Erase line
  ANSIAction [2] 'K' -> do
    let (y, _) = cursorPos term
        emptyC = mkEmptyChar term
    clearColumnsST arr emptyC y [1..cols term]
    goST arr term rest

  ANSIAction [1] 'K' -> do
    let (y, x) = cursorPos term
        emptyC = mkEmptyChar term
    clearColumnsST arr emptyC y [1..x]
    goST arr term rest

  ANSIAction _ 'K' -> do
    let (y, x) = cursorPos term
        emptyC = mkEmptyChar term
    clearColumnsST arr emptyC y [x..cols term]
    goST arr term rest

  -- ECH: erase characters
  ANSIAction [] 'X' -> do
    let (y, x) = cursorPos term
        emptyC = mkEmptyChar term
    clearColumnsST arr emptyC y [x..x]
    goST arr term rest

  ANSIAction [n] 'X' -> do
    let (y, x) = cursorPos term
        emptyC = mkEmptyChar term
    clearColumnsST arr emptyC y [x..min (x+n-1) (cols term)]
    goST arr term rest

  -- DCH: delete characters
  ANSIAction [] 'P' -> do
    let (y, x) = cursorPos term
        emptyC = mkEmptyChar term
    deleteCharsST arr emptyC 1 y x (cols term)
    goST arr term rest

  ANSIAction [n] 'P' -> do
    let (y, x) = cursorPos term
        emptyC = mkEmptyChar term
    deleteCharsST arr emptyC n y x (cols term)
    goST arr term rest

  -- ICH: insert characters
  ANSIAction [] '@' -> do
    let (y, x) = cursorPos term
        emptyC = mkEmptyChar term
    insertCharsST arr emptyC 1 y x (cols term)
    goST arr term rest

  ANSIAction [n] '@' -> do
    let (y, x) = cursorPos term
        emptyC = mkEmptyChar term
    insertCharsST arr emptyC n y x (cols term)
    goST arr term rest

  -- DL: delete lines
  ANSIAction [] 'M' -> do
    let (cy, _) = cursorPos term
        (_, srBot) = scrollingRegion term
        emptyC = mkEmptyChar term
    deleteLinesST arr emptyC 1 cy srBot (cols term)
    goST arr term rest

  ANSIAction [n] 'M' -> do
    let (cy, _) = cursorPos term
        (_, srBot) = scrollingRegion term
        emptyC = mkEmptyChar term
    deleteLinesST arr emptyC n cy srBot (cols term)
    goST arr term rest

  -- IL: insert lines
  ANSIAction [] 'L' -> do
    let (cy, _) = cursorPos term
        (_, srBot) = scrollingRegion term
        emptyC = mkEmptyChar term
    insertLinesST arr emptyC 1 cy srBot (cols term)
    goST arr term rest

  ANSIAction [n] 'L' -> do
    let (cy, _) = cursorPos term
        (_, srBot) = scrollingRegion term
        emptyC = mkEmptyChar term
    insertLinesST arr emptyC n cy srBot (cols term)
    goST arr term rest

  -- DEC Private Mode Set (DECSET)
  DECAction ps 'h' -> do
    let applyDEC t p = case p of
          25   -> t { optionShowCursor = True }
          1000 -> t { mouseMode = MouseNormal }
          1002 -> t { mouseMode = MouseButton }
          1003 -> t { mouseMode = MouseAll }
          1006 -> t { mouseEncoding = MouseEncodingSGR }
          1048 -> t { savedCursor = Just (cursorPos t) }
          -- alt screen: barrier 必要
          _ | p `elem` [47, 1047, 1049] -> applyDECSet t p
          _    -> t
    -- alt screen 切り替えがあった場合のみ barrier
    if any (`elem` [47, 1047, 1049]) ps
      then do
        curScreen <- freeze arr
        let term' = foldl' applyDECSet (term { screen = curScreen }) ps
        arr' <- thaw (screen term')
        goST arr' term' rest
      else goST arr (foldl' applyDEC term ps) rest

  -- DEC Private Mode Reset (DECRST)
  DECAction ps 'l' -> do
    let applyDEC t p = case p of
          25   -> t { optionShowCursor = False }
          1000 -> t { mouseMode = MouseNone }
          1002 -> t { mouseMode = MouseNone }
          1003 -> t { mouseMode = MouseNone }
          1006 -> t { mouseEncoding = MouseEncodingX10 }
          1048 -> restoreSavedCursor t
          -- alt screen: barrier 必要
          _ | p `elem` [47, 1047, 1049] -> applyDECReset t p
          _    -> t
    if any (`elem` [47, 1047, 1049]) ps
      then do
        curScreen <- freeze arr
        let term' = foldl' applyDECReset (term { screen = curScreen }) ps
        arr' <- thaw (screen term')
        goST arr' term' rest
      else goST arr (foldl' applyDEC term ps) rest

  -- その他: barrier で pure 関数にフォールバック
  _ -> do
    trace ("goST barrier fallback: " ++ show act) $ return ()
    curScreen <- freeze arr
    let term' = applyAction (term { screen = curScreen }) act
    arr' <- thaw (screen term')
    goST arr' term' rest

-- | pendingWrap を解決する ST 版。
resolvePendingST :: STArray s ScreenIndex TerminalChar -> Terminal
                 -> ST s Terminal
resolvePendingST arr term
  | pendingWrap term = do
      let (py, _) = cursorPos term
          (startrow, srBot) = scrollingRegion term
          c = cols term
          emptyC = mkEmptyChar term
      if py == srBot
        then do
          -- scrollTerminalDown: scrollback + scroll
          term' <- if startrow == 1 && isNothing (altScreen term)
            then do
              topLine <- pushScrollbackST arr c
              let buf = scrollbackBuffer term Seq.|> topLine
                  buf' = if Seq.length buf > scrollbackMax term
                         then Seq.drop 1 buf else buf
              return $ term { scrollbackBuffer = buf', cursorPos = (srBot, 1), pendingWrap = False }
            else return $ term { cursorPos = (srBot, 1), pendingWrap = False }
          scrollDownST (scrollingRegion term') c arr
          clearRowsST arr emptyC c [srBot]
          return term'
        else if py >= rows term
          then return $ term { cursorPos = (rows term, 1), pendingWrap = False }
          else return $ term { cursorPos = (py + 1, 1), pendingWrap = False }
  | otherwise = return term

