{-# LANGUAGE Rank2Types #-}
-- | ターミナル状態マシン。
--
-- 'TerminalAction' を 'Terminal' に適用して状態を遷移させる純粋関数を提供する。
-- カーソル移動、スクロール、文字書き込み、属性変更、画面消去、
-- リサイズなどのターミナル操作を実装する。
module Terminal.Terminal (newTerminal, defaultTerm, applyAction, applyActionsBatched, testTerm, scrollTerminalDown, scrollTerminalUp, setSize) where
import System.Process
import Data.Array
import Data.Char
import Control.Monad
import Control.Monad.State hiding (state)
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
    pendingWrap = False
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
    pendingWrap = False
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
    clearRows [endrow] $ term {
        screen = scrollScreenDown (scrollingRegion term) s
    }

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
applyDECSet term 1000 = term { mouseMode = MouseNormal }
applyDECSet term 1002 = term { mouseMode = MouseButton }
applyDECSet term 1003 = term { mouseMode = MouseAll }
applyDECSet term 1006 = term { mouseEncoding = MouseEncodingSGR }
applyDECSet term _    = term

-- | DECRST: DEC プライベートモードの個別パラメータリセット。
applyDECReset :: Terminal -> Int -> Terminal
applyDECReset term 25   = term { optionShowCursor = False }
applyDECReset term 1000 = term { mouseMode = MouseNone }
applyDECReset term 1002 = term { mouseMode = MouseNone }
applyDECReset term 1003 = term { mouseMode = MouseNone }
applyDECReset term 1006 = term { mouseEncoding = MouseEncodingX10 }
applyDECReset term _    = term


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
              in if x0 >= c
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
            DECAction ps 'h'   -> foldl applyDECSet term ps

            -- DEC Private Mode Reset (DECRST) — 各パラメータを個別適用
            DECAction ps 'l'   -> foldl applyDECReset term ps

            -- マウストラッキングモード（simplify 経由で単一パラメータから変換済みの場合）
            SetMouseMode m     -> term { mouseMode = m }
            SetMouseEncoding e -> term { mouseEncoding = e }

            -- ANSI save/restore cursor (CSI s / CSI u) — no-op
            ANSIAction [] 's'   -> term
            ANSIAction [] 'u'   -> term
         
            -- Set the terminal title
            SetTerminalTitle t  -> term { terminalTitle = t }

            -- Attribute mode / color handling
            SetAttributeMode ms -> foldl applyAttributeMode term ms
            -- SetAttributeMode [] -> applyAttributeMode term $ ResetAllAttributes

            -- Keypad mode switches (no-op for rendering; affects key encoding)
            KeypadKeysApplicationsMode -> term
            KeypadKeysNumericMode      -> term

            _                   -> trace ("\nTerminal.hs Unimplemented action: " ++ show act) term

-- | 'applyAction' のバッチ最適化版。
--
-- 連続する通常文字入力の画面書き込みを蓄積し、一括で @Array (//)@ に反映する。
-- @(//)@ は呼び出しごとに配列全体をコピーするため、1 文字ずつ呼ぶと
-- @O(actions × cells)@ のコストがかかるが、まとめて適用すると
-- @O(actions + cells)@ で済む。
--
-- SGR・カーソル移動など画面バッファに触れないアクションは蓄積中でも
-- フラッシュせずにインラインで処理するため、htop のような
-- "SetCursor → SGR → 文字列" パターンでバッチが途切れない。
--
-- Deferred wrap: 最終列に文字を書いたら pendingWrap = True とし、
-- 次の文字入力時に折り返す。カーソル移動コマンドでは pendingWrap を解除する。
applyActionsBatched :: Terminal -> [TerminalAction] -> Terminal
applyActionsBatched term [] = term
applyActionsBatched term actions = go term actions []
  where
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

    -- 入力終了: 蓄積した書き込みを反映
    go term [] pending = flush term pending

    -- 通常文字入力: 画面書き込みを蓄積しカーソルだけ進める
    go term (CharInput c : rest) pending
      | c /= '\a', c /= '\t', c /= '\n', c /= '\r', c /= '\b'
      = let term0 = if pendingWrap term then flush term pending else term
            term1 = resolvePending term0
            (y, x) = cursorPos term1
            pending' = if pendingWrap term then [] else pending
        in if x >= 1 && y >= 1 && y <= rows term1
           then if x >= cols term1
                -- 最終列: 文字を書いて pending wrap
                then go (term1 { pendingWrap = True })
                        rest
                        (((y, x), mkChar c term1) : pending')
                -- 通常: 文字を書いてカーソルを右へ
                else go (term1 { cursorPos = (y, x + 1) })
                        rest
                        (((y, x), mkChar c term1) : pending')
           else go (applyAction (flush term1 pending') (CharInput c)) rest []

    -- SGR (色・属性変更): 画面は触らない
    go term (SetAttributeMode ms : rest) pending =
      go (foldl applyAttributeMode term ms) rest pending

    -- カーソル移動: 画面は触らない（クランプのみ）、pendingWrap を解除
    go term (SetCursor row col : rest) pending =
      let y' = min (rows term) (max 1 row)
          x' = min (cols term) (max 1 col)
      in go (term { cursorPos = (y', x'), pendingWrap = False }) rest pending
    go term (CursorUp n : rest) pending =
      let (y, x) = cursorPos term
      in go (term { cursorPos = (max 1 (y - n), x), pendingWrap = False }) rest pending
    go term (CursorDown n : rest) pending =
      let (y, x) = cursorPos term
      in go (term { cursorPos = (min (rows term) (y + n), x), pendingWrap = False }) rest pending
    go term (CursorForward n : rest) pending =
      let (y, x) = cursorPos term
      in go (term { cursorPos = (y, min (cols term) (x + n)), pendingWrap = False }) rest pending
    go term (CursorBackward n : rest) pending =
      let (y, x) = cursorPos term
      in go (term { cursorPos = (y, max 1 (x - n)), pendingWrap = False }) rest pending
    go term (CursorAbsoluteColumn col : rest) pending =
      let (y, _) = cursorPos term
      in go (term { cursorPos = (y, min (cols term) (max 1 col)), pendingWrap = False }) rest pending
    go term (CursorAbsoluteRow row : rest) pending =
      let (_, x) = cursorPos term
      in go (term { cursorPos = (min (rows term) (max 1 row), x), pendingWrap = False }) rest pending

    -- no-op 系: 画面は触らない
    go term (Ignored : rest) pending = go term rest pending
    go term (ShowCursor s : rest) pending =
      go (term { optionShowCursor = s }) rest pending
    go term (SetTerminalTitle t : rest) pending =
      go (term { terminalTitle = t }) rest pending
    go term (KeypadKeysApplicationsMode : rest) pending = go term rest pending
    go term (KeypadKeysNumericMode : rest) pending = go term rest pending
    go term (SetScrollingRegion s e : rest) pending =
      go (term { scrollingRegion = (s, e), pendingWrap = False }) rest pending
    go term (ANSIAction [] 'r' : rest) pending =
      go (term { scrollingRegion = (1, rows term), pendingWrap = False }) rest pending
    go term (DECAction ps 'h' : rest) pending = go (foldl applyDECSet term ps) rest pending
    go term (DECAction ps 'l' : rest) pending = go (foldl applyDECReset term ps) rest pending
    go term (SetMouseMode m : rest) pending =
      go (term { mouseMode = m }) rest pending
    go term (SetMouseEncoding e : rest) pending =
      go (term { mouseEncoding = e }) rest pending
    go term (ANSIAction [] 's' : rest) pending = go term rest pending
    go term (ANSIAction [] 'u' : rest) pending = go term rest pending
    go term (ANSIAction _ 'm' : rest) pending = go term rest pending

    -- その他（スクロール、消去、文字削除/挿入 等）:
    -- 蓄積した書き込みを反映してから applyAction で処理
    go term (act : rest) pending =
      go (applyAction (flush term pending) act) rest []

    flush term [] = term
    flush term@Terminal{screen = s} ws = term { screen = s // ws }

