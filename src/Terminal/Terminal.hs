{-# LANGUAGE Rank2Types #-}
-- Captures how TerminalActions change the Terminal
module Terminal.Terminal (newTerminal, defaultTerm, applyAction, testTerm, scrollTerminalDown, scrollTerminalUp, setSize) where
import System.Process
import Data.Array.Diff
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

defaultForegroundColor = White
defaultBackgroundColor = Black

mkChar c term = TerminalChar {
                    character = c,
                    foregroundColor = currentForeground term,
                    backgroundColor = currentBackground term,
                    isBright = optionBright term,
                    isBlinking = optionBlinking term,
                    isUnderlined = optionUnderlined term,
                    isInverse = optionInverse term
                }
mkEmptyChar = mkChar ' '
testTerm = defaultTerm
defaultTerm = newTerminal (24, 80)

setSize :: (Int, Int) -> Terminal -> Terminal
setSize  s@(r, c) term  = let
  or = rows term
  oc = cols term
  diff = r*c - or*oc
  del = if diff <0 then 0 else diff
  allBuff = allBuffer term
  -- newBuff = delLastTill (\c-> (character c) `eq` CharInput (mkEmptyChar term)) $ allBuff
  newterm = (newTerminal s $ terminfo term)
  buff = drop ((length allBuff)- (2*r*c)::Int) $ allBuff 
  in foldl  applyAction newterm buff

delLastTill :: (a -> Bool) -> [a] -> [a]
delLastTill p (x:xs) = if p x
                       then delLastTill p xs
                       else xs
  -- ixmap ((1::Int,1::Int) , (r, c)) f s
  -- where f (y, x) = let
  --         (_ , (oldr, oldc) ) = bounds s
  --         total = y * oldc + x
                            --         in (total `quot` c, total `rem` c)
          
newTerminal s@(rows, cols) term = Terminal {
    cursorPos = (1, 1),
    rows = rows,
    cols = cols,
    inBuffer = "",
    allBuffer = [],
    responseBuffer = "",
    scrollingRegion = (1, rows),
    screen = array ((1, 1), s)
        [((y, x), e) | x <- [1..cols], y <- [1..rows]],
    currentForeground = defaultForegroundColor,
    currentBackground = defaultBackgroundColor,
    optionShowCursor = True,
    terminalTitle = "",
    terminfo = term,
    optionBright = False,
    optionUnderlined = False,
    optionBlinking = False,
    optionInverse = False
} where e = mkEmptyChar (newTerminal s term) -- Hail laziness

up t@Terminal {cursorPos = (y, x)} = safeCursor $ t { cursorPos = (y - 1, x) }
down t@Terminal {cursorPos = (y, x)} = safeCursor $ t { cursorPos = (y + 1, x) }
left t@Terminal {cursorPos = (y, x)} = safeCursor $ t { cursorPos = (y, x - 1) }
right t@Terminal {cursorPos = (y, x)} = safeCursor $ t { cursorPos = (y, x + 1) }

-- Wrap line
safeCursor t@Terminal {cursorPos = (y, x), cols = c , rows = r }
  | x > c  =  safeCursor $ t { cursorPos = (y + 1, 1) }
  | y > r  =  safeCursor $ scrollTerminalDown $ t
    { cursorPos = (r, 1)
    -- , inBuffer = (inBuffer t) ++  [takeWhile (((==) '\n') . character) (elems (screen t))]
    }
  | otherwise     =  t { cursorPos = (min r (max 1 y), min c (max 1 x)) }


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


applyAction :: Terminal -> TerminalAction -> Terminal
applyAction term'@Terminal { screen = s, cursorPos = pos_, inBuffer = inb  } act =
    -- safeCursor t
    t
    -- where t = case (trace ("Action" ++ show act) act) of
    where
      pos@ (y, x) = pos_
      term = term' {allBuffer = (allBuffer term') ++ [act]}
      add s c = s ++ [ mkChar c term]
      r = rows term
      c = cols term
      write' l term = write term l
      curpos p term =safeCursor $ term {cursorPos = p}

      t = case act of
            Ignored             -> term

            -- Bell
            CharInput c@'\a'      -> term 

            -- Tab
            CharInput c@ '\t'      -> curpos (y, (x `div` 8 + 1) * 8) term

            -- Newline
            CharInput ch@ '\n'      -> curpos  (y + 1, 1) term
            CharInput c@ '\r'      -> curpos (y, 1) term
            CharInput c@ '\b'      -> curpos (y, x - 1) $ write' [(pos, mkEmptyChar term)] term
            CharInput c         -> curpos (y, x + 1) $ write' [(pos, mkChar c term)] term 

            -- Cursor movements
            CursorUp n          -> (iterate up term) !! n
            CursorDown n        -> (iterate down term) !! n
            CursorForward n     -> (iterate right term) !! n
            CursorBackward n    -> (iterate left term) !! n
            CursorAbsoluteColumn col -> term { cursorPos = (y, col) }
            CursorAbsoluteRow row  -> term { cursorPos = (row, x) }
            SetCursor row col   -> term { cursorPos = (row, col) }

            -- Cursor visibility
            ShowCursor s        -> term { optionShowCursor = s }

            -- Colors, yay!
            ANSIAction _ 'm'    -> term

            -- Scrolling
            SetScrollingRegion start end -> term { scrollingRegion = (start, end) }
            ScrollUp n          -> (iterate scrollTerminalUp term) !! n
            ScrollDown n        -> (iterate scrollTerminalDown term) !! n

            -- Erases the screen with the background color and moves the cursor to home.
            ANSIAction [2] 'J'  -> clearRows [1..r] $ term { cursorPos = (1, 1) }

            -- Erases the screen from the current line up to the top of the screen.
            ANSIAction [1] 'J'  -> clearRows [1..y] term

            -- Erases the screen from the current line down to the bottom of the screen.
            ANSIAction _ 'J'    -> clearRows [y..r] term

            -- Erases the entire current line.
            ANSIAction [2] 'K'  -> clearColumns y [1..c] term

            -- Erases from the current cursor position to the start of the current line.
            ANSIAction [1] 'K'  -> clearColumns y [1..x] term

            -- Erases from the current cursor position to the end of the current line. 
            ANSIAction _ 'K'    -> clearColumns y [x..c] term

            ANSIAction [n] 'X'    -> clearColumns y [x..(n+x)] term
            -- Set Mode. 
            ANSIAction _ 'h'    -> clearColumns y [x..c] term

            -- Reset Mode. 
            ANSIAction _ 'l'    -> clearColumns y [x..c] term
         
            -- Set the terminal title
            SetTerminalTitle t  -> term { terminalTitle = t }

            -- Attribute mode / color handling
            SetAttributeMode ms -> foldl applyAttributeMode term ms
            -- SetAttributeMode [] -> applyAttributeMode term $ ResetAllAttributes

            _                   -> trace ("\nTerminal.hs Unimplemented action: " ++ show act) term

