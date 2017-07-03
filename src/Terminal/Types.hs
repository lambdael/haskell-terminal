module Terminal.Types where
import Data.Array.Diff
import Data.Char
import Data.Maybe (fromJust, fromMaybe)
import Data.Tuple (swap)
import qualified System.Console.Terminfo as TI

type ScreenIndex = (Int, Int)

data TerminalChar = TerminalChar {
    character :: Char,
    foregroundColor :: TerminalColor,
    backgroundColor :: TerminalColor,
    isBright :: Bool,
    isUnderlined :: Bool,
    isBlinking :: Bool,
    isInverse :: Bool
} deriving (Show)

type TerminalArray = DiffArray ScreenIndex
type TerminalScreen = TerminalArray TerminalChar
type TerminalColorArray = TerminalArray Int

data Terminal = Terminal {
    cursorPos :: ScreenIndex,
    screen :: TerminalScreen,
    inBuffer :: String,
    allBuffer :: [TerminalAction],
    responseBuffer :: String,
    terminalTitle :: String,
    scrollingRegion :: (Int, Int),
    rows :: Int,
    cols :: Int,

    currentForeground :: TerminalColor,
    currentBackground :: TerminalColor,
    terminfo :: TI.Terminal,
    optionShowCursor :: Bool,
    optionBright :: Bool,
    optionUnderlined :: Bool,
    optionInverse :: Bool,
    optionBlinking :: Bool
}

data TerminalAction =
       CharInput Char


     -- Cursor movements
     | CursorUp Int
     | CursorDown Int
     | CursorForward Int
     | CursorBackward Int
     | SetCursor Int Int
     | CursorAbsoluteColumn Int
     | CursorAbsoluteRow Int



     -- Scrolling
     | SetScrollingRegion Int Int
     | ScrollUp Int
     | ScrollDown Int

     | ANSIAction [Int] Char
     | KeypadKeysApplicationsMode
     | KeypadKeysNumericMode
     | SetAttributeMode [AttributeMode]
     | SetTerminalTitle String
     | ShowCursor Bool
     | Ignored
     deriving (Show, Eq)

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

data AttributeMode =
       InvalidAttributeMode
     | ResetAllAttributes
     | Bright
     | Dim
     | Underlined
     | Blinking
     | Inverse
     | Hidden
     | Normal
     | NotUnderlined
     | NotBlinking
     | NotInverse
     | NotHidden
     | Foreground TerminalColor
     | Background TerminalColor
     | ResetForeground
     | ResetBackground
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

