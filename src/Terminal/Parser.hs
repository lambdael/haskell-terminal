-- | VT100/ANSI エスケープシーケンスパーサー。
--
-- 生のバイトストリームを Parsec で解析し、
-- 'TerminalAction' のリストに変換する。
-- CSI シーケンス、OSC タイトル設定、制御文字、
-- 通常文字を認識する。
module Terminal.Parser (parseANSI, parseANSIAnnotate) where
import           Control.Applicative  hiding (many, (<|>))
import           Control.Monad
import           Control.Monad.State
import           Data.Char
import           Data.List            (insert)
import           Data.Maybe           (maybeToList)
import           Data.Word            (Word8)
import           Debug.Trace
import           System.Exit
import           System.IO
import           Text.Parsec
import           Text.Parsec.String
import qualified Text.Parsec.Token    as PT

import           Terminal.ParserUtils
import           Terminal.Types

-- | 生の CSI シーケンスを意味のある 'TerminalAction' に変換する。
--
-- 例: @ANSIAction [1] 'A'@ → @CursorUp 1@
-- 例: @ANSIAction [31,40] 'm'@ → @SetAttributeMode [Foreground Red, Background Black]@
simplify :: TerminalAction -> TerminalAction
simplify (ANSIAction [] 'A') = CursorUp 1
simplify (ANSIAction [n] 'A') = CursorUp n
simplify (ANSIAction [] 'B')  = CursorDown 1
simplify (ANSIAction [n] 'B')  = CursorDown n
simplify (ANSIAction [] 'C') = CursorForward 1
simplify (ANSIAction [n] 'C') = CursorForward n
simplify (ANSIAction [] 'D') = CursorBackward 1
simplify (ANSIAction [n] 'D') = CursorBackward n
simplify (ANSIAction [n] 'G') = CursorAbsoluteColumn n
simplify (ANSIAction [n] 'd') = CursorAbsoluteRow n
simplify (ANSIAction [25] 'h') = ShowCursor True
simplify (ANSIAction [25] 'l') = ShowCursor False
simplify (ANSIAction [] 'H') = SetCursor 1 1
simplify (ANSIAction [] 'f') = SetCursor 1 1
simplify (ANSIAction [y,x] 'H') = SetCursor y x
simplify (ANSIAction [y,x] 'f') = SetCursor y x


simplify (ANSIAction [start, end] 'r') = SetScrollingRegion start end
-- CSI S = SU (Scroll Up): content moves UP = scrollTerminalDown
-- CSI T = SD (Scroll Down): content moves DOWN = scrollTerminalUp
simplify (ANSIAction [] 'S') = ScrollDown 1
simplify (ANSIAction [n] 'S') = ScrollDown n
simplify (ANSIAction [] 'T') = ScrollUp 1
simplify (ANSIAction [n] 'T') = ScrollUp n

simplify (ANSIAction [] 'm') = SetAttributeMode [ResetAllAttributes] 
simplify (ANSIAction attrModeNumbers 'm') = SetAttributeMode (parseAttributeModes attrModeNumbers)
simplify x = x

-- | SGR パラメータリストを 'AttributeMode' に変換する。
--
-- 256色 (38;5;N / 48;5;N) および TrueColor (38;2;R;G;B / 48;2;R;G;B) の
-- サブパラメータ構造を認識する。
parseAttributeModes :: [Int] -> [AttributeMode]
parseAttributeModes [] = []
-- 256色: 前景 (38;5;N)
parseAttributeModes (38:5:n:rest) = Foreground (Color256 (clamp256 n)) : parseAttributeModes rest
-- TrueColor: 前景 (38;2;R;G;B)
parseAttributeModes (38:2:r:g:b:rest) = Foreground (ColorRGB (clampW r) (clampW g) (clampW b)) : parseAttributeModes rest
-- 256色: 背景 (48;5;N)
parseAttributeModes (48:5:n:rest) = Background (Color256 (clamp256 n)) : parseAttributeModes rest
-- TrueColor: 背景 (48;2;R;G;B)
parseAttributeModes (48:2:r:g:b:rest) = Background (ColorRGB (clampW r) (clampW g) (clampW b)) : parseAttributeModes rest
-- その他: 単一パラメータを toEnum で変換
parseAttributeModes (n:rest) = toEnum n : parseAttributeModes rest

-- | 値を 0–255 にクランプ。
clamp256 :: Int -> Int
clamp256 = max 0 . min 255

-- | Int を Word8 にクランプ変換。
clampW :: Int -> Word8
clampW = fromIntegral . max 0 . min 255

parseString :: String -> [TerminalAction]
parseString str = fst (fromRight (parseANSI str))
                where fromRight :: Either a b -> b
                      fromRight (Right r) = r

-- | ANSI エスケープシーケンスをパースする。
--
-- 成功すると、パース済みの 'TerminalAction' リストと
-- 未消費の残り文字列を返す。インクリメンタルパースに対応。
--
-- >>> parseANSI "Hello\ESC[31mWorld"
-- Right ([CharInput 'H',...,SetAttributeMode [Foreground Red],...],"")
parseANSI :: String -> Either ParseError ([TerminalAction], String)
parseANSI s = parse (manyWithLeftover pSingle) "" s

-- | 'parseANSI' のデバッグ版。
-- 各アクションに、それを生成した元のバイト列を付与して返す。
parseANSIAnnotate :: String -> Either ParseError ([(TerminalAction, String)], String)
parseANSIAnnotate s = parse (manyWithLeftover $ annotate pSingle) "" s

pSingle :: Parser TerminalAction
pSingle = (pANSISequence <|> pChar) >>= return . simplify

pANSISequence :: Parser (TerminalAction)
pANSISequence = try (pStandardANSISeq)
    <|> try (pSetTerminalTitle)
    <|> try (string "\ESCM" >> return (ScrollUp 1))
    <|> try (string "\ESCD" >> return (ScrollDown 1))
    <|> try (string "\ESC=" >> return KeypadKeysApplicationsMode)
    <|> try (string "\ESC>" >> return KeypadKeysNumericMode)
    <|> try (string "\ESC(B" >> return Ignored)
    <|> try (string "\ESC#8" >> return Ignored)
    -- Catch invalid and not implemented sequences
    <|> try (string "\ESC" >> notFollowedBy (string "]0;") >> manyTill anyNonEscapeChar (letter <|> try (char '\ESC')) >> return Ignored)

pStandardANSISeq = do
    string "\ESC["
    optionMaybe (char '?')
    param <- optionMaybe pNumber
    params <- many (try (char ';' >> pNumber))
    c <- letter
    return $ ANSIAction (maybeToList param ++ params) c

pSetTerminalTitle = do
    string "\ESC]0;"
    title <- manyTill (satisfy (/= '\007')) (try (char '\007'))
    return (SetTerminalTitle title)

anyNonEscapeChar = satisfy (/= '\ESC')

pChar :: Parser (TerminalAction)
pChar = (anyNonEscapeChar >>= return . CharInput)

pNumber = read `fmap` (manyUpTo 1 6 digit)

main = do
    print $ parseANSI "wldjawlkdj1234\a\n\n\ESC[0m\ESC[1;6m\ESC[2K\ESC[A\n12\n"
    print $ parseANSI "|M}\210\195\238\ESC[;\171\&2`[ZZZ_`__a\\a]\\aaa`_Z]["
    print $ parseANSIAnnotate "\ESC[1;6m\ESC[2K\ESC[A\n12\n"

