-- General parsec helpers
-- | Parsec の汎用コンビネータ。
--
-- ANSI パーサーで使用するユーティリティ関数を提供する。
module Terminal.ParserUtils where
import Text.Parsec
import Text.Parsec.String
import Data.Maybe (catMaybes)

-- |Apply parser p and return its result together with the string that has been
-- parsed.
-- | パーサーの結果に、消費した入力文字列を付与する。
-- デバッグ時に各アクションの元のバイト列を確認するのに使用。
annotate p = do
    before <- getInput
    result <- p
    after <- getInput
    return (result, take (length before - length after) before)

-- |Apply parser p as often as possible and return the matches together with the
-- bytes that are not successfully parsed (that are left over)
-- | パーサーを繰り返し適用し、結果と未消費の入力を返す。
manyWithLeftover p = do
    x <- many p
    i <- getInput
    return (x, i)

-- |Apply parser p at least n and up to m times
-- | @manyUpTo n m p@ — パーサー @p@ を最少 @n@ 回、最大 @m@ 回適用する。
manyUpTo n m p = do
    first <- count n p
    rest <- count (m - n) (optionMaybe p) 
    return (first ++ (catMaybes rest))

