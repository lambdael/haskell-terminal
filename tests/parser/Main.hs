module Main where
import Data.Monoid
import Control.Applicative ((<$>))

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

import Terminal.Parser
import Terminal.Terminal
import Terminal.Types

-- |This section contains unit tests to validate the parser, e.g., to ensure
-- that the incoming character stream is correctly translated into
-- `TerminalAction`s. This fact is clearly expressed by the function signature
-- of the assertion function `parsesTo` which is used in the assertions below.
parsesTo :: String -> [TerminalAction] -> Assertion
parsesTo str is = let Right result = parseANSI str in result @?= (is, "")

testSetCursor = "A\ESC[H\ESC[2;2H"
        `parsesTo` [CharInput 'A', SetCursor 1 1, SetCursor 2 2]

testInvalidSetCursor = "\ESC[H\ESC[2;2;X\ESC[5;1H"
        `parsesTo` [SetCursor 1 1, Ignored, SetCursor 5 1]

testMoveCursor = "A\ESC[A\ESCA\ESC[10B"
        `parsesTo` [CharInput 'A', CursorUp 1, Ignored, CursorDown 10]

testCharInput = "A2$?"
        `parsesTo` [CharInput 'A', CharInput '2', CharInput '$', CharInput '?']

unitTests =
       [ testCase "testSetCursor" testSetCursor
       , testCase "testInvalidSetCursor" testInvalidSetCursor
       , testCase "testMoveCursor" testMoveCursor
       , testCase "testCharInput" testCharInput
       ]

-- |The second section of this file consists of QuickCheck properties to ensure
-- that the peraser is rebust again arbirtrary input. Therefore a InputStream
-- type is defined which represents the incoming character stream.
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
    let Right (x, s) = parseANSI str in
    length s < 15

propertyTests =
       [ testProperty "parseNotManyCharsLeftOver" prop_NotManyCharsLeftOver
       ]

-- Finally, execute the tests with the cabal test framework
main :: IO ()
main = defaultMainWithOpts (unitTests ++ propertyTests) mempty
