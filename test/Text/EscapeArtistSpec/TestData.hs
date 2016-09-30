module Text.EscapeArtistSpec.TestData (
                                TestCase(..)
                              , TestCaseEq(..)
                              , allEscTestCases
                              , inheritTestCases
                              , escSingleTestCases
                              , nestedSumTestCases
                              , sumTestCases
                              , allEqTestCases
                              , allNotEqTestCases
                              , showTestCases
                              ) where

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Word
import Test.QuickCheck
import Text.EscapeArtist.Internal
import Text.EscapeArtist.Constants

data TestCase = forall a. (ToEscapable a) => TestCase a String
data TestCaseEq = TestCaseEq Escapable Escapable

-----------------------------------------------------------

openCloseCons = [
        (black,   defaultColor, Black  ),
        (red,     defaultColor, Red    ),
        (green,   defaultColor, Green  ),
        (yellow,  defaultColor, Yellow ),
        (blue,    defaultColor, Blue   ),
        (magenta, defaultColor, Magenta),
        (cyan,    defaultColor, Cyan   ),
        (white,   defaultColor, White  ),

        (bgblack,   defaultBgColor, BgBlack  ),
        (bgred,     defaultBgColor, BgRed    ),
        (bggreen,   defaultBgColor, BgGreen  ),
        (bgyellow,  defaultBgColor, BgYellow ),
        (bgblue,    defaultBgColor, BgBlue   ),
        (bgmagenta, defaultBgColor, BgMagenta),
        (bgcyan,    defaultBgColor, BgCyan   ),
        (bgwhite,   defaultBgColor, BgWhite  ),

        (defaultColor,   "", Default  ),
        (defaultBgColor, "", BgDefault),
        ("",             "", Inherit  ),
        (reset,          "", Normal   ),

        (blinkOn,      blinkOff,     Blink       ),
        (blinkOff,     "",           BlinkOff    ),
        (brightOn,     brightOff,    Bright      ),
        (brightOff,    "",           BrightOff   ),
        (underlineOn,  underlineOff, Underline   ),
        (underlineOff, "",           UnderlineOff),
        (inverseOn,    inverseOff,   Inverse     ),
        (inverseOff,   "",           InverseOff  )
        ]

genTestCases valueList = [
                TestCase (cons v) e |
                (open, close, cons) <- openCloseCons,
                (v, vs) <- valueList,
                let e = open ++ vs ++ close
                ]

-- Atom terminating type tests

charValueExp = [('5', "5"), ('X', "X"), ('@', "@")] :: [(Char, String)]
charTestCases = genTestCases charValueExp

bsValueExp = [(BSC.pack "ASDASDASD", "ASDASDASD"), (BSC.pack "%%$\":98^tug'kjgh\"", "%%$\":98^tug'kjgh\""), (BSC.pack "aaa\nggg\thhh\n", "aaa\nggg\thhh\n")]
bsTestCases = genTestCases bsValueExp

bslValueExp = [(BSLC.pack "ASDASDASD", "ASDASDASD"), (BSLC.pack "%%$\":98^tug'kjgh\"", "%%$\":98^tug'kjgh\""), (BSLC.pack "aaa\nggg\thhh\n", "aaa\nggg\thhh\n")]
bslTestCases = genTestCases bslValueExp

stringValueExp = [("ASDASDASD", "ASDASDASD"), ("%%$\":98^tug'kjgh\"", "%%$\":98^tug'kjgh\""), ("aaa\nggg\thhh\n", "aaa\nggg\thhh\n")]
stringTestCases = genTestCases stringValueExp

textValueExp = [(T.pack "ASDASDASD", "ASDASDASD"), (T.pack "%%$\":98^tug'kjgh\"", "%%$\":98^tug'kjgh\""), (T.pack "aaa\nggg\thhh\n", "aaa\nggg\thhh\n")]
textTestCases = genTestCases textValueExp

textLazyValueExp = [(TL.pack "ASDASDASD", "ASDASDASD"), (TL.pack "%%$\":98^tug'kjgh\"", "%%$\":98^tug'kjgh\""), (TL.pack "aaa\nggg\thhh\n", "aaa\nggg\thhh\n")]
textLazyTestCases = genTestCases textLazyValueExp

intValueExp = [(500, "500"), ((-4000), "-4000"), (9999999, "9999999")] :: [(Int, String)]
intTestCases = genTestCases intValueExp

integerValueExp = [(500, "500"), ((-4000), "-4000"), (9999999, "9999999")] :: [(Integer, String)]
integerTestCases = genTestCases integerValueExp

wValueExp = [(500, "500"), ((4), "4"), (999, "999")] :: [(Word, String)]
wTestCases = genTestCases wValueExp

w8ValueExp = [(200, "200"), (4, "4"), (99, "99")] :: [(Word8, String)]
w8TestCases = genTestCases w8ValueExp

w16ValueExp = [(500, "500"), ((4), "4"), (999, "999")] :: [(Word16, String)]
w16TestCases = genTestCases w16ValueExp

w32ValueExp = [(500, "500"), ((400), "400"), (99999, "99999")] :: [(Word32, String)]
w32TestCases = genTestCases w32ValueExp

w64ValueExp = [(500, "500"), ((400), "400"), (99999, "99999")] :: [(Word64, String)]
w64TestCases = genTestCases w64ValueExp

floatValueExp = [(4.5, "4.5"), (0.0001, "1.0e-4"), (-0.003, "-3.0e-3")] :: [(Float, String)]
floatTestCases = genTestCases floatValueExp

doubleValueExp = [(4.5, "4.5"), (0.0001, "1.0e-4"), (-0.003, "-3.0e-3")] :: [(Double, String)]
doubleTestCases = genTestCases doubleValueExp

-- Atom tests

atomTestCases = [TestCase (Atom v) e | (v, e) <- stringValueExp]

-- Other types of ToEscapable tests

data SomeToEscapable = A deriving (Show)

instance ToEscapable SomeToEscapable where
    toEscapable (A) = Red $ "A"

toEscTestCases = [TestCase 5 "5", TestCase "Some String" "Some String", TestCase A (red ++ "A" ++ defaultColor)]

-- Put them all together to run through the same test

escSingleTestCases = charTestCases
                   ++ bsTestCases
                   ++ bslTestCases
                   ++ stringTestCases
                   ++ textTestCases
                   ++ textLazyTestCases
                   ++ atomTestCases
                   ++ toEscTestCases
                   ++ intTestCases
                   ++ integerTestCases
                   ++ wTestCases
                   ++ w8TestCases
                   ++ w16TestCases
                   ++ w32TestCases
                   ++ w64TestCases
                   ++ floatTestCases
                   ++ doubleTestCases

-----------------------------------------------------------

-- Inherit tests

inheritTestCases = [TestCase (Underline $ Bright 6) (underlineOn ++ brightOn ++ "6" ++ brightOff ++ underlineOff)]

-----------------------------------------------------------

-- Sum tests

sumTestCases1esc = Sum [Red 6, Blue "Color", Yellow "Hello"]
sumTestCases1exp = concat [red, "6", defaultColor, blue, "Color", defaultColor, yellow, "Hello", defaultColor]
sumTestCases = [TestCase sumTestCases1esc sumTestCases1exp]

oneNestedSum = Bright $ Red $ Underline $ Sum [Underline $ Yellow "Hello", Green 1000 ]
oneNestedSumExp = concat [
    brightOn, red, underlineOn, underlineOn, yellow, "Hello", defaultColor, underlineOff, underlineOff, defaultColor, brightOff,
    brightOn, red, underlineOn, green, "1000", defaultColor, underlineOff, defaultColor, brightOff
    ]

twoNestedSum = Underline $ Sum [Underline $ Yellow "Hello", Bright $ Sum [Green 1000, Blue 999]]
twoNestedSumExp = concat [
    underlineOn, underlineOn, yellow, "Hello", defaultColor, underlineOff, underlineOff,
    underlineOn, brightOn, green, "1000", defaultColor, brightOff, underlineOff,
    underlineOn, brightOn, blue,  "999",  defaultColor, brightOff, underlineOff
    ]

threeNestedSum = Inverse $ Sum [Underline $ Yellow "Hello", Bright $ Sum [Inverse $ Sum [Green 1000, Cyan "C"], Blue 999]]
threeNestedSumExp = concat [
    inverseOn, underlineOn, yellow, "Hello", defaultColor, underlineOff, inverseOff,
    inverseOn, brightOn, inverseOn, green, "1000", defaultColor, inverseOff, brightOff, inverseOff,
    inverseOn, brightOn, inverseOn, cyan,  "C",    defaultColor, inverseOff, brightOff, inverseOff,
    inverseOn, brightOn, blue, "999", defaultColor, brightOff, inverseOff
    ]

nestedSumTestCases = [
    TestCase oneNestedSum oneNestedSumExp,
    TestCase twoNestedSum twoNestedSumExp,
    TestCase threeNestedSum threeNestedSumExp
    ]

-----------------------------------------------------------

-- Tests for putEsc and putEscLn

allEscTestCases = inheritTestCases ++ escSingleTestCases ++ sumTestCases ++ nestedSumTestCases

-----------------------------------------------------------

-- Equality tests

eq1List = [Red 6, Red "6", Red $ BSC.pack "6", Red $ BSLC.pack "6", Red $ T.pack "6", Red $ TL.pack "6"]
eq1TestCases = [TestCaseEq x y | x <- eq1List, y <- eq1List]

eq2List = [Blue (3.5 :: Float), Blue (3.5 :: Double), Blue "3.5", Blue $ T.pack "3.5"]
eq2testCases = [TestCaseEq x y | x <- eq2List, y <- eq2List]

allEqTestCases = eq1TestCases ++ eq2testCases

notEqList11 = [Yellow 10, Blue "100", Green (3.4 :: Float), Cyan "3000", White 'W']
notEqList12 = [Yellow 11, Blue "101", Green (3.5 :: Float), Cyan "3001", White 'Z']
notEq1TestCases = [TestCaseEq x y | x <- notEqList11, y <- notEqList12]

notEqList21 = [White 1, White 2, White 3, White 4, White 5]
notEqList22 = [White 11, White 12, White 13, White 14, White 15]
notEq2TestCases = [TestCaseEq x y | x <- notEqList21, y <- notEqList22]

allNotEqTestCases = notEq1TestCases ++ notEq2TestCases

-----------------------------------------------------------

-- Monoid law test data

instance Arbitrary Escapable where
    arbitrary = oneof $ map return [
        Red 6,
        Underline $ Inverse $ Green 10,
        oneNestedSum,
        twoNestedSum,
        threeNestedSum
        ]

-----------------------------------------------------------

-- Show test data

showValueExp = [
    (Black 1,         "Black (1)"),
    (Red 2,           "Red (2)"),
    (Green 3,         "Green (3)"),
    (Yellow 4,        "Yellow (4)"),
    (Blue 5,          "Blue (5)"),
    (Magenta 6,       "Magenta (6)"),
    (Cyan 7,          "Cyan (7)"),
    (White 8,         "White (8)"),
    (BgBlack 9,       "BgBlack (9)"),
    (BgRed 10,        "BgRed (10)"),
    (BgGreen 11,      "BgGreen (11)"),
    (BgYellow 12,     "BgYellow (12)"),
    (BgBlue 13,       "BgBlue (13)"),
    (BgMagenta 14,    "BgMagenta (14)"),
    (BgCyan 15,       "BgCyan (15)"),
    (BgWhite 16,      "BgWhite (16)"),
    (Default 17,      "Default (17)"),
    (BgDefault 18,    "BgDefault (18)"),
    (Inherit 19,      "Inherit (19)"),
    (Normal 20,       "Normal (20)"),
    (Blink 21,        "Blink (21)"),
    (BlinkOff 22,     "BlinkOff (22)"),
    (Bright 23,       "Bright (23)"),
    (BrightOff 24,    "BrightOff (24)"),
    (Underline 25,    "Underline (25)"),
    (UnderlineOff 26, "UnderlineOff (26)"),
    (Inverse 27,      "Inverse (27)"),
    (InverseOff 28,   "InverseOff (28)"),

    (Sum [Red 6, Blue 3], "Sum [Red (6),Blue (3)]"),

    (Atom "text", "Atom \"text\"")
    ]
showTestCases = [TestCase v e | (v, e) <- showValueExp]
