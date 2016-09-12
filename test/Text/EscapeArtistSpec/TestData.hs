module Text.EscapeArtistSpec.TestData (
                                TestCase(..)
                              , allEscTestCases
                              , contextTestCases
                              , escSingleTestCases
                              , nestedSumTestCases
                              , sumTestCases
                              ) where

import Test.QuickCheck
import Text.EscapeArtist.Internal
import Text.EscapeArtist.Constants

data TestCase = TestCase { getEscapable :: Escapable, getExpected :: String }

openCloseCons = [
        (black,   defaultColor, Black  ),
        (red,     defaultColor, Red    ),
        (green,   defaultColor, Green  ),
        (yellow,  defaultColor, Yellow ),
        (magenta, defaultColor, Magenta),
        (cyan,    defaultColor, Cyan   ),
        (white,   defaultColor, White  ),

        (bgblack,   defaultBgColor, BgBlack  ),
        (bgred,     defaultBgColor, BgRed    ),
        (bggreen,   defaultBgColor, BgGreen  ),
        (bgyellow,  defaultBgColor, BgYellow ),
        (bgmagenta, defaultBgColor, BgMagenta),
        (bgcyan,    defaultBgColor, BgCyan   ),
        (bgwhite,   defaultBgColor, BgWhite  ),

        (defaultColor,   "", Default  ),
        (defaultBgColor, "", BgDefault),
        ("",             "", Inherited),
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

intValueExp = [(500, "500"), ((-4000), "-4000"), (9999999, "9999999")] :: [(Int, String)]
intTestCases = genTestCases intValueExp

integerValueExp = [(500, "500"), ((-4000), "-4000"), (9999999, "9999999")] :: [(Integer, String)]
integerTestCases = genTestCases integerValueExp

floatValueExp = [(4.5, "4.5"), (0.0001, "1.0e-4"), (-0.003, "-3.0e-3")] :: [(Float, String)]
floatTestCases = genTestCases floatValueExp

doubleValueExp = [(4.5, "4.5"), (0.0001, "1.0e-4"), (-0.003, "-3.0e-3")] :: [(Double, String)]
doubleTestCases = genTestCases doubleValueExp

stringValueExp = [("ASDASDASD", "ASDASDASD"), ("%%$\":98^tug'kjgh\"", "%%$\":98^tug'kjgh\""), ("aaa\nggg\thhh\n", "aaa\nggg\thhh\n")]
stringTestCases = genTestCases stringValueExp

textTestCases = [TestCase (Text v) e | (v, e) <- stringValueExp]

escSingleTestCases = intTestCases ++ integerTestCases ++ floatTestCases ++ doubleTestCases ++ stringTestCases ++ textTestCases

contextTestCases = [TestCase (Underline $ Bright 6) (underlineOn ++ brightOn ++ "6" ++ brightOff ++ underlineOff)]

sumTestCases = let escapables = map getEscapable intTestCases
                   expected = concat $ map getExpected intTestCases
                in [TestCase (Sum escapables) expected]

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

allEscTestCases = contextTestCases ++ escSingleTestCases ++ sumTestCases ++ nestedSumTestCases

instance Arbitrary Escapable where
    arbitrary = oneof $ map return [
        Red 6,
        Underline $ Inverse $ Green 10,
        oneNestedSum,
        twoNestedSum,
        threeNestedSum
        ]
