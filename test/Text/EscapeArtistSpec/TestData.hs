module Text.EscapeArtistSpec.TestData where

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

        (brightOn,    brightOff,    Bright   ),
        (underlineOn, underlineOff, Underline),
        (inverseOn,   inverseOff,   Inverse  ),
        (strikeOn,    strikeOff,    Strike   )
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

stringValueExp = [("ASDASDASD", "ASDASDASD"), ("%%$\":98^tug'kjgh\"", "%%$\":98^tug'kjgh\""), ("aaa\nggg\thhh\n", "aaa\nggg\thhh\n")] :: [(String, String)]
stringTestCases = genTestCases stringValueExp

modTestCases = intTestCases ++ integerTestCases ++ floatTestCases ++ doubleTestCases ++ stringTestCases



sumTestCases = let modifiers = map getEscapable intTestCases
                   expected = concat $ map getExpected intTestCases
                in [TestCase (Sum modifiers) expected]
