module Text.EscapeArtistSpec.TestData (
                                TestCaseVE(..)
                              , allEscTestCases
                              , inheritTestCases
                              , escSingleTestCases
                              , nestedSumTestCases
                              , sumTestCases
                              , eqTestCases
                              , notEqTestCases
                              , monoidArgs
                              , monoidTestCases
                              , showTestCases
                              ) where

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Typeable (Typeable)
import Data.Word
import Text.EscapeArtist.Internal
import Text.EscapeArtist.Internal.Constants
import Test.QuickCheck

data TestCaseVE = forall a. (ToEscapable a) => TestCaseVE a String

-----------------------------------------------------------

openCloseCons = [
        (fgBlack,   defaultFgColor, FgBlack  ),
        (fgRed,     defaultFgColor, FgRed    ),
        (fgGreen,   defaultFgColor, FgGreen  ),
        (fgYellow,  defaultFgColor, FgYellow ),
        (fgBlue,    defaultFgColor, FgBlue   ),
        (fgMagenta, defaultFgColor, FgMagenta),
        (fgCyan,    defaultFgColor, FgCyan   ),
        (fgWhite,   defaultFgColor, FgWhite  ),

        (bgBlack,   defaultBgColor, BgBlack  ),
        (bgRed,     defaultBgColor, BgRed    ),
        (bgGreen,   defaultBgColor, BgGreen  ),
        (bgYellow,  defaultBgColor, BgYellow ),
        (bgBlue,    defaultBgColor, BgBlue   ),
        (bgMagenta, defaultBgColor, BgMagenta),
        (bgCyan,    defaultBgColor, BgCyan   ),
        (bgWhite,   defaultBgColor, BgWhite  ),

        (defaultFgColor, "", FgDefault  ),
        (defaultBgColor, "", BgDefault),
        ("",             "", Inherit  ),
        (defaultAll,     "", Default   ),

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
                TestCaseVE (cons v) e |
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

atomTestCases = [TestCaseVE (Atom v) e | (v, e) <- stringValueExp]

-- Other types of ToEscapable tests

data SomeToEscapable = A deriving (Eq, Show)

#if ! MIN_VERSION_base(4,8,0)
deriving instance Typeable SomeToEscapable
#endif

instance ToEscapable SomeToEscapable where
    toEscapable (A) = FgRed $ "A"

toEscTestCases = [TestCaseVE 5 "5", TestCaseVE "Some String" "Some String", TestCaseVE A (fgRed ++ "A" ++ defaultFgColor)]

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

inheritTestCases = [TestCaseVE (Underline $ Bright 6) (underlineOn ++ brightOn ++ "6" ++ brightOff ++ underlineOff)]

-----------------------------------------------------------

-- Sum tests

singleSum = Sum [FgRed 6, FgBlue "Color", FgYellow "Hello"]
singleSumExp = concat [fgRed, "6", defaultFgColor, fgBlue, "Color", defaultFgColor, fgYellow, "Hello", defaultFgColor]
sumTestCases = [TestCaseVE singleSum singleSumExp]

oneNestedSum = Bright $ FgRed $ Underline $ Sum [Underline $ FgYellow "Hello", FgGreen 1000 ]
oneNestedSumExp = concat [
    brightOn, fgRed, underlineOn, underlineOn, fgYellow, "Hello", defaultFgColor, underlineOff, underlineOff, defaultFgColor, brightOff,
    brightOn, fgRed, underlineOn, fgGreen, "1000", defaultFgColor, underlineOff, defaultFgColor, brightOff
    ]

twoNestedSum = Underline $ Sum [Underline $ FgYellow "Hello", Bright $ Sum [FgGreen 1000, FgBlue 999]]
twoNestedSumExp = concat [
    underlineOn, underlineOn, fgYellow, "Hello", defaultFgColor, underlineOff, underlineOff,
    underlineOn, brightOn, fgGreen, "1000", defaultFgColor, brightOff, underlineOff,
    underlineOn, brightOn, fgBlue,  "999",  defaultFgColor, brightOff, underlineOff
    ]

threeNestedSum = Inverse $ Sum [Underline $ FgYellow "Hello", Bright $ Sum [Inverse $ Sum [FgGreen 1000, FgCyan "C"], FgBlue 999]]
threeNestedSumExp = concat [
    inverseOn, underlineOn, fgYellow, "Hello", defaultFgColor, underlineOff, inverseOff,
    inverseOn, brightOn, inverseOn, fgGreen, "1000", defaultFgColor, inverseOff, brightOff, inverseOff,
    inverseOn, brightOn, inverseOn, fgCyan,  "C",    defaultFgColor, inverseOff, brightOff, inverseOff,
    inverseOn, brightOn, fgBlue, "999", defaultFgColor, brightOff, inverseOff
    ]

nestedSumTestCases = [
    TestCaseVE oneNestedSum oneNestedSumExp,
    TestCaseVE twoNestedSum twoNestedSumExp,
    TestCaseVE threeNestedSum threeNestedSumExp
    ]

-----------------------------------------------------------

-- Tests for putEsc and putEscLn

allEscTestCases = inheritTestCases ++ escSingleTestCases ++ sumTestCases ++ nestedSumTestCases

-----------------------------------------------------------

-- Equality tests

forAllCons = [
    FgBlack,
    FgRed,
    FgGreen,
    FgYellow,
    FgBlue,
    FgMagenta,
    FgCyan,
    FgWhite,
    BgBlack,
    BgRed,
    BgGreen,
    BgYellow,
    BgBlue,
    BgMagenta,
    BgCyan,
    BgWhite,
    FgDefault,
    BgDefault,
    Inherit,
    Default,
    Blink,
    BlinkOff,
    Bright,
    BrightOff,
    Underline,
    UnderlineOff,
    Inverse,
    InverseOff
    ]

fnConsSameValSame = (\v -> zipWith (\c v -> (c v, c v)) forAllCons $ repeat v)
fnCVCyc = (zipWith (\c v -> c v) forAllCons) . cycle
fnCVRep = (zipWith (\c v -> c v) forAllCons) . repeat

eqTestCases = fnConsSameValSame 'Z'
            ++ fnConsSameValSame "@%^&*&^%$#$%"
            ++ (fnConsSameValSame $ BSC.pack "4")
            ++ (fnConsSameValSame $ BSLC.pack "7")
            ++ (fnConsSameValSame $ T.pack "999")
            ++ (fnConsSameValSame $ TL.pack "Some Text")
            ++ fnConsSameValSame 6
            -- This case hadles different values which are the
            -- same when strings
            ++ zip (fnCVRep (3.5 :: Float)) (fnCVRep (3.5 :: Float))
            ++ [(Atom "6", Atom "6")]
            ++ [(Sum [FgRed 6], Sum [FgRed 6])]

li1 = fnCVCyc [1, 10, 5]
li2 = fnCVCyc [1000, 10000, 5000]

ls1 = fnCVCyc ["asdf", "@@#$%", "+_)(+_)(+_)(+_)()"]
ls2 = fnCVCyc ["99999", "^&*(9876(*&^9876))", "************"]

lc1 = fnCVCyc "QWERTY"
lc2 = fnCVCyc "ZXCVBN"

notEqConsSameValueNotTestCases = zip (li1 ++ ls1 ++ lc1) (li2 ++ ls2 ++ lc2)
                               ++ [(Atom "not", Atom "same")]
                               ++ [(Sum [FgBlue "not"], Sum [FgBlue "same"])]

forAllConsEnum = zip [1..] forAllCons

notEqConsNotValueSameTestCases = [(c1 v, c2 v) | (c1e, c1) <- forAllConsEnum,
                                                 (c2e, c2) <- forAllConsEnum,
                                                 let v = "Any Value",
                                                 c1e /= c2e
                                                 ]
                               ++ [(Atom "same", FgRed "same")]
                               ++ [(Sum [FgWhite "same"], FgRed "same")]

notEqTestCases = notEqConsSameValueNotTestCases
               ++ notEqConsNotValueSameTestCases

-----------------------------------------------------------

-- Monoid law test data

instance Arbitrary Escapable where
    arbitrary = oneof $ map return [
        FgRed 6,
        Underline $ Inverse $ FgGreen 10,
        oneNestedSum,
        twoNestedSum,
        threeNestedSum
        ]

monoidArgs = [
    FgRed 6,
    Underline $ Inverse $ FgGreen 10,
    singleSum,
    oneNestedSum,
    twoNestedSum,
    threeNestedSum
    ]

monoidTestCases = [(a, b, c) | a <- monoidArgs, b <- monoidArgs, c <- monoidArgs]

-----------------------------------------------------------

-- Show test data

showValueExp = [
    (FgBlack 1,       "FgBlack (1)"),
    (FgRed 2,         "FgRed (2)"),
    (FgGreen 3,       "FgGreen (3)"),
    (FgYellow 4,      "FgYellow (4)"),
    (FgBlue 5,        "FgBlue (5)"),
    (FgMagenta 6,     "FgMagenta (6)"),
    (FgCyan 7,        "FgCyan (7)"),
    (FgWhite 8,       "FgWhite (8)"),
    (BgBlack 9,       "BgBlack (9)"),
    (BgRed 10,        "BgRed (10)"),
    (BgGreen 11,      "BgGreen (11)"),
    (BgYellow 12,     "BgYellow (12)"),
    (BgBlue 13,       "BgBlue (13)"),
    (BgMagenta 14,    "BgMagenta (14)"),
    (BgCyan 15,       "BgCyan (15)"),
    (BgWhite 16,      "BgWhite (16)"),
    (FgDefault 17,    "FgDefault (17)"),
    (BgDefault 18,    "BgDefault (18)"),
    (Inherit 19,      "Inherit (19)"),
    (Default 20,       "Default (20)"),
    (Blink 21,        "Blink (21)"),
    (BlinkOff 22,     "BlinkOff (22)"),
    (Bright 23,       "Bright (23)"),
    (BrightOff 24,    "BrightOff (24)"),
    (Underline 25,    "Underline (25)"),
    (UnderlineOff 26, "UnderlineOff (26)"),
    (Inverse 27,      "Inverse (27)"),
    (InverseOff 28,   "InverseOff (28)"),

    (Sum [FgRed 6, FgBlue 3], "Sum [FgRed (6),FgBlue (3)]"),

    (Atom "text", "Atom \"text\"")
    ]
showTestCases = [TestCaseVE v e | (v, e) <- showValueExp]
