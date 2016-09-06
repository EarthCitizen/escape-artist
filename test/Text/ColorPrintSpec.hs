{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}

module Text.ColorPrintSpec (spec) where

import Control.Monad (forM_)
import System.IO.Silently
import Test.QuickCheck
import Test.Hspec
import Text.ColorPrint
import Text.ColorPrint.Internal

-- data ValueExp = forall a. (ToString a) => ValueExp (a -> Modifier)
-- data OpenCloseCons = forall a. (ToString a) => OpenCloseCons String String (a -> Modifier)

type IntColor = (Int -> Modifier)
type StringColor = (String -> Modifier)

-- black   = "\x1B[30m"
-- red     = "\x1B[31m"
-- green   = "\x1B[32m"
-- yellow  = "\x1B[33m"
-- blue    = "\x1B[34m"
-- magenta = "\x1B[35m"
-- cyan    = "\x1B[36m"
-- white   = "\x1B[37m"

intOpenCloseCons = [
        (black,   defaultColor, (Black :: IntColor)),
        (red,     defaultColor, (Red :: IntColor)),
        (green,   defaultColor, (Green :: IntColor)),
        (yellow,  defaultColor, (Yellow :: IntColor)),
        (magenta, defaultColor, (Magenta :: IntColor)),
        (cyan,    defaultColor, (Cyan :: IntColor)),
        (white,   defaultColor, (White :: IntColor))
        ]

intValueExp = [(500, "500"), ((-4000), "-4000"), (9999999, "9999999")] :: [(Int, String)]
-- intColors = [Black, Red, Green,  ] :: [Int -> Modifier]

-- openCloseCons = [("one", "two", Black)]

-- openCloseCons = [
--     OpenCloseCons black   defaultColor Black,
--     OpenCloseCons red     defaultColor Red,
--     OpenCloseCons green   defaultColor Green,
--     OpenCloseCons yellow  defaultColor Yellow,
--     OpenCloseCons magenta defaultColor Magenta,
--     OpenCloseCons cyan    defaultColor Cyan,
--     OpenCloseCons white   defaultColor White
--     ] :: [OpenCloseCons]

-- forEach :: String -> [a] -> Spec
-- forEach s as = it s as

spec :: Spec
spec = do
    describe "putColor" $ do
        -- it "does stuff" $ forEach [1..10] $
        --     \x -> print x
        -- -- context "when given all" $ do
        -- --     it "tests all colors" $ do
        -- --         -- (open, close, cons) <- intOpenCloseCons
        -- --         --(v, e) <- return intValueExp
        -- --         --(out, result) <- capture $ putColor $ Black (500::Int)
        -- --         -- x <- return $ map return [1..5]
        -- --         -- y <- x
        -- --         [1..5] >>= print
        --         -- True `shouldBe` True
        --         -- out `shouldBe` e
        --         -- result `shouldBe` ()
        context "when passed Black" $ do
            it "outputs the contained value in black" $ do
                (out, result) <- capture $ putColor $ Black (500::Int)
                out `shouldBe` "\x1B[30m500\x1B[39m"
                result `shouldBe` ()
        context "when passed Red" $ do
            it "outputs the contained value in red" $ do
                (out, result) <- capture $ putColor $ Red (404::Integer)
                out `shouldBe` "\x1B[31m404\x1B[39m"
                result `shouldBe` ()
        context "when passed Green" $ do
            it "outputs the contained value in green" $ do
                (out, result) <- capture $ putColor $ Green "I am a green color"
                out `shouldBe` "\x1B[32mI am a green color\x1B[39m"
                result `shouldBe` ()
        context "when passed Yellow" $ do
            it "outputs the contained value in yellow" $ do
                (out, result) <- capture $ putColor $ Yellow 'Y'
                out `shouldBe` "\x1B[33mY\x1B[39m"
                result `shouldBe` ()
        context "when passed Blue" $ do
            it "outputs the contained value in blue" $ do
                (out, result) <- capture $ putColor $ Blue (4.5234::Double)
                out `shouldBe` "\x1B[34m4.5234\x1B[39m"
                result `shouldBe` ()
        context "when passed Magenta" $ do
            it "outputs the contained value in magenta" $ do
                (out, result) <- capture $ putColor $ Magenta (0.00001::Float)
                out `shouldBe` "\x1B[35m1.0e-5\x1B[39m"
                result `shouldBe` ()
        context "when passed Cyan" $ do
            it "outputs the contained value in cyan" $ do
                (out, result) <- capture $ putColor $ Cyan (-4)
                out `shouldBe` "\x1B[36m-4\x1B[39m"
                result `shouldBe` ()
        context "when passed White" $ do
            it "outputs the contained value in white" $ do
                (out, result) <- capture $ putColor $ White 509
                out `shouldBe` "\x1B[37m509\x1B[39m"
                result `shouldBe` ()

        context "when passed Default" $ do
            it "outputs the contained value in default" $ do
                (out, result) <- capture $ putColor $ Default 509
                out `shouldBe` "\x1B[39m509"
                result `shouldBe` ()

        context "when passed BgBlack" $ do
            it "outputs the contained value in black" $ do
                (out, result) <- capture $ putColor $ BgBlack (500::Int)
                out `shouldBe` "\x1B[40m500\x1B[49m"
                result `shouldBe` ()
        context "when passed BgRed" $ do
            it "outputs the contained value with red background" $ do
                (out, result) <- capture $ putColor $ BgRed (404::Integer)
                out `shouldBe` "\x1B[41m404\x1B[49m"
                result `shouldBe` ()
        context "when passed BgGreen" $ do
            it "outputs the contained value with green background" $ do
                (out, result) <- capture $ putColor $ BgGreen "I am a green color"
                out `shouldBe` "\x1B[42mI am a green color\x1B[49m"
                result `shouldBe` ()
        context "when passed BgYellow" $ do
            it "outputs the contained value with yellow background" $ do
                (out, result) <- capture $ putColor $ BgYellow 'Y'
                out `shouldBe` "\x1B[43mY\x1B[49m"
                result `shouldBe` ()
        context "when passed BgBlue" $ do
            it "outputs the contained value with blue background" $ do
                (out, result) <- capture $ putColor $ BgBlue (4.5234::Double)
                out `shouldBe` "\x1B[44m4.5234\x1B[49m"
                result `shouldBe` ()
        context "when passed BgMagenta" $ do
            it "outputs the contained value with magenta background" $ do
                (out, result) <- capture $ putColor $ BgMagenta (0.00001::Float)
                out `shouldBe` "\x1B[45m1.0e-5\x1B[49m"
                result `shouldBe` ()
        context "when passed BgCyan" $ do
            it "outputs the contained value with cyan background" $ do
                (out, result) <- capture $ putColor $ BgCyan (-4)
                out `shouldBe` "\x1B[46m-4\x1B[49m"
                result `shouldBe` ()
        context "when passed BgWhite" $ do
            it "outputs the contained value with white background" $ do
                (out, result) <- capture $ putColor $ BgWhite 509
                out `shouldBe` "\x1B[47m509\x1B[49m"
                result `shouldBe` ()

        context "when passed BgDefault" $ do
            it "outputs the contained value with default background" $ do
                (out, result) <- capture $ putColor $ BgDefault 509
                out `shouldBe` "\x1B[49m509"
                result `shouldBe` ()
