module Text.EscapeArtistSpec (spec) where

import Control.Monad (forM_)
import Data.Monoid ((<>), mempty)
import Text.EscapeArtist.Internal
import Text.EscapeArtistSpec.TestData
import System.IO.Silently (capture)
import Test.QuickCheck
import Test.Hspec (context, describe, it, shouldBe, shouldNotBe, Spec)

spec :: Spec
spec = do
    describe "escToString" $ do
        context "when passed any instance of ToEscapable" $ do
            it "outputs contained valued with corresponding escape codes" $ forM_ escSingleTestCases $
                \(TestCaseVE escapable expectation) -> do
                    escToString escapable `shouldBe` expectation
        context "when passed an Inherit" $ do
            it "outputs contained value with only parent escape codes" $ forM_ inheritTestCases $ do
                \(TestCaseVE escapable expectation) -> do
                    escToString escapable `shouldBe` expectation
        context "when passed a Sum" $ do
            it "outputs all contained values with the corresponding escape codes" $ forM_ sumTestCases $
                \(TestCaseVE escapable expectation) -> do
                    escToString escapable `shouldBe` expectation
            it "outputs parent escape codes around each nested sum element" $ forM_ nestedSumTestCases $ do
                \(TestCaseVE escapable expectation) -> do
                    escToString escapable `shouldBe` expectation

    describe "putEsc" $ do
        context "when passed any instance of ToEscapable" $ do
            it "outputs contained values with corresponding escape codes" $ forM_ allEscTestCases $
                \(TestCaseVE escapable expectation) -> do
                    (out, result) <- capture $ putEsc $ escapable
                    out `shouldBe` expectation
                    result `shouldBe` ()

    describe "putEscLn" $ do
        context "when passed any instance of ToEscapable" $ do
            it "outputs contained values with corresponding escape codes and newline" $ forM_ allEscTestCases $
                \(TestCaseVE escapable expectation) -> do
                    (out, result) <- capture $ putEscLn $ escapable
                    out `shouldBe` (expectation ++ ['\n'])
                    result `shouldBe` ()

    describe "^$" $ do
        it "produces the same result as $" $ do
            (FgRed ^$ Underline ^$ Blink 6) `shouldBe` (FgRed $ Underline $ Blink 6)
        it "gets processed before <>" $ do
            (FgRed ^$ Underline 5 <> FgBlue 3 <> FgYellow 9) `shouldBe` (Sum [FgRed (Underline 5), FgBlue 3, FgYellow 9])

    describe "/<>/" $ do
        it "produces the same result as <>" $ do
            (FgRed "A" /<>/ FgBlue 10) `shouldBe` (FgRed "A" <> FgBlue 10)
        it "wraps non-Escapable values in Inherit" $ do
            (5 /<>/ FgRed "A" /<>/ 'C') `shouldBe` (Sum [Inherit 5, FgRed "A", Inherit 'C']) 

    describe "Eq Escapable" $ do
        it "considers escapables equal when constructors and contained values are same" $ forM_ eqTestCases $ do
            \(a, b) -> a `shouldBe` b
        it "considers escapables not equal when constructors or contained values are not same" $ forM_ notEqTestCases $ do
            \(a, b) -> a `shouldNotBe` b

    describe "Monoid Escapable" $ do
        it "obeys left identity" $ forM_ monoidArgs $
            -- left identity
            -- mempty <> x = x
            \x -> mempty <> x `shouldBe` (x :: Escapable)
        it "obeys right identify" $ forM_ monoidArgs $
            -- right identity
            -- x <> mempty = x
            \x -> x <> mempty `shouldBe` (x :: Escapable)
        it "obeys associativity" $ forM_ monoidTestCases $
            -- associativity
            -- (x <> y) <> z = x <> (y <> z)
            \(x, y, z) -> (x <> y) <> z `shouldBe` (x :: Escapable) <> ((y :: Escapable) <> (z :: Escapable))

    describe "Show Escapable" $ do
        it "returns the String representation of the given value" $ forM_ showTestCases $ do
            \(TestCaseVE escapable expectation) -> show escapable `shouldBe` expectation
