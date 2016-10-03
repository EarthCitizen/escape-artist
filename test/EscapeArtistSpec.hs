module EscapeArtistSpec (spec) where

import Control.Monad (forM_)
import Data.Monoid ((<>), mempty)
import EscapeArtist.Constants
import EscapeArtist.Internal
import EscapeArtistSpec.TestData
import System.IO.Silently
import Test.QuickCheck
import Test.Hspec

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
            (Red ^$ Underline ^$ Blink 6) `shouldBe` (Red $ Underline $ Blink 6)
        it "gets processed before <>" $ do
            (Red ^$ Underline 5 <> Blue 3 <> Yellow 9) `shouldBe` (Sum [Red (Underline 5), Blue 3, Yellow 9])

    describe "Eq Escapable" $ do
        it "considers escapables equal when constructors and strings of contained values are same" $ forM_ eqTestCases $ do
            \(a, b) -> a `shouldBe` b
        it "considers escapables not equal when constructors or strings of contained values are not same" $ forM_ notEqTestCases $ do
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
