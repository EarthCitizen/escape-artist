module Text.EscapeArtistSpec (spec) where

import Control.Monad (forM_)
import Data.Monoid ((<>), mempty)
import System.IO.Silently
import Test.QuickCheck
import Test.Hspec
import Text.EscapeArtist.Constants
import Text.EscapeArtist.Internal
import Text.EscapeArtistSpec.TestData

spec :: Spec
spec = do
    describe "escToString" $ do
        context "when passed any instance of ToEscapable" $ do
            it "outputs contained valued with corresponding escape codes" $ forM_ escSingleTestCases $
                \(TestCase escapable expectation) -> do
                    escToString escapable `shouldBe` expectation
        context "when passed an Inherit" $ do
            it "outputs contained value with only parent escape codes" $ forM_ inheritedTestCases $ do
                \(TestCase escapable expectation) -> do
                    escToString escapable `shouldBe` expectation
        context "when passed a Sum" $ do
            it "outputs all contained values with the corresponding escape codes" $ forM_ sumTestCases $
                \(TestCase escapable expectation) -> do
                    escToString escapable `shouldBe` expectation
            it "outputs parent escape codes around each nested sum element" $ forM_ nestedSumTestCases $ do
                \(TestCase escapable expectation) -> do
                    escToString escapable `shouldBe` expectation
    describe "putEsc" $ do
        context "when passed any instance of ToEscapable" $ do
            it "outputs contained values with corresponding escape codes" $ forM_ allEscTestCases $
                \(TestCase escapable expectation) -> do
                    (out, result) <- capture $ putEsc $ escapable
                    out `shouldBe` expectation
                    result `shouldBe` ()
    describe "putEscLn" $ do
        context "when passed any instance of ToEscapable" $ do
            it "outputs contained values with corresponding escape codes and newline" $ forM_ allEscTestCases $
                \(TestCase escapable expectation) -> do
                    (out, result) <- capture $ putEscLn $ escapable
                    out `shouldBe` (expectation ++ ['\n'])
                    result `shouldBe` ()

    describe "Eq Escapable" $ do
        it "considers escapables equal when strings of contained values are same" $ forM_ allEqTestCases $ do
            \(TestCaseEq a b) -> a `shouldBe` b
        it "considers escapables not equal when strings of contained values are not same" $ forM_ allNotEqTestCases $ do
            \(TestCaseEq a b) -> a `shouldNotBe` b

    describe "Monoid Escapable" $ do
        it "obeys left identity" $ property $
            -- left identity
            -- mempty <> x = x
            \x -> mempty <> x `shouldBe` (x :: Escapable)
        it "obeys right identify" $ property $
            -- right identity
            -- x <> mempty = x
            \x -> x <> mempty `shouldBe` (x :: Escapable)
        it "obeys associativity" $ property $
            -- associativity
            -- (x <> y) <> z = x <> (y <> z)
            \x y z -> (x <> y) <> z `shouldBe` (x :: Escapable) <> ((y :: Escapable) <> (z :: Escapable))
