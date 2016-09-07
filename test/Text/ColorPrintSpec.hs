module Text.ColorPrintSpec (spec) where

import Control.Monad (forM_)
import System.IO.Silently
import Test.QuickCheck
import Test.Hspec
import Text.ColorPrint
import Text.ColorPrint.Internal
import Text.ColorPrint.TestData

spec :: Spec
spec = do
    describe "putColor" $ do
        context "when passed a visual modifier" $ do
            it "outputs the contained valued in the corresponding modification" $ forM_ modTestCases $
                \(TestCase modifier expectation) -> do
                    (out, result) <- capture $ putColor $ modifier
                    out `shouldBe` expectation
                    result `shouldBe` ()
    describe "putColorLn" $ do
        context "when passed a visual modifier" $ do
            it "outputs the contained valued in the corresponding modification" $ forM_ modTestCases $
                \(TestCase modifier expectation) -> do
                    (out, result) <- capture $ putColorLn $ modifier
                    out `shouldBe` (expectation ++ ['\n'])
                    result `shouldBe` ()
