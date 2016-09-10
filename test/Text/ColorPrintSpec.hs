module Text.ColorPrintSpec (spec) where

import Control.Monad (forM_)
import System.IO.Silently
import Test.QuickCheck
import Test.Hspec
import Text.ColorPrint
import Text.ColorPrint.Internal
import Text.ColorPrint.TestData

g = Green 1000

spec :: Spec
spec = do
    describe "putColor" $ do
        context "when passed a visual modifier" $ do
            it "outputs the contained valued with the corresponding modification" $ forM_ modTestCases $
                \(TestCase modifier expectation) -> do
                    (out, result) <- capture $ putColor $ modifier
                    out `shouldBe` expectation
                    result `shouldBe` ()
        context "when passed a Sum modifier" $ do
            it "outputs all of the contained values with the corresponding modifications" $ forM_ sumTestCases $
                \(TestCase modifier expectation) -> do
                    (out, result) <- capture $ putColor $ modifier
                    out `shouldBe` expectation
                    result `shouldBe` ()
            -- it "outputs the preceding modifiers at the beginning of each branch" $ do
            --     let modifier = Bright $ Red $ Underline $ Sum [ Underline $ Yellow "Hello", Green 1000 ]
            --         expectation = concat [
            --                         brightOn, red, underlineOn,
            --                         underlineOn, yellow, "Hello", defaultColor, underlineOff,
            --                         brightOn, red, underlineOn,
            --                         green, "1000", defaultColor,
            --                         underlineOff, defaultColor, brightOff
            --                         ]
            --     (out, result) <- capture $ putColor $ modifier
            --     out `shouldBe` expectation
            --     result `shouldBe` ()
    describe "putColorLn" $ do
        context "when passed a visual modifier" $ do
            it "outputs the contained valued in the corresponding modification" $ forM_ modTestCases $
                \(TestCase modifier expectation) -> do
                    (out, result) <- capture $ putColorLn $ modifier
                    out `shouldBe` (expectation ++ ['\n'])
                    result `shouldBe` ()
