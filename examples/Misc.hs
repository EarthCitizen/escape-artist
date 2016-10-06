{-# LANGUAGE ExtendedDefaultRules #-}

import Data.Monoid ((<>))
import Text.EscapeArtist
import Data.List (intersperse)
import Text.Regex

instance (ToEscapable a) => ToEscapable (Maybe a) where
    toEscapable (Just a) = FgGreen "Just" <> Inherit " " <> (Bright $ FgYellow a)
    toEscapable (Nothing) = Inverse $ FgRed "Nothing"

alterString :: String -> Escapable
alterString [] = FgDefault ""
alterString a  = mconcat $ zipWith (\c v -> c v) (cycle [Inverse . FgRed, FgRed, Inverse . FgBlue, FgRed]) a

rainbowString :: String -> Escapable
rainbowString [] = Inherit ""
rainbowString a = mconcat $ zipWith (\ c v -> c v) (cycle [FgRed, FgBlue, FgYellow, FgCyan, FgMagenta]) a

takeFromZip :: Escapable
takeFromZip = mconcat $ take 10 $ zipWith (<>) (cycle [alterString "Zip"]) (cycle [rainbowString "Rainbow"])

posNeg :: (Integral a, ToEscapable a) => a -> Escapable
posNeg a | a < 0 = FgRed a
         | a == 0 = Inherit a
         | otherwise = FgGreen a

replaceNumbers :: String -> String
replaceNumbers searchIn = subRegex (mkRegex "([0-9]+)") searchIn (escToString $ FgRed "\\1")

numberAts :: Escapable
numberAts =  mconcat $ intersperse (Inherit "@") $ map FgDefault [1..10]

main = do
    putStrLn "Numbers in FgRed"
    putStrLn "=============="
    putEscLn $ replaceNumbers "7 times 3 is 21"
    putStrLn ""
    putStrLn "Negative Numbers in red, Positive Numbers in green"
    putStrLn "=================================================="
    putEscLn $ mconcat $ intersperse (Inherit " ") $ map posNeg [-10..10]
    putStrLn ""
    putStrLn "@ Inherits Parent Color"
    putStrLn "======================="
    putEscLn $ FgYellow $ numberAts
    putEscLn $ FgCyan $ numberAts
    putStrLn ""
    putStrLn "Decorate Existing Data Types"
    putStrLn "============================"
    putEscLn $ Underline $ Just 10
    putEscLn $ Just 100
    putEscLn $ Inverse $ Underline $ Just 1000
    putEscLn (Nothing :: Maybe String)
    putStrLn ""
    putStrLn "Zip and Combine"
    putStrLn "==============="
    putEscLn $ rainbowString "Hello World!"
    putEscLn $ alterString "/\\/\\/\\/\\/\\/\\/\\/\\/\\/\\"
    putEscLn $ takeFromZip
