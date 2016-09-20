{-# LANGUAGE ExtendedDefaultRules #-}

import Data.Monoid ((<>))
import Text.EscapeArtist
import Data.List (intersperse)
import Text.Regex

instance (ToEscapable a) => ToEscapable (Maybe a) where
    toEscapable (Just a) = Green "Just" <> Inherit " " <> (Bright $ Yellow a)
    toEscapable (Nothing) = Inverse $ Red "Nothing"

alterString :: String -> Escapable
alterString [] = Default ""
alterString a  = mconcat $ zipWith (\c v -> c v) (cycle [Inverse . Red, Red, Inverse . Blue, Red]) a

rainbowString :: String -> Escapable
rainbowString [] = Inherit ""
rainbowString a = mconcat $ zipWith (\ c v -> c v) (cycle [Red, Blue, Yellow, Cyan, Magenta]) a

takeFromZip :: Escapable
takeFromZip = mconcat $ take 10 $ zipWith (<>) (cycle [alterString "Zip"]) (cycle [rainbowString "Rainbow"])

posNeg :: (Integral a, ToEscapable a) => a -> Escapable
posNeg a | a < 0 = Red a
         | a == 0 = Inherit a
         | otherwise = Green a

replaceNumbers :: String -> String
replaceNumbers searchIn = subRegex (mkRegex "([0-9]+)") searchIn (escToString $ Red "\\1")

numberAts :: Escapable
numberAts =  mconcat $ intersperse (Inherit "@") $ map Default [1..10]

main = do
    putStrLn "Numbers in Red"
    putStrLn "=============="
    putEscLn $ replaceNumbers "7 times 3 is 21"
    putStrLn ""
    putStrLn "Negative Numbers in Red, Positive Numbers in Green"
    putStrLn "=================================================="
    putEscLn $ mconcat $ intersperse (Inherit " ") $ map posNeg [-10..10]
    putStrLn ""
    putStrLn "@ Inherits Parent Color"
    putStrLn "======================="
    putEscLn $ Yellow $ numberAts
    putEscLn $ Cyan $ numberAts
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
