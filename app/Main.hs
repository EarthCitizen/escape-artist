{-# LANGUAGE ExtendedDefaultRules, NoMonomorphismRestriction #-}

import Data.Monoid ((<>))
import Text.EscapeArtist
import Data.List (intersperse)
import Text.Regex


instance (ToEscapable a) => ToEscapable (Maybe a) where
    toEscapable (Just a) = Green "Just" <> Inherited " " <> (Bright $ Yellow a)
    toEscapable (Nothing) = Inverse $ Red "Nothing"

alterString :: String -> Escapable
alterString [] = Default ""
alterString a  = mconcat $ zipWith (\c v -> c v) (cycle [Inverse . Red, Red, Inverse . Blue, Red]) a

rainbowString :: String -> Escapable
rainbowString [] = Inherited ""
rainbowString a = mconcat $ zipWith (\ c v -> c v) (cycle [Red, Blue, Yellow, Cyan, Magenta]) a

posNeg :: (Integral a, ToEscapable a) => a -> Escapable
posNeg a | a < 0 = Red a
         | otherwise = Green a

replaceNumbers :: String -> String
replaceNumbers searchIn = subRegex (mkRegex "([0-9]+)") searchIn (escToString $ Red "\\1")

numberAts :: Escapable
numberAts =  mconcat $ intersperse (Inherited "@") $ map Default [1..10]

numberAtsWithInheritedYellow = Yellow $ numberAts
numberAtsWithInheritedCyan = Cyan $ numberAts

main = do
    putEscLn $ replaceNumbers "7 times 3 is 21"
    putEscLn $ mconcat $ intersperse (Inherited " ") $ map posNeg [-10..10]
    putEscLn $ numberAtsWithInheritedYellow
    putEscLn $ numberAtsWithInheritedCyan
    putEscLn 10
    putEscLn $ Just 10
    putEscLn $ Underline $ Just 100
    putEscLn $ Inverse $ Underline $ Just 1000
    putEscLn (Nothing :: Maybe String)
    putEscLn $ rainbowString "Hello World!"
    putEscLn $ alterString "/\\/\\/\\/\\/\\/\\/\\/\\/\\/\\"
