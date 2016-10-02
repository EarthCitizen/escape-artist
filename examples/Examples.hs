{-# LANGUAGE FlexibleInstances #-}

import Data.Monoid (mempty, (<>))
import EscapeArtist
import Text.Regex

rainbowString :: String -> Escapable
rainbowString s = fn s (cycle [Red, White, Green, Blue, Yellow, Cyan])
    where fn [] _ = mempty
          fn _ [] = mempty
          fn (s:ss) ca@(c:cs)
              | s `elem` " \t\n\r" = Inherit s <> fn ss ca
              | otherwise = c s <> fn ss cs

spacesInherit = Red '@' <> Inherit ' ' <> Yellow '@' <> Inherit ' ' <> Green '@'

underlineOff = Underline $ Cyan "I am underlined" <> UnderlineOff " but I am not " <> Magenta "and I am over here"

op1 = Underline $ Bright ^$ Green "GREEN" <> Normal " " <> Yellow "YELLOW"
op2 = Underline $ (Bright $ Green "GREEN") <> Normal " " <> Yellow "YELLOW"

replaceNumbers :: String -> String
replaceNumbers searchIn = subRegex (mkRegex "([0-9]+)") searchIn (escToString $ Red "\\1")

type FileName = String
type LineNumber = Integer
type ColumnNumber = Integer
data ErrorType = SyntaxError FileName LineNumber ColumnNumber deriving (Show)

instance ToEscapable ErrorType where
    toEscapable (SyntaxError fn ln cn) = Normal "Syntax error in file "
                                       <> Yellow ^$ Underline fn
                                       <> Normal " at "
                                       <> Red (show ln ++ ":" ++ show cn)

instance ToEscapable (Either ErrorType String) where
    toEscapable (Left e) = toEscapable e
    toEscapable (Right m) = Green m

gotSyntaxError :: Either ErrorType String
gotSyntaxError = Left $ SyntaxError "some/File.hs" 1 23

gotMessage :: Either ErrorType String
gotMessage = Right "Status OK"

main = do
    putStrLn ""
    putEscLn $ rainbowString "Hello World!"
    putStrLn ""
    putEscLn spacesInherit
    putStrLn ""
    putEscLn $ Underline spacesInherit
    putStrLn ""
    putEscLn $ Inverse spacesInherit
    putStrLn ""
    putEscLn $ BgBlue spacesInherit
    putStrLn ""
    putEscLn $ underlineOff
    putStrLn ""
    putEscLn op1
    putStrLn ""
    putEscLn op2
    putStrLn ""
    putStrLn $ replaceNumbers "Line 7 of 23"
    putStrLn ""
    putEscLn $ gotSyntaxError
    putStrLn ""
    putEscLn $ gotMessage
