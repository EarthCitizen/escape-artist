{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Data.List (intercalate)

black   = "\x1B[30m"
red     = "\x1B[31m"
green   = "\x1B[32m"
yellow  = "\x1B[33m"
blue    = "\x1B[34m"
magenta = "\x1B[35m"
cyan    = "\x1B[36m"
white   = "\x1B[37m"

bgblack   = "\x1B[40m"
bgred     = "\x1B[41m"
bggreen   = "\x1B[42m"
bgyellow  = "\x1B[43m"
bgblue    = "\x1B[44m"
bgmagenta = "\x1B[45m"
bgcyan    = "\x1B[46m"
bgwhite   = "\x1B[47m"

reset = "\x1B[0m"

defaultColor = "\x1B[39m"

defaultBgColor = "\x1B[49m"

brightOn  = "\x1B[1m"
brightOff = "\x1B[22m"

underlineOn = "\x1B[4m"
underlineOff = "\x1B[24m"

inverseOn = "\x1B[7m"
inverseOff = "\x1B[27m"

strikeOn = "\x1B[9m"
strikeOff = "\x1B[29m"

data Modifier = forall a. (ToString a) => Black a
              | forall a. (ToString a) => Red a
              | forall a. (ToString a) => Green a
              | forall a. (ToString a) => Yellow a
              | forall a. (ToString a) => Blue a
              | forall a. (ToString a) => Magenta a
              | forall a. (ToString a) => Cyan a
              | forall a. (ToString a) => White a

              | forall a. (ToString a) => BgBlack a
              | forall a. (ToString a) => BgRed a
              | forall a. (ToString a) => BgGreen a
              | forall a. (ToString a) => BgYellow a
              | forall a. (ToString a) => BgBlue a
              | forall a. (ToString a) => BgMagenta a
              | forall a. (ToString a) => BgCyan a
              | forall a. (ToString a) => BgWhite a

              | forall a. (ToString a) => Default a
              | forall a. (ToString a) => BgDefault a

              | forall a. (ToString a) => Bright a
              | forall a. (ToString a) => Underline a
              | forall a. (ToString a) => Inverse a
              | forall a. (ToString a) => Strike a
              | forall a. (ToString a) => Multi [a]

class ToString a where
    toString :: a -> String

instance ToString String where
    toString a = a

instance ToString Char where
    toString a = [a]

instance ToString Int where
    toString a = show a

instance ToString Integer where
    toString a = show a

instance ToString Float where
    toString a = show a

instance ToString Double where
    toString a = show a

instance ToString Modifier where
    toString (Black a)     = black ++ toString a ++ defaultColor
    toString (Red a)       = red ++ toString a ++ defaultColor
    toString (Green a)     = green ++ toString a ++ defaultColor
    toString (Yellow a)    = yellow ++ toString a ++ defaultColor
    toString (Blue a)      = blue ++ toString a ++ defaultColor
    toString (Magenta a)   = magenta ++ toString a ++ defaultColor
    toString (Cyan a)      = cyan ++ toString a ++ defaultColor
    toString (White a)     = white ++ toString a ++ defaultColor

    toString (BgBlack a)   = bgblack ++ toString a ++ defaultBgColor
    toString (BgRed a)     = bgred ++ toString a ++ defaultBgColor
    toString (BgGreen a)   = bggreen ++ toString a ++ defaultBgColor
    toString (BgYellow a)  = bgyellow ++ toString a ++ defaultBgColor
    toString (BgBlue a)    = bgblue ++ toString a ++ defaultBgColor
    toString (BgMagenta a) = bgmagenta ++ toString a ++ defaultBgColor
    toString (BgCyan a)    = bgcyan ++ toString a ++ defaultBgColor
    toString (BgWhite a)   = bgwhite ++ toString a ++ defaultBgColor

    toString (Default a)   = defaultColor ++ toString a
    toString (BgDefault a) = defaultBgColor ++ toString a

    toString (Bright a)    = brightOn ++ toString a ++ brightOff
    toString (Underline a) = underlineOn ++ toString a ++ underlineOff
    toString (Inverse a)   = inverseOn ++ toString a ++ inverseOff
    toString (Strike a)    = strikeOn ++ toString a ++ strikeOff
    toString (Multi as)    = concat $ map toString as

x = [Black "black", Red "red", Green "green", Yellow "yellow", Blue "Blue", Magenta "magenta", Cyan "cyan", White "white"]
y = map Underline x
z = Underline (Green 5)

m1 = Default $ Inverse $ Multi [Underline $ Blue "Hello Green", Bright $ Blue "Hello Blue"]
m2 = Red "Hello RED"

main = putStrLn $ toString $ Multi [BgYellow "Hello", BgYellow $ Multi [m1, m2]]
