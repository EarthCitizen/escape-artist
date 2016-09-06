{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Data.List (intercalate)
import Data.Monoid
import qualified Data.Text as T
import Data.Word

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
              | Multi [Modifier]

class ToString a where
    toString :: a -> String

instance ToString String where
    toString a = a

instance ToString Char where
    toString a = [a]

instance ToString T.Text where
    toString a = T.unpack a

instance ToString Int where
    toString a = show a

instance ToString Word8 where
    toString a = show a

instance ToString Word16 where
    toString a = show a

instance ToString Word32 where
    toString a = show a

instance ToString Word64 where
    toString a = show a

instance ToString Integer where
    toString a = show a

instance ToString Float where
    toString a = show a

instance ToString Double where
    toString a = show a

instance ToString Modifier where
    toString (Black a)     = concat [black, toString a, defaultColor]
    toString (Red a)       = concat [red, toString a, defaultColor]
    toString (Green a)     = concat [green, toString a, defaultColor]
    toString (Yellow a)    = concat [yellow, toString a, defaultColor]
    toString (Blue a)      = concat [blue, toString a, defaultColor]
    toString (Magenta a)   = concat [magenta, toString a, defaultColor]
    toString (Cyan a)      = concat [cyan, toString a, defaultColor]
    toString (White a)     = concat [white, toString a, defaultColor]

    toString (BgBlack a)   = concat [bgblack, toString a, defaultBgColor]
    toString (BgRed a)     = concat [bgred, toString a, defaultBgColor]
    toString (BgGreen a)   = concat [bggreen, toString a, defaultBgColor]
    toString (BgYellow a)  = concat [bgyellow, toString a, defaultBgColor]
    toString (BgBlue a)    = concat [bgblue, toString a, defaultBgColor]
    toString (BgMagenta a) = concat [bgmagenta, toString a, defaultBgColor]
    toString (BgCyan a)    = concat [bgcyan, toString a, defaultBgColor]
    toString (BgWhite a)   = concat [bgwhite, toString a, defaultBgColor]

    toString (Default a)   = concat [defaultColor, toString a]
    toString (BgDefault a) = concat [defaultBgColor, toString a]

    toString (Bright a)    = concat [brightOn, toString a, brightOff]
    toString (Underline a) = concat [underlineOn, toString a, underlineOff]
    toString (Inverse a)   = concat [inverseOn, toString a, inverseOff]
    toString (Strike a)    = concat [strikeOn, toString a, strikeOff]
    toString (Multi as)    = concat $ map toString as

instance Monoid Modifier where
    mempty = Multi []
    mappend (Multi as) (Multi bs) = Multi $ as ++ bs
    mappend (Multi as) b = Multi $ as ++ [b]
    mappend a (Multi bs) = Multi $ [a] ++ bs
    mappend a b = Multi [a, b]

putColorLn :: Modifier -> IO ()
putColorLn = putStrLn . toString

putColor :: Modifier -> IO ()
putColor = putStr . toString

x = [BgWhite $ Black "black", Red "red", Green "green", Yellow "yellow", Blue "Blue", Magenta "magenta", Cyan "cyan", White "white"]
y = map Underline x
z = Underline (Green 5)

m1 = Inverse $ Multi [Underline $ Green "Hello Green", Bright $ Blue "Hello Blue"]
m2 = Red "Hello RED"

m3 = Inverse $ BgRed $ White $ Bright "Hello World"

-- main = putColorLn $ Multi [BgYellow "Hello", BgYellow $ Multi [m1, m2]]
main = do
    mapM_ putColorLn x
    putColorLn m1
    putColorLn m2
    putColorLn $ Cyan $ T.pack "Cyan Text"
    putColorLn $ Magenta $ (10::Word64)
    putColorLn $ Multi [Red "RED", Yellow "YELLOW"] <> Multi [Default " ", White 1, Default " ", Underline "Hello"]
    putColorLn $ Red "RED" <> Default " " <> Yellow "YELLOW" <> Default " " <> White 1 <> Default " " <> Underline "Hello"
