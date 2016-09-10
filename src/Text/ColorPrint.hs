module Text.ColorPrint (Modifier(..), putColorLn, putColor) where

import Data.List (intercalate)
import Data.Monoid hiding (Sum)
import qualified Data.Text as T
import Data.Word
import Text.ColorPrint.Internal

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
              | Sum [Modifier]

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
    toString (Black     a) = concat [black  , toString a, defaultColor]
    toString (Red       a) = concat [red    , toString a, defaultColor]
    toString (Green     a) = concat [green  , toString a, defaultColor]
    toString (Yellow    a) = concat [yellow , toString a, defaultColor]
    toString (Blue      a) = concat [blue   , toString a, defaultColor]
    toString (Magenta   a) = concat [magenta, toString a, defaultColor]
    toString (Cyan      a) = concat [cyan   , toString a, defaultColor]
    toString (White     a) = concat [white  , toString a, defaultColor]

    toString (BgBlack   a) = concat [bgblack  , toString a, defaultBgColor]
    toString (BgRed     a) = concat [bgred    , toString a, defaultBgColor]
    toString (BgGreen   a) = concat [bggreen  , toString a, defaultBgColor]
    toString (BgYellow  a) = concat [bgyellow , toString a, defaultBgColor]
    toString (BgBlue    a) = concat [bgblue   , toString a, defaultBgColor]
    toString (BgMagenta a) = concat [bgmagenta, toString a, defaultBgColor]
    toString (BgCyan    a) = concat [bgcyan   , toString a, defaultBgColor]
    toString (BgWhite   a) = concat [bgwhite  , toString a, defaultBgColor]

    toString (Default   a) = concat [defaultColor  , toString a]
    toString (BgDefault a) = concat [defaultBgColor, toString a]

    toString (Bright    a) = concat [brightOn   , toString a, brightOff   ]
    toString (Underline a) = concat [underlineOn, toString a, underlineOff]
    toString (Inverse   a) = concat [inverseOn  , toString a, inverseOff  ]
    toString (Strike    a) = concat [strikeOn   , toString a, strikeOff   ]
    toString (Sum      as) = concat $ map toString as

instance Monoid Modifier where
    mempty = Sum []
    mappend (Sum as) (Sum bs) = Sum $ as ++ bs
    mappend (Sum as) b        = Sum $ as ++ [b]
    mappend a        (Sum bs) = Sum $ [a] ++ bs
    mappend a        b        = Sum [a, b]

putColorLn :: Modifier -> IO ()
putColorLn = putStrLn . toString

putColor :: Modifier -> IO ()
putColor = putStr . toString
