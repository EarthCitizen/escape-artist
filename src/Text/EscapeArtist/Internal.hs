module Text.EscapeArtist.Internal (Escapable(..), ToEscapable(..), putEscLn, putEsc, escToString) where

import Data.Monoid hiding (Sum)
import qualified Data.Text as T
import Data.Word
import Text.EscapeArtist.Constants

data Escapable = forall a. (ToEscapable a) => Black a
               | forall a. (ToEscapable a) => Red a
               | forall a. (ToEscapable a) => Green a
               | forall a. (ToEscapable a) => Yellow a
               | forall a. (ToEscapable a) => Blue a
               | forall a. (ToEscapable a) => Magenta a
               | forall a. (ToEscapable a) => Cyan a
               | forall a. (ToEscapable a) => White a

               | forall a. (ToEscapable a) => BgBlack a
               | forall a. (ToEscapable a) => BgRed a
               | forall a. (ToEscapable a) => BgGreen a
               | forall a. (ToEscapable a) => BgYellow a
               | forall a. (ToEscapable a) => BgBlue a
               | forall a. (ToEscapable a) => BgMagenta a
               | forall a. (ToEscapable a) => BgCyan a
               | forall a. (ToEscapable a) => BgWhite a

               | forall a. (ToEscapable a) => Default a
               | forall a. (ToEscapable a) => BgDefault a

               | forall a. (ToEscapable a) => Bright a
               | forall a. (ToEscapable a) => Underline a
               | forall a. (ToEscapable a) => Inverse a
               | forall a. (ToEscapable a) => Strike a
               | Sum [Escapable]
               | Text String

class ToEscapable a where
    toEscapable :: a -> Escapable

instance ToEscapable String where
    toEscapable a = Text a

instance ToEscapable Char where
    toEscapable a = Text [a]

instance ToEscapable T.Text where
    toEscapable a = Text $ T.unpack a

instance ToEscapable Int where
    toEscapable a = Text $ show a

instance ToEscapable Word8 where
    toEscapable a = Text $ show a

instance ToEscapable Word16 where
    toEscapable a = Text $ show a

instance ToEscapable Word32 where
    toEscapable a = Text $ show a

instance ToEscapable Word64 where
    toEscapable a = Text $ show a

instance ToEscapable Integer where
    toEscapable a = Text $ show a

instance ToEscapable Float where
    toEscapable a = Text $ show a

instance ToEscapable Double where
    toEscapable a = Text $ show a

instance ToEscapable Escapable where
    toEscapable = id

escToString :: Escapable -> String
escToString (Text a) = a
escToString esc      = escToStringEnclosed "" "" esc

escToStringEnclosed :: String -> String -> Escapable -> String
escToStringEnclosed prefix suffix (Black     a) = escToStringEnclosed (prefix ++ black  ) (defaultColor ++ suffix) (toEscapable a)
escToStringEnclosed prefix suffix (Red       a) = escToStringEnclosed (prefix ++ red    ) (defaultColor ++ suffix) (toEscapable a)
escToStringEnclosed prefix suffix (Green     a) = escToStringEnclosed (prefix ++ green  ) (defaultColor ++ suffix) (toEscapable a)
escToStringEnclosed prefix suffix (Yellow    a) = escToStringEnclosed (prefix ++ yellow ) (defaultColor ++ suffix) (toEscapable a)
escToStringEnclosed prefix suffix (Blue      a) = escToStringEnclosed (prefix ++ blue   ) (defaultColor ++ suffix) (toEscapable a)
escToStringEnclosed prefix suffix (Magenta   a) = escToStringEnclosed (prefix ++ magenta) (defaultColor ++ suffix) (toEscapable a)
escToStringEnclosed prefix suffix (Cyan      a) = escToStringEnclosed (prefix ++ cyan   ) (defaultColor ++ suffix) (toEscapable a)
escToStringEnclosed prefix suffix (White     a) = escToStringEnclosed (prefix ++ white  ) (defaultColor ++ suffix) (toEscapable a)

escToStringEnclosed prefix suffix (BgBlack   a) = escToStringEnclosed (prefix ++ bgblack  ) (defaultBgColor ++ suffix) (toEscapable a)
escToStringEnclosed prefix suffix (BgRed     a) = escToStringEnclosed (prefix ++ bgred    ) (defaultBgColor ++ suffix) (toEscapable a)
escToStringEnclosed prefix suffix (BgGreen   a) = escToStringEnclosed (prefix ++ bggreen  ) (defaultBgColor ++ suffix) (toEscapable a)
escToStringEnclosed prefix suffix (BgYellow  a) = escToStringEnclosed (prefix ++ bgyellow ) (defaultBgColor ++ suffix) (toEscapable a)
escToStringEnclosed prefix suffix (BgBlue    a) = escToStringEnclosed (prefix ++ bgblue   ) (defaultBgColor ++ suffix) (toEscapable a)
escToStringEnclosed prefix suffix (BgMagenta a) = escToStringEnclosed (prefix ++ bgmagenta) (defaultBgColor ++ suffix) (toEscapable a)
escToStringEnclosed prefix suffix (BgCyan    a) = escToStringEnclosed (prefix ++ bgcyan   ) (defaultBgColor ++ suffix) (toEscapable a)
escToStringEnclosed prefix suffix (BgWhite   a) = escToStringEnclosed (prefix ++ bgwhite  ) (defaultBgColor ++ suffix) (toEscapable a)

escToStringEnclosed prefix suffix (Default   a) = escToStringEnclosed (prefix ++ defaultColor  ) (suffix) (toEscapable a)
escToStringEnclosed prefix suffix (BgDefault a) = escToStringEnclosed (prefix ++ defaultBgColor) (suffix) (toEscapable a)

escToStringEnclosed prefix suffix (Bright    a) = escToStringEnclosed (prefix ++ brightOn   ) (brightOff    ++ suffix) (toEscapable  a)
escToStringEnclosed prefix suffix (Underline a) = escToStringEnclosed (prefix ++ underlineOn) (underlineOff ++ suffix) (toEscapable  a)
escToStringEnclosed prefix suffix (Inverse   a) = escToStringEnclosed (prefix ++ inverseOn  ) (inverseOff   ++ suffix) (toEscapable  a)
escToStringEnclosed prefix suffix (Strike    a) = escToStringEnclosed (prefix ++ strikeOn   ) (strikeOff    ++ suffix) (toEscapable  a)
escToStringEnclosed prefix suffix (Sum  a) = concat $ map (escToStringEnclosed prefix suffix) a
escToStringEnclosed prefix suffix (Text a) = concat [prefix, a, suffix]

instance Monoid Escapable where
    mempty = Sum []
    mappend (Sum []) b        = b
    mappend a        (Sum []) = a
    mappend (Sum as) (Sum bs) = Sum $ mconcat [as,  bs ]
    mappend (Sum as) b        = Sum $ mconcat [as,  [b]]
    mappend a        (Sum bs) = Sum $ mconcat [[a], bs ]
    mappend a        b        = Sum [a, b]

putEscLn :: (ToEscapable a) => a -> IO ()
putEscLn = putStrLn . escToString . toEscapable

putEsc :: (ToEscapable a) => a -> IO ()
putEsc = putStr . escToString . toEscapable
