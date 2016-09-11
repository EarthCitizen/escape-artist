module Text.EscapeArtist.Internal (Escapable(..), ToEscapable(..), putEscLn, putEsc, escToString) where

import Data.Monoid hiding (Sum)
import qualified Data.Text as T
import Data.Typeable (Typeable, cast)
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
               | forall a. (ToEscapable a) => Context a

               | forall a. (ToEscapable a) => Bright a
               | forall a. (ToEscapable a) => Underline a
               | forall a. (ToEscapable a) => Inverse a
               | forall a. (ToEscapable a) => Strike a
               | Sum [Escapable]
               | Text String

instance Show Escapable where
    show (Black     a) = "Black ("     ++ show a ++ ")"
    show (Red       a) = "Red ("       ++ show a ++ ")"
    show (Green     a) = "Green ("     ++ show a ++ ")"
    show (Yellow    a) = "Yellow ("    ++ show a ++ ")"
    show (Blue      a) = "Blue ("      ++ show a ++ ")"
    show (Magenta   a) = "Magenta ("   ++ show a ++ ")"
    show (Cyan      a) = "Cyan ("      ++ show a ++ ")"
    show (White     a) = "White ("     ++ show a ++ ")"

    show (BgBlack   a) = "BgBlack ("   ++ show a ++ ")"
    show (BgRed     a) = "BgRed ("     ++ show a ++ ")"
    show (BgGreen   a) = "BgGreen ("   ++ show a ++ ")"
    show (BgYellow  a) = "BgYellow ("  ++ show a ++ ")"
    show (BgBlue    a) = "BgBlue ("    ++ show a ++ ")"
    show (BgMagenta a) = "BgMagenta (" ++ show a ++ ")"
    show (BgCyan    a) = "BgCyan ("    ++ show a ++ ")"
    show (BgWhite   a) = "BgWhite ("   ++ show a ++ ")"

    show (Default   a) = "Default ("   ++ show a ++ ")"
    show (BgDefault a) = "BgDefault (" ++ show a ++ ")"
    show (Context   a) = "Context ("   ++ show a ++ ")"

    show (Bright    a) = "Bright ("    ++ show a ++ ")"
    show (Underline a) = "Underline (" ++ show a ++ ")"
    show (Inverse   a) = "Inverse ("   ++ show a ++ ")"
    show (Strike    a) = "Strike ("    ++ show a ++ ")"

    show (Sum       a) = "Sum "  ++ show a
    show (Text      a) = "Text " ++ show a

toCompStr :: (Show a, Typeable a) => a -> String
toCompStr a = case cast a :: Maybe String of
                (Just s) -> s
                _ -> show a

instance Eq Escapable where
    (Black     a) == (Black     b) = toCompStr a == toCompStr b
    (Red       a) == (Red       b) = toCompStr a == toCompStr b
    (Green     a) == (Green     b) = toCompStr a == toCompStr b
    (Yellow    a) == (Yellow    b) = toCompStr a == toCompStr b
    (Blue      a) == (Blue      b) = toCompStr a == toCompStr b
    (Magenta   a) == (Magenta   b) = toCompStr a == toCompStr b
    (Cyan      a) == (Cyan      b) = toCompStr a == toCompStr b
    (White     a) == (White     b) = toCompStr a == toCompStr b

    (BgBlack   a) == (BgBlack   b) = toCompStr a == toCompStr b
    (BgRed     a) == (BgRed     b) = toCompStr a == toCompStr b
    (BgGreen   a) == (BgGreen   b) = toCompStr a == toCompStr b
    (BgYellow  a) == (BgYellow  b) = toCompStr a == toCompStr b
    (BgBlue    a) == (BgBlue    b) = toCompStr a == toCompStr b
    (BgMagenta a) == (BgMagenta b) = toCompStr a == toCompStr b
    (BgCyan    a) == (BgCyan    b) = toCompStr a == toCompStr b
    (BgWhite   a) == (BgWhite   b) = toCompStr a == toCompStr b

    (Default   a) == (Default   b) = toCompStr a == toCompStr b
    (BgDefault a) == (BgDefault b) = toCompStr a == toCompStr b
    (Context   a) == (Context   b) = toCompStr a == toCompStr b

    (Bright    a) == (Bright    b) = toCompStr a == toCompStr b
    (Underline a) == (Underline b) = toCompStr a == toCompStr b
    (Inverse   a) == (Inverse   b) = toCompStr a == toCompStr b
    (Strike    a) == (Strike    b) = toCompStr a == toCompStr b

    (Sum       a) == (Sum       b) = toCompStr a == toCompStr b
    (Text      a) == (Text      b) = toCompStr a == toCompStr b
    _ == _ = False

class (Eq a, Show a, Typeable a) => ToEscapable a where
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
escToString esc      = escToStrEncl "" "" esc

recur = escToStrEncl
dc = defaultColor
dbc = defaultBgColor
te = toEscapable

escToStrEncl :: String -> String -> Escapable -> String
escToStrEncl pref suff (Black     a) = recur (pref ++ black  ) (dc ++ suff) (te a)
escToStrEncl pref suff (Red       a) = recur (pref ++ red    ) (dc ++ suff) (te a)
escToStrEncl pref suff (Green     a) = recur (pref ++ green  ) (dc ++ suff) (te a)
escToStrEncl pref suff (Yellow    a) = recur (pref ++ yellow ) (dc ++ suff) (te a)
escToStrEncl pref suff (Blue      a) = recur (pref ++ blue   ) (dc ++ suff) (te a)
escToStrEncl pref suff (Magenta   a) = recur (pref ++ magenta) (dc ++ suff) (te a)
escToStrEncl pref suff (Cyan      a) = recur (pref ++ cyan   ) (dc ++ suff) (te a)
escToStrEncl pref suff (White     a) = recur (pref ++ white  ) (dc ++ suff) (te a)

escToStrEncl pref suff (BgBlack   a) = recur (pref ++ bgblack  ) (dbc ++ suff) (te a)
escToStrEncl pref suff (BgRed     a) = recur (pref ++ bgred    ) (dbc ++ suff) (te a)
escToStrEncl pref suff (BgGreen   a) = recur (pref ++ bggreen  ) (dbc ++ suff) (te a)
escToStrEncl pref suff (BgYellow  a) = recur (pref ++ bgyellow ) (dbc ++ suff) (te a)
escToStrEncl pref suff (BgBlue    a) = recur (pref ++ bgblue   ) (dbc ++ suff) (te a)
escToStrEncl pref suff (BgMagenta a) = recur (pref ++ bgmagenta) (dbc ++ suff) (te a)
escToStrEncl pref suff (BgCyan    a) = recur (pref ++ bgcyan   ) (dbc ++ suff) (te a)
escToStrEncl pref suff (BgWhite   a) = recur (pref ++ bgwhite  ) (dbc ++ suff) (te a)

escToStrEncl pref suff (Default   a) = recur (pref ++ dc ) (suff) (te a)
escToStrEncl pref suff (BgDefault a) = recur (pref ++ dbc) (suff) (te a)
escToStrEncl pref suff (Context   a) = recur (pref)        (suff) (te a)

escToStrEncl pref suff (Bright    a) = recur (pref ++ brightOn   ) (brightOff    ++ suff) (te a)
escToStrEncl pref suff (Underline a) = recur (pref ++ underlineOn) (underlineOff ++ suff) (te a)
escToStrEncl pref suff (Inverse   a) = recur (pref ++ inverseOn  ) (inverseOff   ++ suff) (te a)
escToStrEncl pref suff (Strike    a) = recur (pref ++ strikeOn   ) (strikeOff    ++ suff) (te a)
escToStrEncl pref suff (Sum  a) = concat $ map (recur pref suff) a
escToStrEncl pref suff (Text a) = concat [pref, a, suff]

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
