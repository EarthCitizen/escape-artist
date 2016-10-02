{-# OPTIONS_HADDOCK hide #-}

module EscapeArtist.Internal (Escapable(..), ToEscapable(..), putEscLn, putEsc, escToString, (^$)) where

import Control.Applicative ((<|>))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Typeable (Typeable, cast)
import Data.Word
import EscapeArtist.Constants

infixr 7 ^$

(^$) :: (a -> b) -> a -> b
(^$) = ($)

-- | The constructors used to apply attributes to values
-- for terminal output
data Escapable = forall a. (ToEscapable a) => Black   a -- ^ Forground color black
               | forall a. (ToEscapable a) => Red     a -- ^ Forground color red
               | forall a. (ToEscapable a) => Green   a -- ^ Forground color green
               | forall a. (ToEscapable a) => Yellow  a -- ^ Forground color yellow
               | forall a. (ToEscapable a) => Blue    a -- ^ Forground color blue
               | forall a. (ToEscapable a) => Magenta a -- ^ Forground color magenta
               | forall a. (ToEscapable a) => Cyan    a -- ^ Forground color cyan
               | forall a. (ToEscapable a) => White   a -- ^ Forground color white

               | forall a. (ToEscapable a) => BgBlack   a -- ^ Background color black
               | forall a. (ToEscapable a) => BgRed     a -- ^ Background color red
               | forall a. (ToEscapable a) => BgGreen   a -- ^ Background color green
               | forall a. (ToEscapable a) => BgYellow  a -- ^ Background color yellow
               | forall a. (ToEscapable a) => BgBlue    a -- ^ Background color blue
               | forall a. (ToEscapable a) => BgMagenta a -- ^ Background color magenta
               | forall a. (ToEscapable a) => BgCyan    a -- ^ Background color cyan
               | forall a. (ToEscapable a) => BgWhite   a -- ^ Background color white

               | forall a. (ToEscapable a) => Default   a -- ^ Applies default terminal foreground color
               | forall a. (ToEscapable a) => BgDefault a -- ^ Applies default terminal background color
               | forall a. (ToEscapable a) => Inherit   a -- ^ Inherit attributes from the parent, but apply none directly
               | forall a. (ToEscapable a) => Normal    a -- ^ Applied value will not inherit any attribute from parent

               | forall a. (ToEscapable a) => Blink        a -- ^ Blinking text
               | forall a. (ToEscapable a) => BlinkOff     a -- ^ Will not inherit blink attribute from parent
               | forall a. (ToEscapable a) => Bright       a -- ^ Color mode to bright
               | forall a. (ToEscapable a) => BrightOff    a -- ^ Will not inherit bright attribute from parent
               | forall a. (ToEscapable a) => Underline    a -- ^ Underlined text
               | forall a. (ToEscapable a) => UnderlineOff a -- ^ Will not inherit underline attribute from parent
               | forall a. (ToEscapable a) => Inverse      a -- ^ Swap the background and foreground colors
               | forall a. (ToEscapable a) => InverseOff   a -- ^ Will not inherit inverse attribute from parent
               | Sum [Escapable]
               | Atom String

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
    show (Inherit   a) = "Inherit ("   ++ show a ++ ")"
    show (Normal    a) = "Normal ("    ++ show a ++ ")"

    show (Blink        a) = "Blink ("        ++ show a ++ ")"
    show (BlinkOff     a) = "BlinkOff ("     ++ show a ++ ")"
    show (Bright       a) = "Bright ("       ++ show a ++ ")"
    show (BrightOff    a) = "BrightOff ("    ++ show a ++ ")"
    show (Underline    a) = "Underline ("    ++ show a ++ ")"
    show (UnderlineOff a) = "UnderlineOff (" ++ show a ++ ")"
    show (Inverse      a) = "Inverse ("      ++ show a ++ ")"
    show (InverseOff   a) = "InverseOff ("   ++ show a ++ ")"

    show (Sum       a) = "Sum "  ++ show a
    show (Atom      a) = "Atom " ++ show a

-- TODO: Replace Atom contents with this type?
-- data Atom = BSAtom  BS.ByteString
--           | BSLAtom BSL.ByteString
--           | SAtom   String
--           | TAtom   T.Text
--           | TLAtom  TL.Text
--           deriving (Eq, Show)

tryCast :: forall a b. (Show b, Typeable a, Typeable b) => a -> (b -> String) -> Maybe String
tryCast a f = case cast a of
                (Just s) -> Just $ f s
                _ -> Nothing

tryString, tryChar, tryBS, tryBSL, tryT, tryTL :: Typeable a => a -> Maybe String

tryString a = tryCast a id
tryChar   a = tryCast a (\b -> [b :: Char]  )
tryBS     a = tryCast a (\b -> BSC.unpack  b)
tryBSL    a = tryCast a (\b -> BSLC.unpack b)
tryT      a = tryCast a (\b -> T.unpack    b)
tryTL     a = tryCast a (\b -> TL.unpack   b)

toCompStr :: forall a. (Show a, Typeable a) => a -> String
toCompStr a = case options of
                (Just s) -> s
                _ -> show a
    where options = tryString a
                  <|> tryChar a
                  <|> tryBS a
                  <|> tryBSL a
                  <|> tryT a
                  <|> tryTL a

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
    (Inherit   a) == (Inherit   b) = toCompStr a == toCompStr b
    (Normal    a) == (Normal    b) = toCompStr a == toCompStr b

    (Blink        a) == (Blink        b) = toCompStr a == toCompStr b
    (BlinkOff     a) == (BlinkOff     b) = toCompStr a == toCompStr b
    (Bright       a) == (Bright       b) = toCompStr a == toCompStr b
    (BrightOff    a) == (BrightOff    b) = toCompStr a == toCompStr b
    (Underline    a) == (Underline    b) = toCompStr a == toCompStr b
    (UnderlineOff a) == (UnderlineOff b) = toCompStr a == toCompStr b
    (Inverse      a) == (Inverse      b) = toCompStr a == toCompStr b
    (InverseOff   a) == (InverseOff   b) = toCompStr a == toCompStr b

    (Sum       a) == (Sum       b) = toCompStr a == toCompStr b
    (Atom      a) == (Atom      b) = toCompStr a == toCompStr b
    _ == _ = False

class (Show a, Typeable a) => ToEscapable a where
    -- | Convert the given type to an Escapable
    toEscapable :: a -> Escapable

instance ToEscapable String where
    toEscapable = Atom

instance ToEscapable Char where
    toEscapable a = Atom [a]

instance ToEscapable BS.ByteString where
    toEscapable a = Atom $ BSC.unpack a

instance ToEscapable BSL.ByteString where
    toEscapable a = Atom $ BSLC.unpack a

instance ToEscapable T.Text where
    toEscapable a = Atom $ T.unpack a

instance ToEscapable TL.Text where
    toEscapable a = Atom $ TL.unpack a

instance ToEscapable Int where
    toEscapable a = Atom $ show a

instance ToEscapable Integer where
    toEscapable a = Atom $ show a

instance ToEscapable Word where
    toEscapable a = Atom $ show a

instance ToEscapable Word8 where
    toEscapable a = Atom $ show a

instance ToEscapable Word16 where
    toEscapable a = Atom $ show a

instance ToEscapable Word32 where
    toEscapable a = Atom $ show a

instance ToEscapable Word64 where
    toEscapable a = Atom $ show a

instance ToEscapable Float where
    toEscapable a = Atom $ show a

instance ToEscapable Double where
    toEscapable a = Atom $ show a

instance ToEscapable Escapable where
    toEscapable = id

-- | Convert any instance of ToEscapable to a String
escToString :: (ToEscapable a) => a -> String
escToString esc = escToStrEncl "" "" $ toEscapable esc

recur :: String -> String -> Escapable -> String
recur = escToStrEncl

dc :: String
dc = defaultColor

dbc :: String
dbc = defaultBgColor

res :: String
res = reset

te :: (ToEscapable a) => a -> Escapable
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

escToStrEncl pref suff (Default   a) = recur (pref ++ dc ) suff (te a)
escToStrEncl pref suff (BgDefault a) = recur (pref ++ dbc) suff (te a)
escToStrEncl pref suff (Inherit   a) = recur pref          suff (te a)
escToStrEncl pref suff (Normal    a) = recur (pref ++ res) suff (te a)

escToStrEncl pref suff (Blink        a) = recur (pref ++ blinkOn     ) (blinkOff     ++ suff) (te a)
escToStrEncl pref suff (BlinkOff     a) = recur (pref ++ blinkOff    ) suff                   (te a)
escToStrEncl pref suff (Bright       a) = recur (pref ++ brightOn    ) (brightOff    ++ suff) (te a)
escToStrEncl pref suff (BrightOff    a) = recur (pref ++ brightOff   ) suff                   (te a)
escToStrEncl pref suff (Underline    a) = recur (pref ++ underlineOn ) (underlineOff ++ suff) (te a)
escToStrEncl pref suff (UnderlineOff a) = recur (pref ++ underlineOff) suff                   (te a)
escToStrEncl pref suff (Inverse      a) = recur (pref ++ inverseOn   ) (inverseOff   ++ suff) (te a)
escToStrEncl pref suff (InverseOff   a) = recur (pref ++ inverseOff  ) suff                   (te a)

escToStrEncl pref suff (Sum  a) = concatMap (recur pref suff) a
escToStrEncl pref suff (Atom a) = concat [pref, a, suff]

instance Monoid Escapable where
    mempty = Sum []
    mappend (Sum []) b        = b
    mappend a        (Sum []) = a
    mappend (Sum as) (Sum bs) = Sum $ mconcat [as,  bs ]
    mappend (Sum as) b        = Sum $ mconcat [as,  [b]]
    mappend a        (Sum bs) = Sum $ mconcat [[a], bs ]
    mappend a        b        = Sum [a, b]

-- | Convert any instance of ToEscapable to a String and output it to the terminal
putEscLn :: (ToEscapable a) => a -> IO ()
putEscLn = putStrLn . escToString

-- | Convert any instance of ToEscapable to a String and output it to the terminal follow by a newline
putEsc :: (ToEscapable a) => a -> IO ()
putEsc = putStr . escToString
