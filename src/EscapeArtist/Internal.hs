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

-- | The same as '$', but with higher precedence. One level of precedence higher than 'Data.Monoid.<>'. This allows
-- avoiding parentheses when using '$' and 'Data.Monoid.<>' in the same expression. For example:
--
-- @
-- Underline $ (Bright $ FgGreen \"GREEN\") <> Default \" \" <> FgYellow \"YELLOW\"
-- @
--
-- can be written as:
--
-- @
-- Underline $ Bright ^$ FgGreen \"GREEN\" <> Default \" \" <> FgYellow \"YELLOW\"
-- @
--
-- In this example, 'Bright' is applied only to the 'String' \"GREEN\", that is concatenated
-- with a space and the yellow text \"YELLOW\", then 'Underline' is applied to the entire
-- expression.
--
(^$) :: (a -> b) -> a -> b
(^$) = ($)

-- | The constructors used to apply attributes to values
-- for terminal output
data Escapable = forall a. (ToEscapable a) => FgBlack   a -- ^ Foreground color black
               | forall a. (ToEscapable a) => FgRed     a -- ^ Foreground color red
               | forall a. (ToEscapable a) => FgGreen   a -- ^ Foreground color green
               | forall a. (ToEscapable a) => FgYellow  a -- ^ Foreground color yellow
               | forall a. (ToEscapable a) => FgBlue    a -- ^ Foreground color blue
               | forall a. (ToEscapable a) => FgMagenta a -- ^ Foreground color magenta
               | forall a. (ToEscapable a) => FgCyan    a -- ^ Foreground color cyan
               | forall a. (ToEscapable a) => FgWhite   a -- ^ Foreground color white

               | forall a. (ToEscapable a) => BgBlack   a -- ^ Background color black
               | forall a. (ToEscapable a) => BgRed     a -- ^ Background color red
               | forall a. (ToEscapable a) => BgGreen   a -- ^ Background color green
               | forall a. (ToEscapable a) => BgYellow  a -- ^ Background color yellow
               | forall a. (ToEscapable a) => BgBlue    a -- ^ Background color blue
               | forall a. (ToEscapable a) => BgMagenta a -- ^ Background color magenta
               | forall a. (ToEscapable a) => BgCyan    a -- ^ Background color cyan
               | forall a. (ToEscapable a) => BgWhite   a -- ^ Background color white

               | forall a. (ToEscapable a) => FgDefault a -- ^ Applies default terminal foreground color
               | forall a. (ToEscapable a) => BgDefault a -- ^ Applies default terminal background color
               | forall a. (ToEscapable a) => Inherit   a -- ^ Inherit attributes from the parent, but apply none directly
               | forall a. (ToEscapable a) => Default   a -- ^ Applied value will have defaults of terminal

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
    show (FgBlack   a) = "FgBlack ("   ++ show a ++ ")"
    show (FgRed     a) = "FgRed ("     ++ show a ++ ")"
    show (FgGreen   a) = "FgGreen ("   ++ show a ++ ")"
    show (FgYellow  a) = "FgYellow ("  ++ show a ++ ")"
    show (FgBlue    a) = "FgBlue ("    ++ show a ++ ")"
    show (FgMagenta a) = "FgMagenta (" ++ show a ++ ")"
    show (FgCyan    a) = "FgCyan ("    ++ show a ++ ")"
    show (FgWhite   a) = "FgWhite ("   ++ show a ++ ")"

    show (BgBlack   a) = "BgBlack ("   ++ show a ++ ")"
    show (BgRed     a) = "BgRed ("     ++ show a ++ ")"
    show (BgGreen   a) = "BgGreen ("   ++ show a ++ ")"
    show (BgYellow  a) = "BgYellow ("  ++ show a ++ ")"
    show (BgBlue    a) = "BgBlue ("    ++ show a ++ ")"
    show (BgMagenta a) = "BgMagenta (" ++ show a ++ ")"
    show (BgCyan    a) = "BgCyan ("    ++ show a ++ ")"
    show (BgWhite   a) = "BgWhite ("   ++ show a ++ ")"

    show (FgDefault a) = "FgDefault (" ++ show a ++ ")"
    show (BgDefault a) = "BgDefault (" ++ show a ++ ")"
    show (Inherit   a) = "Inherit ("   ++ show a ++ ")"
    show (Default   a) = "Default ("   ++ show a ++ ")"

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
    (FgBlack     a) == (FgBlack   b) = toCompStr a == toCompStr b
    (FgRed       a) == (FgRed     b) = toCompStr a == toCompStr b
    (FgGreen     a) == (FgGreen   b) = toCompStr a == toCompStr b
    (FgYellow    a) == (FgYellow  b) = toCompStr a == toCompStr b
    (FgBlue      a) == (FgBlue    b) = toCompStr a == toCompStr b
    (FgMagenta   a) == (FgMagenta b) = toCompStr a == toCompStr b
    (FgCyan      a) == (FgCyan    b) = toCompStr a == toCompStr b
    (FgWhite     a) == (FgWhite   b) = toCompStr a == toCompStr b

    (BgBlack   a) == (BgBlack   b) = toCompStr a == toCompStr b
    (BgRed     a) == (BgRed     b) = toCompStr a == toCompStr b
    (BgGreen   a) == (BgGreen   b) = toCompStr a == toCompStr b
    (BgYellow  a) == (BgYellow  b) = toCompStr a == toCompStr b
    (BgBlue    a) == (BgBlue    b) = toCompStr a == toCompStr b
    (BgMagenta a) == (BgMagenta b) = toCompStr a == toCompStr b
    (BgCyan    a) == (BgCyan    b) = toCompStr a == toCompStr b
    (BgWhite   a) == (BgWhite   b) = toCompStr a == toCompStr b

    (FgDefault a) == (FgDefault b) = toCompStr a == toCompStr b
    (BgDefault a) == (BgDefault b) = toCompStr a == toCompStr b
    (Inherit   a) == (Inherit   b) = toCompStr a == toCompStr b
    (Default   a) == (Default   b) = toCompStr a == toCompStr b

    (Blink        a) == (Blink        b) = toCompStr a == toCompStr b
    (BlinkOff     a) == (BlinkOff     b) = toCompStr a == toCompStr b
    (Bright       a) == (Bright       b) = toCompStr a == toCompStr b
    (BrightOff    a) == (BrightOff    b) = toCompStr a == toCompStr b
    (Underline    a) == (Underline    b) = toCompStr a == toCompStr b
    (UnderlineOff a) == (UnderlineOff b) = toCompStr a == toCompStr b
    (Inverse      a) == (Inverse      b) = toCompStr a == toCompStr b
    (InverseOff   a) == (InverseOff   b) = toCompStr a == toCompStr b

    (Sum  a) == (Sum  b) = toCompStr a == toCompStr b
    (Atom a) == (Atom b) = toCompStr a == toCompStr b
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

-- | Convert any instance of 'ToEscapable' to a 'String'
escToString :: (ToEscapable a) => a -> String
escToString esc = escToStrEncl "" "" $ toEscapable esc

recur :: String -> String -> Escapable -> String
recur = escToStrEncl

dc :: String
dc = defaultFgColor

dbc :: String
dbc = defaultBgColor

def :: String
def = defaultAll

te :: (ToEscapable a) => a -> Escapable
te = toEscapable

escToStrEncl :: String -> String -> Escapable -> String
escToStrEncl pref suff (FgBlack   a) = recur (pref ++ fgBlack  ) (dc ++ suff) (te a)
escToStrEncl pref suff (FgRed     a) = recur (pref ++ fgRed    ) (dc ++ suff) (te a)
escToStrEncl pref suff (FgGreen   a) = recur (pref ++ fgGreen  ) (dc ++ suff) (te a)
escToStrEncl pref suff (FgYellow  a) = recur (pref ++ fgYellow ) (dc ++ suff) (te a)
escToStrEncl pref suff (FgBlue    a) = recur (pref ++ fgBlue   ) (dc ++ suff) (te a)
escToStrEncl pref suff (FgMagenta a) = recur (pref ++ fgMagenta) (dc ++ suff) (te a)
escToStrEncl pref suff (FgCyan    a) = recur (pref ++ fgCyan   ) (dc ++ suff) (te a)
escToStrEncl pref suff (FgWhite   a) = recur (pref ++ fgWhite  ) (dc ++ suff) (te a)

escToStrEncl pref suff (BgBlack   a) = recur (pref ++ bgBlack  ) (dbc ++ suff) (te a)
escToStrEncl pref suff (BgRed     a) = recur (pref ++ bgRed    ) (dbc ++ suff) (te a)
escToStrEncl pref suff (BgGreen   a) = recur (pref ++ bgGreen  ) (dbc ++ suff) (te a)
escToStrEncl pref suff (BgYellow  a) = recur (pref ++ bgYellow ) (dbc ++ suff) (te a)
escToStrEncl pref suff (BgBlue    a) = recur (pref ++ bgBlue   ) (dbc ++ suff) (te a)
escToStrEncl pref suff (BgMagenta a) = recur (pref ++ bgMagenta) (dbc ++ suff) (te a)
escToStrEncl pref suff (BgCyan    a) = recur (pref ++ bgCyan   ) (dbc ++ suff) (te a)
escToStrEncl pref suff (BgWhite   a) = recur (pref ++ bgWhite  ) (dbc ++ suff) (te a)

escToStrEncl pref suff (FgDefault a) = recur (pref ++ dc ) suff (te a)
escToStrEncl pref suff (BgDefault a) = recur (pref ++ dbc) suff (te a)
escToStrEncl pref suff (Inherit   a) = recur pref          suff (te a)
escToStrEncl pref suff (Default   a) = recur (pref ++ def) suff (te a)

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

-- | Convert any instance of 'ToEscapable' to a 'String' and output it to the terminal
putEscLn :: (ToEscapable a) => a -> IO ()
putEscLn = putStrLn . escToString

-- | Convert any instance of 'ToEscapable' to a 'String' and output it to the terminal follow by a newline
putEsc :: (ToEscapable a) => a -> IO ()
putEsc = putStr . escToString
