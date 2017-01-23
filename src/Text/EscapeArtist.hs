{-|
Module      : Text.EscapeArtist
Description : ANSI Escape Sequence Text Decoration Made Easy
Copyright   : (c) Ryan Daniels 2016
License     : BSD3
Maintainer  : rd.github@gmail.com
Stability   : stable
Portability : Terminal supporting ANSI escape sequences

A library for text decoration with ANSI escape sequences made easy. Decorate your terminal text expressively.
Any complex data type, existing or custom, can be simply colorized by implementing the class 'ToEscapable', then
output to terminal or converted to 'String' using the provided functions.

=== Simple Example

@
import Data.Monoid ((<>))
import Text.EscapeArtist

underlines = Underline $ FgCyan \"I am underlined\" <> UnderlineOff \" but I am not \" <> FgMagenta \"and I am over here\"

putEscLn underlines
@

<<https://raw.githubusercontent.com/EarthCitizen/escape-artist/master/images/underline_off.png>>

=== Implementing 'ToEscapable'

@
import Data.Monoid ((<>))
import Text.EscapeArtist

data ABC = A | B deriving (Show, Eq)

instance ToEscapable ABC where
   toEscapable (A) = FgRed $ show A
   toEscapable (B) = FgGreen $ show B

instance (ToEscapable a) => ToEscapable (Maybe a) where
    toEscapable (Just a) = FgGreen \"Just\" <> Inherit \" \" <> FgYellow a
    toEscapable a = FgRed $ show a
@

=== Notes

See the documentation on 'ToEscapable' below for a more advanced example.

For GHC < 7.10 you will also need to explicitly derive 'Data.Typeable.Typeable' for custom data types
implementing 'ToEscapable'. See the section __/Explicitly Derived Typeable/__ in the
<https://github.com/EarthCitizen/escape-artist#explicitly-derived-typeable documentation>.

Comprehensive documentation with many examples here:

<https://github.com/EarthCitizen/escape-artist#readme>
-}

module Text.EscapeArtist (Escapable(..), ToEscapable(..), putEscLn, putEsc, escToString, (^$)) where

import Text.EscapeArtist.Internal hiding (Atom, Sum)
