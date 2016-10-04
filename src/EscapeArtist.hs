{-|
Module      : EscapeArtist
Description : Haskell ASCII Escape Codes Made Easy
Copyright   : (c) Ryan Daniels 2016
License     : BSD3
Maintainer  : rd.github@gmail.com
Stability   : stable
Portability : Terminal supporting ASCII escape codes

A Haskell library for ASCII escape codes made easy. Decorate your terminal text expressively while staying in your normal Haskell coding style.

@
import Data.Monoid ((<>))
import EscapeArtist

underlines = Underline $ FgCyan "I am underlined" <> UnderlineOff " but I am not " <> FgMagenta "and I am over here"

putEscLn underlines
@

<<images/underline_off_sm.png>>

See extended documentation with many examples here:

<https://github.com/EarthCitizen/escape-artist#readme>
-}

module EscapeArtist (Escapable(..), ToEscapable(..), putEscLn, putEsc, escToString, (^$)) where

import EscapeArtist.Internal hiding (Atom, Sum)
