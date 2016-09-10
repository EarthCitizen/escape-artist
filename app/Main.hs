{-# LANGUAGE ExtendedDefaultRules, NoMonomorphismRestriction #-}

import Data.Monoid ((<>))
import Text.EscapeArtist

main = putEscLn $ Red 123 <> Blue 4
