{-# LANGUAGE ExtendedDefaultRules, NoMonomorphismRestriction #-}

import Data.Monoid ((<>))
import Text.EscapeArtist
--
r = Red 123 <> Blue 4

main = putEscLn r

-- main = print "ello"
