{-# LANGUAGE ExtendedDefaultRules #-}

import Data.Monoid ((<>))
import Text.ColorPrint

main = putColorLn $ Red 123 <> Blue 4
