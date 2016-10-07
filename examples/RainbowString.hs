{-# LANGUAGE FlexibleInstances #-}

import Data.Monoid (mempty, (<>))
import Text.EscapeArtist

rainbowString :: String -> Escapable
rainbowString s = fn s (cycle [FgRed, FgWhite, FgGreen, FgBlue, FgYellow, FgCyan])
    where fn [] _ = mempty
          fn _ [] = mempty
          fn (s:ss) ca@(c:cs)
              | s `elem` " \t\n\r" = Inherit s <> fn ss ca
              | otherwise = c s <> fn ss cs

main = putEscLn $ rainbowString "Hello World!"
