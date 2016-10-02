{-# OPTIONS_HADDOCK hide #-}

module EscapeArtist.Constants where

black, red, green, yellow, blue, magenta, cyan, white :: String

black   = "\ESC[30m"
red     = "\ESC[31m"
green   = "\ESC[32m"
yellow  = "\ESC[33m"
blue    = "\ESC[34m"
magenta = "\ESC[35m"
cyan    = "\ESC[36m"
white   = "\ESC[37m"

bgblack, bgred, bggreen, bgyellow, bgblue, bgmagenta, bgcyan, bgwhite :: String

bgblack   = "\ESC[40m"
bgred     = "\ESC[41m"
bggreen   = "\ESC[42m"
bgyellow  = "\ESC[43m"
bgblue    = "\ESC[44m"
bgmagenta = "\ESC[45m"
bgcyan    = "\ESC[46m"
bgwhite   = "\ESC[47m"

reset, defaultColor, defaultBgColor :: String

reset = "\ESC[0m"

defaultColor = "\ESC[39m"

defaultBgColor = "\ESC[49m"

blinkOn, blinkOff, brightOn, brightOff :: String

blinkOn  = "\ESC[5m"
blinkOff = "\ESC[25m"

brightOn  = "\ESC[1m"
brightOff = "\ESC[22m"

underlineOn, underlineOff, inverseOn, inverseOff :: String

underlineOn = "\ESC[4m"
underlineOff = "\ESC[24m"

inverseOn = "\ESC[7m"
inverseOff = "\ESC[27m"

-- strikeOn = "\ESC[9m"
-- strikeOff = "\ESC[29m"
