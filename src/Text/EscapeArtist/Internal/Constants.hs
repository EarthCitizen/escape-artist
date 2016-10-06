{-# OPTIONS_HADDOCK hide #-}

module Text.EscapeArtist.Internal.Constants where

fgBlack, fgRed, fgGreen, fgYellow, fgBlue, fgMagenta, fgCyan, fgWhite :: String

fgBlack   = "\ESC[30m"
fgRed     = "\ESC[31m"
fgGreen   = "\ESC[32m"
fgYellow  = "\ESC[33m"
fgBlue    = "\ESC[34m"
fgMagenta = "\ESC[35m"
fgCyan    = "\ESC[36m"
fgWhite   = "\ESC[37m"

bgBlack, bgRed, bgGreen, bgYellow, bgBlue, bgMagenta, bgCyan, bgWhite :: String

bgBlack   = "\ESC[40m"
bgRed     = "\ESC[41m"
bgGreen   = "\ESC[42m"
bgYellow  = "\ESC[43m"
bgBlue    = "\ESC[44m"
bgMagenta = "\ESC[45m"
bgCyan    = "\ESC[46m"
bgWhite   = "\ESC[47m"

defaultAll, defaultFgColor, defaultBgColor :: String

defaultAll = "\ESC[0m"

defaultFgColor = "\ESC[39m"

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
