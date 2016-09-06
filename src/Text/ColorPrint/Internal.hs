module Text.ColorPrint.Internal where

black   = "\x1B[30m"
red     = "\x1B[31m"
green   = "\x1B[32m"
yellow  = "\x1B[33m"
blue    = "\x1B[34m"
magenta = "\x1B[35m"
cyan    = "\x1B[36m"
white   = "\x1B[37m"

bgblack   = "\x1B[40m"
bgred     = "\x1B[41m"
bggreen   = "\x1B[42m"
bgyellow  = "\x1B[43m"
bgblue    = "\x1B[44m"
bgmagenta = "\x1B[45m"
bgcyan    = "\x1B[46m"
bgwhite   = "\x1B[47m"

reset = "\x1B[0m"

defaultColor = "\x1B[39m"

defaultBgColor = "\x1B[49m"

brightOn  = "\x1B[1m"
brightOff = "\x1B[22m"

underlineOn = "\x1B[4m"
underlineOff = "\x1B[24m"

inverseOn = "\x1B[7m"
inverseOff = "\x1B[27m"

strikeOn = "\x1B[9m"
strikeOff = "\x1B[29m"

class ToString a where
    toString :: a -> String
