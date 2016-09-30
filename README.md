# escape-artist

[![Build Status](https://travis-ci.org/EarthCitizen/escape-artist.svg?branch=master)](https://travis-ci.org/EarthCitizen/escape-artist)
[![Coverage Status](https://coveralls.io/repos/github/EarthCitizen/escape-artist/badge.svg?branch=master)](https://coveralls.io/github/EarthCitizen/escape-artist?branch=master)

A Haskell library for ASCII escape codes made easy. Decorate your terminal text expressively while staying in your normal Haskell coding style.

## Getting Started

### Building from Source
#### Prerequisites

To build this project from source, you will need to install stack. See https://docs.haskellstack.org/en/stable/README/#how-to-install for detailed installation instructions for your operating system.

#### Building

```
git clone https://github.com/EarthCitizen/escape-artist
cd escape-artist
stack setup
stack build
```

### Using

The data type used to perform text decoration is `Escapable`. This defines the constructors for the decoration. Each constructor takes a single argument which can be any type which has implemented the class `ToEscapable`. This means that all of the following are perfectly valid:

```haskell
{-# LANGUAGE ExtendedDefaultRules #-}

Red 6
Red "6"
Red '6'
Red (6 :: Float)
Red (6 :: Double)
```

And can all dwell in the same list:

```haskell
{-# LANGUAGE ExtendedDefaultRules #-}

import Data.List (intersperse)
import Text.EscapeArtist

let redList = [Red 6, Red "6", Red '6', Red (6 :: Float), Red (6 :: Double)]

putEscLn $ mconcat $ intersperse (Inherit " ") redList
```

<img src="images/six.png?raw=true" height="20">

The following data types already come with an implementation of `ToEscapable`:

* `Char`
* `Data.ByteString.ByteString`
* `Data.ByteString.Lazy.ByteString`
* `Data.Text.Text`
* `Data.Text.Lazy.Text`
* `Double`
* `Float`
* `Int`
* `Integer`
* `String`
* `Word`
* `Word8`
* `Word16`
* `Word32`
* `Word64`

Implementing `ToEscapable` for other data types is fairly simple:

```haskell
import Data.Monoid ((<>))
import Text.EscapeArtist

data ABC = A | B deriving (Show, Eq)

instance ToEscapable ABC where
   toEscapable (A) = Red $ show A
   toEscapable (B) = Green $ show B

instance (ToEscapable a) => ToEscapable (Maybe a) where
    toEscapable (Just a) = Green "Just" <> Inherit " " <> Yellow a
    toEscapable a = Red $ show a

putEscLn A
putEscLn B
putEscLn $ Just 15
putEscLn (Nothing :: Maybe Int)
```

<img src="images/abc_maybe.png?raw=true" height="150">

When constructors are combined with the application operator (`$`), the effects accumulate and wrap around the applied value:

```haskell
import Text.EscapeArtist

let combined = Red $ Underline $ Blink "Hello World!"
```

would be equivalent to the following in XML:

```xml
<red>
    <underline>
        <blink>
            Hello World!
        </blink>
    </underline>
</red>
```

*NOTE:* This library does not produce nor interact with XML. This example is just for the purpose of explanation.

`Escapable` is an instance of `Monoid`, so a series of `Escapable`s can be appended together into a single value:

```haskell
{-# LANGUAGE ExtendedDefaultRules #-}

import Data.Monoid ((<>))
import Text.EscapeArtist

let series = Yellow 5 <> White 6

putEscLn series
```

When a constructor is applied to a series of appended `Escapable`s using the `$`, the constructor will be applied to each member of the series.

```haskell
{-# LANGUAGE ExtendedDefaultRules #-}

import Data.Monoid ((<>))
import Text.EscapeArtist

let result = Underline $ Yellow 5 <> White 6

putEscLn result
```

<img src="images/56yw.png?raw=true" height="20">

XML equivalent:
```XML
<underline>
    <yellow>5</yellow>
</underline>
<underline>
    <white>6</white>
</underline>
```
*NOTE:* The `Underline` is re-applied to each member of the series, and not once for all of them.

## Constructors

### Foreground Color

`Black Red Green Yellow Magenta Cyan White`

### Background Color

`BgBlack BgRed BgGreen BgYellow BgBlue BgMagenta BgCyan BgWhite`

### Other Types

Name           | Effect on Applied Value
-------------- | -----------------------
`Default`      | Default foreground color of the terminal
`BgDefault`    | Default background color of the terminal
`Inherit`    | Applies attributes of parent constructors. Useful for a value interspersed in a series with other `Escapable`s. See examples below.
`Normal`       | Even when other constructors are applied, the contained value will have the default attributes of the terminal
`Blink`        | Output blinks in terminal
`BlinkOff`     | NOT to end a blinking series, but rather to nest a non-blinking segment inside a series of blinking outputs
`Bright`       | Enables bright output for foreground colors
`BrightOff`    | NOT to end a bright series, but rather to nest a non-bright segment inside a series of bright outputs
`Underline`    | Underlines the output
`UnderlineOff` | NOT to end an underlined series, but rather to nest a non-underlined segment inside a series of underlined outputs
`Inverse`      | Switches the foreground and background colors
`InverseOff`   | NOT to end an inverse series, but rather to nest a non-inverse segment inside a series of inverse outputs

## Functions

Name          | Description
------------- | -----------
`escToString` | Renders anything implementing `ToEscapable` to a `String`
`putEsc`      | Renders anything implementing `ToEscapable` to a `String`, then writes it to standard out
`putEscLn`    | Renders anything implementing `ToEscapable` to a `String`, then writes it to standard out followed by a newline

## Operators

Symbol | Purpose
------ | -------
`^$`   | Same as `$`, but one level of precedence higher than `<>` for avoiding the use of parentheses when needing to use `$` in the same expression as `<>`. See examples below.

## Examples

### Inherit

```haskell
import Data.Monoid ((<>))
import Text.EscapeArtist

spacesInherit = Red '@' <> Inherit ' ' <> Yellow '@' <> Inherit ' ' <> Green '@'

putEscLn spacesInherit
```

<img src="images/inherit_none.png?raw=true" height="20">

```haskell
putEscLn $ Underline spacesInherit
```

<img src="images/inherit_underline.png?raw=true" height="22">

```haskell
putEscLn $ Inverse spacesInherit
```

<img src="images/inherit_inverse.png?raw=true" height="24">

```haskell
putEscLn $ BgBlue spacesInherit
```

<img src="images/inherit_bgblue.png?raw=true" height="24">

### UnderlineOff

```haskell
import Data.Monoid ((<>))
import Text.EscapeArtist

underlineOff = Underline $ Cyan "I am underlined" <> UnderlineOff " but I am not " <> Magenta "and I am over here"

putEscLn $ underlineOff
```

<img src="images/underline_off.png?raw=true" height="20">

The same type of functionality applies as well to `BlinkOff`, `BrightOff` and `InverseOff`.

### Operator ^$

This operator allows you to avoid parentheses in cases where you need to use `$` and `<>` in he same expression.

```haskell
import Data.Monoid ((<>))
import Text.EscapeArtist

op1 = Underline $ Bright ^$ Green "GREEN" <> Normal " " <> Yellow "YELLOW"

putEscLn op1
```

<img src="images/high_prec_apply_op.png?raw=true" height="20">

Without `^$`, this would have to be written as:

```haskell
Underline $ (Bright $ Green "GREEN") <> Normal " " <> Yellow "YELLOW"
```

### Some Slightly More Advanced Examples

```haskell
import Data.Monoid (mempty, (<>))
import Text.EscapeArtist

rainbowString :: String -> Escapable
rainbowString s = fn s (cycle [Red, White, Green, Blue, Yellow, Cyan])
    where fn [] _ = mempty
          fn _ [] = mempty
          fn (s:ss) ca@(c:cs)
              | s `elem` " \t\n\r" = Inherit s <> fn ss ca
              | otherwise = c s <> fn ss cs

putEscLn $ rainbowString "Hello World!"
```

<img src="images/rainbow_string.png?raw=true" height="20">

```haskell
import Text.EscapeArtist
import Text.Regex

replaceNumbers :: String -> String
replaceNumbers searchIn = subRegex (mkRegex "([0-9]+)") searchIn (escToString $ Red "\\1")

putStrLn $ replaceNumbers "Line 7 of 23"
```

<img src="images/highlight_numbers.png?raw=true" height="20">

```haskell
{-# LANGUAGE FlexibleInstances #-}

import Data.Monoid ((<>))
import Text.EscapeArtist

type FileName = String
type LineNumber = Integer
type ColumnNumber = Integer
data ErrorType = SyntaxError FileName LineNumber ColumnNumber deriving (Show)

instance ToEscapable ErrorType where
    toEscapable (SyntaxError fn ln cn) = Normal "Syntax error in file "
                                       <> Yellow ^$ Underline fn
                                       <> Normal " at "
                                       <> Red (show ln ++ ":" ++ show cn)

instance ToEscapable (Either ErrorType String) where
    toEscapable (Left e) = toEscapable e
    toEscapable (Right m) = Green m

gotSyntaxError :: Either ErrorType String
gotSyntaxError = Left $ SyntaxError "some/File.hs" 1 23

gotMessage :: Either ErrorType String
gotMessage = Right "Status OK"

putEscLn $ gotSyntaxError
putStrLn ""
putEscLn $ gotMessage
```

<img src="images/either_error.png?raw=true" height="53">
