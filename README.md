# escape-artist

[![Build Status](https://travis-ci.org/EarthCitizen/escape-artist.svg?branch=master)](https://travis-ci.org/EarthCitizen/escape-artist)
[![Coverage Status](https://coveralls.io/repos/github/EarthCitizen/escape-artist/badge.svg?branch=master)](https://coveralls.io/github/EarthCitizen/escape-artist?branch=master)

A Haskell library for text decoration with ANSI escape sequences made easy. Decorate your terminal text easily and expressively.

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

FgRed 6
FgRed "6"
FgRed '6'
FgRed (6 :: Float)
FgRed (6 :: Double)
```

And can all dwell in the same list:

```haskell
{-# LANGUAGE ExtendedDefaultRules #-}

import Data.List (intersperse)
import Text.EscapeArtist

let redList = [FgRed 6, FgRed "6", FgRed '6', FgRed (6 :: Float), FgRed (6 :: Double)]

putEscLn $ mconcat $ intersperse (Inherit " ") redList
```

<img src="images/six.png?raw=true" height="20">

The following data types already come with an implementation of `ToEscapable`:

* `Char`
* `ByteString of Data.ByteString`
* `ByteString of Data.ByteString.Lazy`
* `Text of Data.Text`
* `Text of Data.Text.Lazy`
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
   toEscapable (A) = FgRed $ show A
   toEscapable (B) = FgGreen $ show B

instance (ToEscapable a) => ToEscapable (Maybe a) where
    toEscapable (Just a) = FgGreen "Just" <> Inherit " " <> FgYellow a
    toEscapable a = FgRed $ show a

putEscLn A
putEscLn B
putEscLn $ Just 15
putEscLn (Nothing :: Maybe Int)
```

<img src="images/abc_maybe.png?raw=true" height="150">

When constructors are combined with the application operator (`$`), the effects accumulate and wrap around the applied value:

```haskell
import Text.EscapeArtist

let combined = FgRed $ Underline $ Blink "Hello World!"
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

let series = FgYellow 5 <> FgWhite 6

putEscLn series
```

When a constructor is applied to a series of appended `Escapable`s using the `$`, the constructor will be applied to each member of the series.

```haskell
{-# LANGUAGE ExtendedDefaultRules #-}

import Data.Monoid ((<>))
import Text.EscapeArtist

let result = Underline $ FgYellow 5 <> FgWhite 6

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

`FgBlack FgRed FgGreen FgYellow FgBlue FgMagenta FgCyan FgWhite`

### Background Color

`BgBlack BgRed BgGreen BgYellow BgBlue BgMagenta BgCyan BgWhite`

### Other Types

Name           | Effect on Applied Value
-------------- | -----------------------
`FgDefault`    | Default foreground color of the terminal
`BgDefault`    | Default background color of the terminal
`Inherit`      | Applies attributes of parent constructors. Useful for a value interspersed in a series with other `Escapable`s. See examples below.
`Default`      | Even when other constructors are applied, the contained value will have the default attributes of the terminal
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

spacesInherit = FgRed '@' <> Inherit ' ' <> FgYellow '@' <> Inherit ' ' <> FgGreen '@'

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

underlines = Underline $ FgCyan "I am underlined" <> UnderlineOff " but I am not " <> FgMagenta "and I am over here"

putEscLn underlines
```

<img src="images/underline_off.png?raw=true" height="20">

The same type of functionality applies as well to `BlinkOff`, `BrightOff` and `InverseOff`.

### Operator ^$

This operator allows you to avoid parentheses in cases where you need to use `$` and `<>` in he same expression.

```haskell
import Data.Monoid ((<>))
import Text.EscapeArtist

op1 = Underline $ Bright ^$ FgGreen "GREEN" <> Default " " <> FgYellow "YELLOW"

putEscLn op1
```

<img src="images/high_prec_apply_op.png?raw=true" height="20">

Without `^$`, this would have to be written as:

```haskell
Underline $ (Bright $ FgGreen "GREEN") <> Default " " <> FgYellow "YELLOW"
```

## Advanced Examples

### Fun with Colors

```haskell
import Data.Monoid (mempty, (<>))
import Text.EscapeArtist

rainbowString :: String -> Escapable
rainbowString s = fn s (cycle [FgRed, FgWhite, FgGreen, FgBlue, FgYellow, FgCyan])
    where fn [] _ = mempty
          fn _ [] = mempty
          fn (s:ss) ca@(c:cs)
              | s `elem` " \t\n\r" = Inherit s <> fn ss ca
              | otherwise = c s <> fn ss cs

putEscLn $ rainbowString "Hello World!"
```

<img src="images/rainbow_string.png?raw=true" height="20">

### Colorize Sections of a String

```haskell
import Text.EscapeArtist
import Text.Regex

replaceNumbers :: String -> String
replaceNumbers searchIn = subRegex (mkRegex "([0-9]+)") searchIn (escToString $ FgRed "\\1")

putStrLn $ replaceNumbers "Line 7 of 23"
```

<img src="images/highlight_numbers.png?raw=true" height="20">

### Implement `ToEscapable` for Custom and Existing Data Types

```haskell
{-# LANGUAGE FlexibleInstances #-}

import Data.Monoid ((<>))
import Text.EscapeArtist

type FileName = String
type LineNumber = Integer
type ColumnNumber = Integer
data ErrorType = SyntaxError FileName LineNumber ColumnNumber deriving (Show)

instance ToEscapable ErrorType where
    toEscapable (SyntaxError fn ln cn) = Default "Syntax error in file "
                                       <> FgYellow ^$ Underline fn
                                       <> Default " at "
                                       <> FgRed (show ln ++ ":" ++ show cn)

instance ToEscapable (Either ErrorType String) where
    toEscapable (Left e) = toEscapable e
    toEscapable (Right m) = FgGreen m

gotSyntaxError :: Either ErrorType String
gotSyntaxError = Left $ SyntaxError "some/File.hs" 1 23

gotMessage :: Either ErrorType String
gotMessage = Right "Status OK"

putEscLn gotSyntaxError
putStrLn ""
putEscLn gotMessage
```

<img src="images/either_error.png?raw=true" height="53">
