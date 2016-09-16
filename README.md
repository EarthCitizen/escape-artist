# escape-artist

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

The data type used to perform text decoration is `Escapable`. This defines the constructors for text decoration. Each constructor takes a single argument which can be any type which has implemented the class `ToEscapable`. This means that all of the following are perfectly valid:

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

let redList = [Red 6, Red "6", Red '6', Red (6 :: Float), Red (6 :: Double)]
```

When constructors are combined with the application operator (`$`), the effects accumulate and wrap around the applied value:

```haskell
Red $ Underline $ Blink "Hello World!"
```

would be equivalent the following in XML:

```xml
<red>
    <underline>
        <blink>
            Hello World!
        </blink>
    </underline>
</red>
```

`Escapable` is an instance of `Monoid`, so a series of `Escapable`s can be appended together into a single value:

```haskell
{-# LANGUAGE ExtendedDefaultRules #-}

import Data.Monoid ((<>))

let series = Blink 5 <> Blue 6
```

### Foreground Color Constructors

`Black Red Green Yellow Magenta Cyan White`

### Background Color Constructors

`BgBlack BgRed BgGreen BgYellow BgBlue BgMagenta BgCyan BgWhite`

### Other Types of Constructors

Name           | Effect on Applied Value
-------------- | -----------------------
`Default`      | Default foreground color of the terminal
`BgDefault`    | Default background color of the terminal
`Inherited`    | Applies attributes of parent constructors. Useful for a value in a series with other `Escapable`s. See examples below.
`Normal`       | Even when other constructors are applied, the contained value will have the default attributes of the terminal
`Blink`        | Output blinks in terminal
`BlinkOff`     | NOT to end a blinking series, but rather to nest a non-blinking segment inside a series of blinking outputs
`Bright`       | Enables bright output for foreground colors
`BrightOff`    | NOT to end a bright series, but rather to nest a non-bright segment inside a series of bright outputs
`Underline`    | Underlines the output
`UnderlineOff` | NOT to end an underlined series, but rather to nest a non-underlined segment inside a series of underlined outputs
`Inverse`      | Switches the foreground and background colors
`InverseOff`   | NOT to end an inverse series, but rather to nest a non-inverse segment inside a series of inverse outputs
