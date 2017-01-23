{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

import Data.Monoid (mempty, (<>))
import Data.Typeable (Typeable)
import Text.EscapeArtist

type FileName = String
type LineNumber = Integer
type ColumnNumber = Integer
data ErrorType = SyntaxError FileName LineNumber ColumnNumber deriving (Show)

deriving instance Typeable ErrorType

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

data ABC = A | B deriving (Show, Eq)

deriving instance Typeable ABC

instance ToEscapable ABC where
   toEscapable (A) = FgRed $ show A
   toEscapable (B) = FgGreen $ show B

instance (ToEscapable a) => ToEscapable (Maybe a) where
    toEscapable (Just a) = FgGreen "Just" <> Inherit " " <> FgYellow a
    toEscapable a = FgRed $ show a


main = do
    putEscLn $ gotSyntaxError
    putEscLn $ gotMessage
    putStrLn ""
    putEscLn A
    putEscLn B
    putEscLn $ Just 15
    putEscLn (Nothing :: Maybe Int)
