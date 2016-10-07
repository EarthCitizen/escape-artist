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

mkSyntaxError :: FileName -> LineNumber -> ColumnNumber -> Either ErrorType String
mkSyntaxError fn ln cn = Left $ SyntaxError fn ln cn

mkStatusOK :: Either ErrorType String
mkStatusOK = Right "Status OK"

main = do
    putEscLn $ mkSyntaxError "some/File.hs" 1 23
    putStrLn ""
    putEscLn mkStatusOK
