module Lexer.Types (
    type',
) where

import Parser.AST
import Lexer.Data
import Lexer.String

type' :: Parser AST
type' = do
    _       <- spaces
    tname   <- string' "int" <|> string' "float" <|> string' "string" <|> string' "bool" <|> string' "void" <|> string' "char" <|> string' "long"
    _       <- spaces
    return $ Type tname
