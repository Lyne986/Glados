module Lexer.Variable (
    variable
) where

import Parser.AST
import Lexer.Data
import Lexer.String

variable :: Parser AST
variable = do
    _       <- spaces
    vname   <- name
    _       <- spaces
    return $ Var vname
