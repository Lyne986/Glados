module Lexer.Number (
    digit,
    digits,
    number,
    floatnumber
) where

import Lexer.Satisfy
import Lexer.Data
import Lexer.String
import Parser.AST

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

digit :: Parser Char
digit = satisfy isDigit

digits :: Parser String
digits = do
    ds <- many digit
    return ds

floatnumber :: Parser AST
floatnumber = do
    _   <- spaces
    s   <- char' '+' <|> char' '-' <|> return '+'
    _   <- spaces
    ds  <- digits
    _   <- spaces
    _   <- char' '.'
    _   <- spaces
    ds2 <- digits
    _   <- spaces
    if ds == "" || ds2 == "" then do
        _ <- string' "ERROR BUG aefzfzfz"
        error "Number must have at least one digit"
    else
        case s of
            '+' -> return $ FloatNum $ read (ds ++ "." ++ ds2)
            '-' -> return $ FloatNum $ - (read (ds ++ "." ++ ds2))
            _   -> return $ FloatNum $ read (ds ++ "." ++ ds2)

number :: Parser AST
number = do
    _   <- spaces
    s   <- char' '+' <|> char' '-' <|> return '+'
    _   <- spaces
    ds  <- digits
    _   <- spaces
    if ds == "" then do
        _ <- string' "ERROR BUG aefzfzfz"
        error "Number must have at least one digit"
    else
        case s of
            '+' -> return $ Num $ read ds
            '-' -> return $ Num $ - (read ds)
            _   -> return $ Num $ read ds
