module Lexer.String (
    char',
    string',
    space,
    spaces,
    name
) where

import Parser.AST
import Lexer.Satisfy

char' :: Char -> Parser Char
char' c = satisfy (== c)

string' :: String -> Parser String
string' [] = return []
string' (x:xs) = do
    _ <- char' x
    _ <- string' xs
    return (x:xs)

isSpace :: Char -> Bool
isSpace c = c == ' ' || c == '\t' || c == '\n' || c == '\r'

space :: Parser Char
space = satisfy isSpace

spaces :: Parser String
spaces = many space

isNameChar :: Char -> Bool
isNameChar c = c >= 'a' && c <= 'z'
            || c >= 'A' && c <= 'Z'
            || c >= '0' && c <= '9'
            || c == '_'

isLetter :: Char -> Bool
isLetter c = c >= 'a' && c <= 'z'
          || c >= 'A' && c <= 'Z'

name :: Parser String
name = do
    x <- satisfy isLetter
    xs <- many (satisfy isNameChar)
    return (x:xs)
