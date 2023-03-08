module Lexer.Satisfy (
    satisfy,
    fail'
) where

import Parser.AST

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser run
    where
        run [] = Left "error empty st"
        run (x:xs) = if f x then Right (x, xs) else Left "error satisfy"

fail' :: String -> Parser a
fail' msg = Parser $ const $ Left msg