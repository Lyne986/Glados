module Parser.AST (
    Parser(..),
    Alternative(..),
) where

import Control.Applicative

newtype Parser a = Parser {
    runParser :: String -> Either String (a, String)
}

instance Functor Parser where
    fmap f (Parser p) = Parser run
        where
        run str = do
            (a, str') <- p str
            return (f a, str')

instance Applicative Parser where
    pure a = Parser run
        where
        run str = return (a, str)
    Parser p <*> Parser q = Parser run
        where
        run str = do
            (f, str') <- p str
            (a, str'') <- q str'
            return (f a, str'')

instance Monad Parser where
    Parser p >>= f = Parser run
        where
        run str = do
            (a, str') <- p str
            let (Parser b) = f a
            b str'

instance Alternative Parser where
    empty = Parser run
        where
            run _ = Left "error"
    Parser p <|> Parser q = Parser run
        where
        run str = case p str of
            Left _ -> q str
            Right b -> Right b

