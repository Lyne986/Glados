module Lexer.Data (
    AST(..)
) where

data AST = 
    FloatNum Float              | -- Float number
    Num Integer                 | -- Classic number
    Type String                 | -- Type of a variable or a return of function
    Op AST String AST           | -- Opération like (1 + 2)
    Func AST String [AST] [AST] | -- Function with return type, name, parameters and lines 
    Param AST String            | -- Parameter with type and name
    FuncCall String [AST]       | -- Function call with name and parameters
    DefineVar AST String AST    | -- Define a variable with type, name and value
    Var String                  | -- Variable name 
    WhileLoop AST [AST]         | -- Loop with condition and lines
    ForLoop AST AST AST [AST]   | -- For loop with init, condition, increment and lines
    Condition AST [AST] AST     | -- Condition with condition, lines if true and lines otherwise if and else exist
    Otherwise [AST]             | -- Content of else or otherwise
    Empty  Integer              | -- Nothing
    ProtoType AST String [AST]  | -- Prototype of a function with return type, name and parameters
    Ret AST                     |  -- Return of a function
    DefineArray AST String AST  | -- Define an array with type, name and values
    Array [AST]                   -- Array values
    deriving (Show)
