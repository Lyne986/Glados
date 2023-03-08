module Lexer.Lexer (
    program,
    parseFile,
    prototype,
    return',
    whileloop,
    condition,
    forloop,
    operation,
) where

import Lexer.Data
import Lexer.String
import Lexer.Number
import Lexer.Types
import Lexer.Variable
import Parser.AST

prototype :: Parser AST
prototype = do
    _       <- spaces
    ptype   <- type'
    _       <- spaces
    fname   <- name
    _       <- spaces
    params  <- parametersDefinition <|> emptyParametersDefinition
    _       <- spaces
    _       <- char' ';'
    _       <- spaces
    return $ ProtoType ptype fname params

prgm_lines :: Parser AST
prgm_lines = do
    _       <- spaces
    line    <- function <|> prototype <|> terminatedLine'
    _       <- spaces
    return line

program :: Parser [AST]
program = do
    _   <- spaces
    l   <- many prgm_lines
    _   <- spaces
    return l

parseFile :: String -> Either String ([AST], String)
parseFile fileContent = runParser program fileContent

parameterDefinition :: Parser AST
parameterDefinition = do
    _       <- spaces
    ptype   <- type'
    _       <- spaces
    pname   <- name
    _       <- spaces
    _       <- char' ','
    _       <- spaces
    return $ Param ptype pname

finalParameterDefinition :: Parser AST
finalParameterDefinition = do
    _       <- spaces
    ptype   <- type'
    _       <- spaces
    pname   <- name
    _       <- spaces
    return $ Param ptype pname

parametersDefinition :: Parser [AST]
parametersDefinition = do
    _           <- char' '('
    _           <- spaces
    params      <- many parameterDefinition
    _           <- spaces
    last_param  <- finalParameterDefinition
    _           <- spaces
    _           <- char' ')'
    _           <- spaces
    return $ params ++ [last_param]

emptyParametersDefinition :: Parser [AST]
emptyParametersDefinition = do
    _           <- char' '('
    _           <- spaces
    _           <- char' ')'
    _           <- spaces
    return []

finalChar :: Parser Char
finalChar = do
    _       <- spaces
    c       <- char' ';'
    _       <- spaces
    return c

whileloop :: Parser AST
whileloop = do
    _       <- spaces
    _       <- string' "while"
    _       <- spaces
    _       <- char' '('
    _       <- spaces
    cond    <- operation <|> functionCall <|> variable  <|> floatnumber <|> number
    _       <- spaces
    _       <- char' ')'
    _       <- spaces
    _       <- char' '{'
    _       <- spaces
    ops     <- many (terminatedLine' <|> whileloop <|> forloop <|> condition)
    _       <- spaces
    _       <- char' '}'
    _       <- spaces
    return $ WhileLoop cond ops

emptyfor :: Parser AST
emptyfor = return $ Empty 0

forloop :: Parser AST
forloop = do
    _       <- spaces
    _       <- string' "for"
    _       <- spaces
    _       <- char' '('
    _       <- spaces
    init    <- defineVar <|> emptyfor
    _       <- spaces
    _       <- char' ';'
    _       <- spaces
    cond    <- operation <|> functionCall <|> variable <|> floatnumber <|> number <|> emptyfor
    _       <- spaces
    _       <- char' ';'
    _       <- spaces
    incr    <- increment <|> operation <|> emptyfor
    _       <- spaces
    _       <- char' ')'
    _       <- spaces
    _       <- char' '{'
    _       <- spaces
    ops     <- many (terminatedLine' <|> whileloop <|> forloop <|> condition)
    _       <- spaces
    _       <- char' '}'
    _       <- spaces
    return $ ForLoop init cond incr ops

condition :: Parser AST
condition = do
    _       <- spaces
    _       <- string' "if"
    _       <- spaces
    _       <- char' '('
    _       <- spaces
    cond    <- operation <|> functionCall <|> variable  <|> floatnumber <|> number
    _       <- spaces
    _       <- char' ')'
    _       <- spaces
    _       <- char' '{'
    _       <- spaces
    ops     <- many (terminatedLine' <|> whileloop <|> forloop <|> condition)
    _       <- spaces
    _       <- char' '}'
    _       <- spaces
    else'   <- else_keyword <|> elseif' <|> return (Otherwise [])
    _       <- spaces
    return $ Condition cond ops else'

else_keyword :: Parser AST
else_keyword = do
    _       <- string' "else" <|> string' "otherwise"
    _       <- spaces
    _       <- char' '{'
    _       <- spaces
    ops     <- many (terminatedLine' <|> whileloop <|> forloop <|> condition)
    _       <- spaces
    _       <- char' '}'
    _       <- spaces
    return $ Otherwise ops

elseif' :: Parser AST
elseif' = do
    _       <- string' "else" <|> string' "otherwise"
    _       <- spaces
    if'     <- condition
    return $ if'

defineVar :: Parser AST
defineVar = do 
    _       <- spaces
    vtype   <- type'
    _       <- spaces
    vname   <- name
    _       <- spaces
    _       <- char' '='
    _       <- spaces
    value   <- operation <|> functionCall <|> variable  <|> floatnumber <|> number
    _       <- spaces
    return $ DefineVar vtype vname value

line' :: Parser AST
line' = do
    _       <- spaces
    op      <- defineVar <|> increment <|> operationEqual <|> operation <|> functionCall <|> variable <|> floatnumber <|> number 
    _       <- spaces
    return op

return' :: Parser AST
return' = do
    _       <- spaces
    _       <- string' "return"
    _       <- spaces
    op      <- increment <|> operationEqual <|> operation <|> functionCall <|> variable <|> floatnumber <|> number 
    _       <- spaces
    return $ Ret op

terminatedLine' :: Parser AST
terminatedLine' = do
    _       <- spaces
    op      <- return' <|> line'
    _       <- spaces
    _       <- finalChar
    return op

function :: Parser AST
function = do
    _           <- spaces
    ret_type    <- type'
    _           <- spaces
    mb_fname    <- name
    _           <- spaces
    params      <- emptyParametersDefinition <|> parametersDefinition
    _           <- spaces
    _           <- char' '{'
    _           <- spaces
    ops         <- many (terminatedLine' <|> whileloop <|> forloop <|> condition)
    _           <- spaces
    _           <- char' '}'
    _           <- spaces
    return $ Func ret_type mb_fname params ops

parameterLine :: Parser AST
parameterLine = do
    _       <- spaces
    l       <- line'
    _       <- spaces
    _       <- char' ','
    _       <- spaces
    return l

finalParameterLine :: Parser AST
finalParameterLine = do
    _       <- spaces
    l       <- line'
    _       <- spaces
    return l

parametersLines :: Parser [AST]
parametersLines = do
    _           <- char' '('
    _           <- spaces
    params      <- many parameterLine
    _           <- spaces
    last_param  <- finalParameterLine
    _           <- spaces
    _           <- char' ')'
    _           <- spaces
    return $ params ++ [last_param]

functionCall :: Parser AST
functionCall = do
    _           <- spaces
    fname       <- name
    _           <- spaces
    params      <- emptyParametersDefinition <|> parametersLines
    _           <- spaces
    return $ FuncCall fname params

-- OPERATION

validMember :: Parser AST
validMember = functionCall <|> variable <|> floatnumber <|> number

operationEqual :: Parser AST
operationEqual = do
    _   <- spaces
    t  <- term
    _   <- spaces
    s   <- string' "+=" <|> string' "-=" <|> string' "*=" <|> string' "/=" <|> string' "%="
    _   <- spaces
    t2  <- operation
    _   <- spaces
    case s of
        "+=" -> return $ Op t "=" (Op t "+" t2)
        "-=" -> return $ Op t "=" (Op t "-" t2)
        "*=" -> return $ Op t "=" (Op t "*" t2)
        "/=" -> return $ Op t "=" (Op t "/" t2)
        "%=" -> return $ Op t "=" (Op t "%" t2)

operation :: Parser AST
operation = do
    _   <- spaces
    t  <- term
    _   <- spaces
    ts <- many signTerm
    _   <- spaces
    return (foldl f t ts)
    where
        signTerm = do
            _   <- spaces
            s   <- string' ">=" <|> string' "<=" <|> string' "==" <|> string' "!=" <|> string' "+=" <|> string' "-=" <|> string' "*=" <|> string' "/=" <|> string' "%=" <|> string' "<" <|> string' ">" <|> string' "="
            _   <- spaces
            t   <- term
            _   <- spaces
            return (s, t)

        f :: AST -> (String, AST) -> AST
        f n1 (s, n2) = Op n1 s n2

term :: Parser AST
term = do
    _   <- spaces
    t  <- factor
    _   <- spaces
    fs <- many signFactor
    _   <- spaces
    return (foldl f t fs)
    where
        signFactor = do
            _   <- spaces
            s   <- string' "+" <|> string' "-" 
            _   <- spaces
            t   <- factor
            _   <- spaces
            return (s, t)

        f :: AST -> (String, AST) -> AST
        f n1 (s, n2) = Op n1 s n2

factor :: Parser AST
factor = do
    _   <- spaces
    n  <- posprimary
    _   <- spaces
    ns <- many signNumber
    _   <- spaces
    return (foldl f n ns)
    where
        signNumber = do
            _   <- spaces
            s   <- string' "*" <|> string' "/" <|> string' "%"
            _   <- spaces
            t   <- posprimary
            _   <- spaces
            return (s, t)

        f :: AST -> (String, AST) -> AST
        f n1 (s, n2) = Op n1 s n2

posprimary :: Parser AST
posprimary = signPrimary <|> primary
    where
        signPrimary = do
            _   <- spaces
            s   <- string' "+" <|> string' "-"
            t   <- posprimary
            return $ if s == "+" 
            then t 
            else Op (Num 0) "-" t

primary :: Parser AST
primary = parentesis <|> validMember
    where
        parentesis = do
            _   <- spaces
            _   <- char' '('
            _   <- spaces
            n   <- operation
            _   <- spaces
            _   <- char' ')'
            _   <- spaces
            return n

increment :: Parser AST
increment = do
    _   <- spaces
    v   <- variable
    _   <- spaces
    s   <- string' "++" <|> string' "--"
    _   <- spaces
    if s == "++"
    then return $ Op v "=" (Op v "+" (Num 1))
    else return $ Op v "=" (Op v "-" (Num 1))

