{-# LANGUAGE OverloadedStrings #-}

module Codegen.Functions (
    createFunction,
    createPrototype
) where

import Lexer.Data as AST
import Codegen.Types
import Codegen.Intern.Cond
import Codegen.Intern.Functions
import Codegen.Intern.Parameters
import Codegen.Intern.Types as T
import Codegen.Intern.Blocks
import Codegen.Intern.Stack
import Codegen.Intern.Operation
import Codegen.Intern.Convert

import LLVM.AST
import LLVM.AST.Global
import LLVM.AST.Constant
import Control.Monad.State

import Data.ByteString.Short.Internal as BS

genFunction :: Type -> BS.ShortByteString -> [(Type, BS.ShortByteString)] -> [BasicBlock] -> Definition
genFunction retType fname fparams fblocks    = GlobalDefinition functionDefaults
    {
        name = Name fname,
        returnType = retType,
        parameters = params fparams,
        basicBlocks = fblocks
    }

-- Condition

genIfBlock :: Maybe BasicBlock -> BasicBlock -> AST -> [AST] -> State (Int, [(Type, String)], [AST]) [BasicBlock]
genIfBlock parent exitBlock _ lines' = do
    genIfName   <- generateName "if"
    ifBlock     <- genBlockWithName genIfName parent
    ifBlock'    <- addTerminator (jmpToBlock exitBlock) ifBlock
    blocks      <- convertBlocks parent ifBlock' lines'
    return blocks

genIfCond :: BasicBlock -> BasicBlock -> BasicBlock -> AST -> State (Int, [(Type, String)], [AST]) BasicBlock
genIfCond block bTrue bFalse (Op a op b)    = do
    (block', r) <- solveInstr block (Op a op b)
    (condL, _)  <- extractJust r
    -- error (show condL)
    ifCondName      <- generateName "ifCond"
    block''         <- addInstructionWName ifCondName condL block'
    block'''        <- addTerminator (jmpIfTrue (var' bool ifCondName) bTrue bFalse) block''
    return block'''
genIfCond _ _ _ _                           = error "Invalid condition"

genIfAloneBlock :: Maybe BasicBlock -> BasicBlock -> [AST] -> State (Int, [(Type, String)], [AST]) [BasicBlock]
genIfAloneBlock parent block ((Condition cond condLines _):xs)  = do
    randomName  <- generateName "block"
    exitBlock   <- genBlockWithName randomName parent
    ifBlocks    <- genIfBlock (Just exitBlock) exitBlock cond condLines
    block'      <- genIfCond block (ifBlocks !! 0) exitBlock cond
    nexts       <- convertBlocks (Just exitBlock) exitBlock xs
    return ([block'] ++ ifBlocks ++ nexts)
genIfAloneBlock _ _ _                                           = error "Invalid if block"

genIfWithElse :: Maybe BasicBlock -> BasicBlock -> [AST] -> State (Int, [(Type, String)], [AST]) [BasicBlock]
genIfWithElse parent block ((Condition cond condLines (Otherwise elseLines)):xs)   = do
    elseName        <- generateName "else"
    exitBlockName   <- generateName "block"

    exitBlock       <- genBlockWithName exitBlockName parent

    ifBlocks        <- genIfBlock (Just exitBlock) exitBlock cond condLines
    elseBlock       <- genBlockWithName elseName parent
    elseBlock'      <- addTerminator (jmpToBlock exitBlock) elseBlock

    elseBlock''     <- convertBlocks (Just exitBlock) elseBlock' elseLines
    block'          <- genIfCond block (ifBlocks !! 0) (elseBlock'' !! 0) cond
    nexts           <- convertBlocks parent exitBlock xs
    return ([block'] ++ nexts ++ ifBlocks ++ elseBlock'')

genIfWithElse _ _ _                                                           = error "Invalid if block"

-- Var

genDefineVar :: BasicBlock -> String -> String -> AST -> State (Int, [(Type, String)], [AST]) BasicBlock
genDefineVar block t vname value'   = do
    let vtype           = convertType t
    let memInstr        = alloca' vtype
    randomName          <- generateName "defVar"

    _                   <- addVarInContext (ptr vtype) vname
    block'              <- addInstructionWName vname memInstr block
    case value' of
        (Var valueName)  -> do
            v   <- extractVarAsOperand valueName
            case isPointer v of
                True    -> do
                    loadName        <- generateName "load"
                    block''         <- addInstructionWName loadName (load' (var' (ptr vtype) valueName)) block'
                    let block'''    = addInstruction (store' (var' vtype vname) (var' vtype loadName)) block''
                    return block'''
                False   -> do
                    let block''     = addInstruction (store' (var' vtype vname) (var' vtype valueName)) block'
                    return block''
        (Num n)         -> do
            let convertInstr    = convertTypeLLVM i32 vtype (i32_as_op n)
            block''             <- addInstructionWName randomName convertInstr block'
            let block'''        = addInstruction (store' (var' vtype vname) (var' vtype randomName)) block''
            return block'''
        _              -> do
            (block'', r)    <- solveInstr block' value'
            (instr, _)      <- extractJust r
            block'''            <- addInstructionWName randomName instr block''
            let newBlock        = addInstruction (store' (var' vtype vname) (var' vtype randomName)) block'''
            return newBlock

-- Functions

convertParameters :: [AST] -> State (Int, [(Type, String)], [AST]) [(Type, BS.ShortByteString)]
convertParameters []                            = return []
convertParameters ((Param (Type t) pname):xs)   = do
    rest        <- convertParameters xs
    paramName   <- generateName (pname ++ "_param")
    _           <- addVarInContext (ptr (convertType t)) pname
    return ((convertType t, stringToShortByteString paramName):rest)
convertParameters _                             = error "Invalid parameter"

splitVarName :: String -> String
splitVarName []     = []
splitVarName (x:xs) = if x == '_' then [] else x : splitVarName xs

generateParamVars :: BasicBlock -> [(Type, BS.ShortByteString)] -> State (Int, [(Type, String)], [AST]) BasicBlock
generateParamVars block []                          = return block
generateParamVars block ((t, pname):xs)             = do
    let memInstr    = alloca' t
    let pnameSplit  = splitVarName (byteStringToString pname)
    block'          <- addInstructionWName pnameSplit memInstr block
    let block''     = addInstruction (store' (var' t pnameSplit) (var' t (byteStringToString pname))) block'
    generateParamVars block'' xs

createFunction :: AST -> String -> [AST] -> [AST] -> State (Int, [(Type, String)], [AST]) Definition
createFunction (Type retType) fname fparams flines   = do
    _               <- addFuncInContext (Func (Type retType) fname fparams flines)
    let retType'    = convertType retType
    let fname'      = stringToShortByteString fname
    fparams'        <- convertParameters fparams
    emptyBlock      <- genEmptyBlock
    emptyBlock'     <- generateParamVars emptyBlock fparams'
    fblocks'        <- convertBlocks Nothing emptyBlock' flines
    return (genFunction retType' fname' fparams' fblocks')
createFunction _ _ _ _                              = error "Invalid function definition"

-- Function call

genParams :: BasicBlock -> [AST] -> State (Int, [(Type, String)], [AST]) (BasicBlock, [(Type, String)])
genParams block []                      = return (block, [])
genParams block ((Param (Type t) n):xs) = do
    (block', rest) <- genParams block xs
    return (block', ((convertType t, n):rest))
genParams block (op:xs)
    | isJust (checkOperand op)          = do
        case op of
            (Var vname) -> do
                (block', vOperand)  <- extractOperandFromAST block (Var vname)
                (block'', rest)     <- genParams block' xs
                return (block'', ((typeOf vOperand, getVarName vOperand):rest))
            (Num n) -> do
                paramName       <- generateName "param"
                block'          <- addInstructionWName paramName (i32_as_instr n) block
                (block'', rest) <- genParams block' xs
                return (block'', ((i32, paramName):rest))
            _       -> error "Invalid operand in genParams"
    | otherwise                         = do
        (block', r)         <- solveInstr block op
        (instr, t)          <- extractJust r
        paramName           <- generateName "param"
        block''             <- addInstructionWName paramName instr block'
        (block''', rest)    <- genParams block'' xs
        return (block''', ((t, paramName):rest))

convertToOperand :: [(Type, String)] -> State (Int, [(Type, String)], [AST]) [Operand]
convertToOperand [] = return []
convertToOperand ((t, n):xs) = do
    rest <- convertToOperand xs
    return ((var' t n):rest)

convertParamsToType :: BasicBlock -> [(Type, String)] -> [AST] -> State (Int, [(Type, String)], [AST]) (BasicBlock, [(Type, String)])
convertParamsToType block [] [] = return (block, [])
convertParamsToType block ((t, n):xs) ((Param (Type t') n'):ys) = do
    let typeTo          = convertType t'
    let convertInstr    = convertTypeLLVM t typeTo (var' t n)
    paramName           <- generateName "convertParam"
    block'              <- addInstructionWName paramName convertInstr block
    (block'', rest)     <- convertParamsToType block' xs ys
    return (block'', ((typeTo, paramName):rest))
convertParamsToType _ _ _ = error "Invalid parameters"

genFuncCallInstr :: BasicBlock -> String -> [AST] -> State (Int, [(Type, String)], [AST]) (BasicBlock, (Instruction, Type))
genFuncCallInstr block fname fargs = do
    f                           <- getFuncFromContext fname
    let ((Type retType), args)  = case f of
            (Func (Type t) _ a _)   -> ((Type t), a)
            _                       -> error "Invalid function"
    (block', fargs')            <- genParams block fargs
    (block'', fargs'')          <- convertParamsToType block' fargs' args
    fargs'''                    <- convertToOperand fargs''
    let types                   = map fst fargs''
    return ((block'', (call' (getFunc (convertType retType) fname types) fargs''', convertType retType)))

genFuncCallWName :: BasicBlock -> String -> String -> [AST] -> State (Int, [(Type, String)], [AST]) BasicBlock
genFuncCallWName block resName fname fargs = do
    (block', (instr, _)) <- genFuncCallInstr block fname fargs
    addInstructionWName resName instr block'

-- Return

genReturn :: BasicBlock -> AST -> State (Int, [(Type, String)], [AST]) BasicBlock
genReturn block (Num n)                 = do
    f           <- getCurrentFunc
    let retType = case f of
            (Func (Type t) _ _ _)   -> t
            _                       -> error "Invalid function"
    let term    = ret' (i_as_op n (convertType retType))
    addTerminator term block

genReturn block (FloatNum n)               = do
    f           <- getCurrentFunc
    let retType = case f of
            (Func (Type t) _ _ _)   -> t
            _                       -> error "Invalid function"
    let term    = ret' (f_as_op n (convertType retType))
    addTerminator term block

genReturn block (Var v)                 = do
    f           <- getCurrentFunc
    let retType = case f of
            (Func (Type t) _ _ _)   -> t
            _                       -> error "Invalid function"
    vOperand    <- extractVarAsOperand v
    case isPointer vOperand of
        True    -> do
            resName     <- generateName "return"
            let instr   = load' vOperand
            newBlock    <- addInstructionWName resName instr block
            convertName <- generateName "convertReturn"
            let convertInstr    = convertTypeLLVM (extractTypeFromPointer (typeOf vOperand)) (convertType retType) (var' (typeOf vOperand) resName)
            newBlock'   <- addInstructionWName convertName convertInstr newBlock
            let term    = ret' (var' (convertType retType) convertName)
            addTerminator term newBlock'
        False   -> do
            let term    = ret' (var' (convertType retType) (getVarName vOperand))
            addTerminator term block

genReturn block (Op a op b)             = do
    f           <- getCurrentFunc
    let retType = case f of
            (Func (Type t) _ _ _)   -> t
            _                       -> error "Invalid function"
    resName     <- generateName "return"
    (b1, r)     <- solveInstr block (Op a op b)
    (instr, t)  <- extractJust r
    newBlock    <- addInstructionWName resName instr b1
    case t /= (convertType retType) of
        True    -> do
            convertName <- generateName "convertReturn"
            let convertInstr    = convertTypeLLVM t (convertType retType) (var' t resName)
            newBlock'   <- addInstructionWName convertName convertInstr newBlock
            let term    = ret' (var' (convertType retType) convertName)
            addTerminator term newBlock'
        False   -> do
            let term    = ret' (var' (convertType retType) resName)
            addTerminator term newBlock

genReturn block (FuncCall fname fargs)   = do
    f           <- getCurrentFunc
    let retType = case f of
            (Func (Type t) _ _ _)   -> t
            _                       -> error "Invalid function"
    resName     <- generateName "return"
    newBlock    <- genFuncCallWName block resName fname fargs
    let term    = ret' (var' (convertType retType) resName)
    addTerminator term newBlock

genReturn _ _                           = error "Invalid return"

-- Operation

genInstruction :: Operand -> String -> Operand -> Instruction
genInstruction a "+" b  = add' a b
genInstruction a "-" b  = sub' a b
genInstruction a "*" b  = mul' a b
genInstruction a "/" b  = div' a b
genInstruction a "%" b  = mod' a b
genInstruction a "==" b = eq' a b
genInstruction a "!=" b = neq' a b
genInstruction a ">" b  = gt' a b
genInstruction a "<" b  = lt' a b
genInstruction a ">=" b = gte' a b
genInstruction a "<=" b = lte' a b
genInstruction _ o _    = error ("Operation not implemented in genInstruction" ++ o)

-- Instruction

checkOperand :: AST -> Maybe Operand
checkOperand (Num n)   = Just (i32_as_op n)
checkOperand (Var v)   = Just (var' i32 v)
checkOperand _         = Nothing

extractJust :: Maybe a -> State (Int, [(Type, String)], [AST]) a
extractJust (Just a) = return a
extractJust Nothing  = error "Invalid instruction"

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing  = False

solveOperatorExtractValue :: BasicBlock -> Operand -> State (Int, [(Type, String)], [AST]) (BasicBlock, Operand)
solveOperatorExtractValue block (LocalReference (PointerType t o) (Name n)) = do
    resName     <- generateName "extract"
    let vtype   = PointerType t o
    block'      <- addInstructionWName resName (load' (var' vtype (byteStringToString n))) block
    return (block', var' t resName)

solveOperatorExtractValue block op = return (block, op)

solveOperator :: BasicBlock -> AST -> State (Int, [(Type, String)], [AST]) (BasicBlock, Maybe (Instruction, Type))
solveOperator block (Op a op b) | isJust (checkOperand a) && isJust (checkOperand b) = do
    case op of
        "=" -> do
            let vname = case a of
                    Var n   -> n
                    _       -> error "Invalid operand in solveOperator"
            vOperand        <- extractVarAsOperand vname
            case isPointer vOperand of
                True    -> do
                    (block', extractedOp)   <- extractOperandFromAST block b
                    (block'', rOperand)     <- solveOperatorExtractValue block' extractedOp
                    let finalInstr          = store' vOperand rOperand
                    let finalBlock          = addInstruction finalInstr block''
                    return (finalBlock, Just (finalInstr, i32))
                False   -> error "isPointer == False"
        _   -> do
            (block', a')        <- extractOperandFromAST block a
            (block'', b')       <- extractOperandFromAST block' b
            (newBlock, a'')     <- solveOperatorExtractValue block'' a'
            (newBlock', b'')    <- solveOperatorExtractValue newBlock b'
            let typea           = typeOf a''
            let typeb           = typeOf b''
            convertName         <- generateName "convert"
            let convertInstr    = convertTypeLLVM typeb typea b''
            newBlock''          <- addInstructionWName convertName convertInstr newBlock'
            let finalInstr      = genInstruction a'' op (var' typea convertName)
            return (newBlock'', Just (finalInstr, typea))

solveOperator block (Op a op b) | isJust (checkOperand a) = do
    case op of
        "=" -> do
            let vname = case a of
                    Var n   -> n
                    _       -> error "Invalid operand in solveOperator"
            vOperand        <- extractVarAsOperand vname
            case isPointer vOperand of
                True    -> do
                    -- opNameTest              <- generateName "opTest3"
                    (block', r)             <- solveInstr block b
                    (instrB, _)             <- extractJust r
                    rName                   <- generateName "rValue"
                    block''                 <- addInstructionWName rName instrB block'
                    let type'               = typeOf vOperand
                    let finalInstr          = store' vOperand (var' type' rName)
                    let finalBlock          = addInstruction finalInstr block''
                    return (finalBlock, Just (finalInstr, type'))
                False   -> error "isPointer == False"
        _   -> do
            (block', r)         <- solveInstr block b
            (instrB, _)         <- extractJust r
            nameB               <- generateName "opB"
            newBlock            <- addInstructionWName nameB instrB block'
            (newBlock', instrA) <- extractOperandFromAST newBlock a
            let type'           = typeOf instrA
            let finalInstr      = genInstruction instrA op (var' type' nameB)
            return (newBlock', Just (finalInstr, type'))

solveOperator block (Op a op b) | isJust (checkOperand b) = do
    (block', r)         <- solveInstr block a
    (instrA, typeA)     <- extractJust r
    nameA               <- generateName "opA"
    newBlock            <- addInstructionWName nameA instrA block'
    (newBlock', opb)    <- extractOperandFromAST newBlock b
    let type'           = typeOf opb
    convertName         <- generateName "convert"
    let convertInstr    = convertTypeLLVM type' typeA opb
    newBlock''          <- addInstructionWName convertName convertInstr newBlock'
    let finalInstr      = genInstruction (var' typeA nameA) op (var' typeA convertName)
    return (newBlock'', Just (finalInstr, type'))

solveOperator block (Op a op b) = do
    opNameTest          <- generateName "opTest1"
    (block', r1)        <- solveInstr block a
    (block'', r2)       <- solveInstr block' b
    (instrA, _)         <- extractJust r1
    (instrB, _)         <- extractJust r2
    nameA               <- generateName "opA"
    nameB               <- generateName "opB"
    newBlockA           <- addInstructionWName nameA instrA block''
    newBlockB           <- addInstructionWName nameB instrB newBlockA
    let finalInstr      = genInstruction (var' i32 nameA) op (var' i32 nameB)
    finalBlock          <- addInstructionWName opNameTest finalInstr newBlockB
    return (finalBlock, Just (finalInstr, i32))

solveOperator _ _ = error "Invalid operator"

solveInstr :: BasicBlock -> AST -> State (Int, [(Type, String)], [AST]) (BasicBlock, Maybe (Instruction, Type))
solveInstr block (Num n)                            = return (block, Just (i32_as_instr n, i32))

solveInstr block (Op a op b)                        = solveOperator block (Op a op b)

solveInstr block (FuncCall name' params')           = do
    (block', r) <- genFuncCallInstr block name' params'
    return (block', Just r)

solveInstr block (AST.Ret a)                        = do
    newBlock    <- genReturn block a
    return (newBlock, Nothing)

solveInstr block (DefineVar (Type t) name' value')  = do
    block'      <- genDefineVar block t name' value'
    return (block', Nothing)

solveInstr _ instr                                  = error ("Invalid instruction: " ++ show instr)

-- Blocks

convertBlocks :: Maybe BasicBlock -> BasicBlock -> [AST] -> State (Int, [(Type, String)], [AST]) [BasicBlock]
convertBlocks parent block []                                           = return [block]

convertBlocks parent block ((Condition cond condLines elseBlock):xs)    = case elseBlock of
    Otherwise []    -> genIfAloneBlock parent block ((Condition cond condLines elseBlock):xs)
    Otherwise _     -> genIfWithElse parent block ((Condition cond condLines elseBlock):xs)
    lines'          -> genIfWithElse parent block ((Condition cond condLines (Otherwise [lines'])):xs)

convertBlocks parent block ((WhileLoop cond lines'):xs)                 = genWhileLoop parent block ((WhileLoop cond lines'):xs)

convertBlocks parent block ((ForLoop start cond end lines'):xs)         = genForLoop parent block ((ForLoop start cond end lines'):xs)

convertBlocks parent block (x:xs)                                       = do
    (block', _) <- solveInstr block x
    convertBlocks parent block' xs

extractVar :: String -> State (Int, [(Type, String)], [AST]) (Type, String)
extractVar vname = do
    exists <- varExists vname
    case exists of
        True    -> do
            (vtype, vname') <- getVarFromContext vname
            return (vtype, vname')
        False   -> do
            return (i32, vname)

extractVarAsOperand :: String -> State (Int, [(Type, String)], [AST]) Operand
extractVarAsOperand vname   = do
    (vtype, vname') <- extractVar vname
    return (var' vtype vname')

extractOperandFromAST :: BasicBlock -> AST -> State (Int, [(Type, String)], [AST]) (BasicBlock, Operand)
extractOperandFromAST block (Num n) = return (block, i32_as_op n)
extractOperandFromAST block (Var v) = do
    op <- extractVarAsOperand v
    case isPointer op of
        True    -> do
            vname       <- generateName "ptr"
            newBlock    <- addInstructionWName vname (load' op) block
            let type'   = extractTypeFromPointer (typeOf op)
            return (newBlock, var' type' vname)
        False   -> return (block, op)
extractOperandFromAST _ _           = error "Invalid operand"

-- Loop

loopCond :: BasicBlock -> AST -> State (Int, [(Type, String)], [AST]) (BasicBlock, Maybe Instruction)
loopCond condBlock (Num n)  = do
    return (condBlock, Just (gt' (i32_as_op n) (i32_as_op 0)))
loopCond condBlock (Var v)  = do
    op <- extractVarAsOperand v
    return (condBlock, Just (gt' op (i32_as_op 0)))
loopCond condBlock ast      = do
    (condBlock', r) <- solveInstr condBlock ast
    (instr, _)      <- extractJust r
    return (condBlock', Just instr)

genWhileLoop :: Maybe BasicBlock -> BasicBlock -> [AST] -> State (Int, [(Type, String)], [AST]) [BasicBlock]
genWhileLoop parent block ((WhileLoop cond lines'):xs)  = do
    trueBlockName           <- generateName "whileTrue"
    falseBlockName          <- generateName "whileFalse"
    condBlockName           <- generateName "whileCond"

    trueBlock               <- genBlockWithName trueBlockName parent
    falseBlock              <- genBlockWithName falseBlockName parent
    condBlock               <- genBlockWithName condBlockName parent

    (condBlock', condInstr) <- loopCond condBlock cond
    condInstr'              <- extractJust condInstr

    othersBlock             <- convertBlocks parent falseBlock xs

    condName                <- generateName "whileCondInstr"
    condBlock''             <- addInstructionWName condName condInstr' condBlock'
    condBlock'''            <- addTerminator (jmpif (var' bool condName) trueBlock (othersBlock !! 0)) condBlock''

    trueBlock'              <- addTerminator (jmpToBlock condBlock''') trueBlock
    block'                  <- addTerminator (jmpToBlock condBlock''') block

    trueBlocks              <- convertBlocks (Just condBlock) trueBlock' lines'

    return ([block', condBlock'''] ++ trueBlocks ++ othersBlock)

genWhileLoop _ _ _                                    = error "Invalid while loop"

forLoopGenStartBlock :: BasicBlock -> AST -> State (Int, [(Type, String)], [AST]) BasicBlock
forLoopGenStartBlock block (DefineVar (Type t) name' value')  = do
    defVarBlock     <- genDefineVar block t name' value'
    return defVarBlock
forLoopGenStartBlock _ _                                    = error "Invalid for loop start"

genForLoop :: Maybe BasicBlock -> BasicBlock -> [AST] -> State (Int, [(Type, String)], [AST]) [BasicBlock]
genForLoop parent block ((ForLoop start cond end lines'):xs)    = do
    trueBlockName               <- generateName "forTrue"
    falseBlockName              <- generateName "forFalse"
    endBlockName                <- generateName "forEnd"
    startBlockName              <- generateName "forStart"
    condBlockName               <- generateName "forCond"

    trueBlock                   <- genBlockWithName trueBlockName parent
    falseBlock                  <- genBlockWithName falseBlockName parent
    endBlock                    <- genBlockWithName endBlockName parent
    startBlock                  <- genBlockWithName startBlockName parent
    condBlock                   <- genBlockWithName condBlockName parent

    startBlock'                 <- forLoopGenStartBlock startBlock start

    (endBlock', _)              <- solveInstr endBlock end

    (condBlock', r)             <- solveInstr condBlock cond
    (condInstr, _)              <- extractJust r
    -- condInstr'                  <- extractJust condInstr
    condName                    <- generateName "forCondInstr"
    condBlock''                 <- addInstructionWName condName condInstr condBlock'

    falseBlocks                 <- convertBlocks parent falseBlock xs

    block'                      <- addTerminator (jmpToBlock startBlock') block
    startBlock''                <- addTerminator (jmpToBlock condBlock'') startBlock'
    condBlock'''                <- addTerminator (jmpif (var' bool condName) trueBlock falseBlock) condBlock''
    trueBlock'                  <- addTerminator (jmpToBlock endBlock') trueBlock
    endBlock''                  <- addTerminator (jmpToBlock condBlock''') endBlock'

    trueBlocks                  <- convertBlocks (Just endBlock'') trueBlock' lines'

    return ([block', startBlock'', condBlock''', endBlock''] ++ trueBlocks ++ falseBlocks)

genForLoop _ _ _                                                  = error "Invalid for loop"

-- Prototype

createProtoParams :: [AST] -> State (Int, [(Type, String)], [AST]) ([Parameter], Bool)
createProtoParams p     = do
    p'      <- convertParameters p
    let p'' = params p'
    return p''

createPrototype :: AST -> String -> [AST] -> State (Int, [(Type, String)], [AST]) Definition
createPrototype (Type t) name params    = do
    _       <- addFuncInContext (Func (Type t) name params [])
    params' <- createProtoParams params
    let proto   = GlobalDefinition $ functionDefaults {
        name        = Name (stringToShortByteString name),
        returnType  = convertType t,
        parameters  = params'
    }
    return proto
createPrototype _ _ _                   = error "Invalid prototype"