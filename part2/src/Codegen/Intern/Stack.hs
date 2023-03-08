{-# LANGUAGE OverloadedStrings #-}

module Codegen.Intern.Stack (
    var',
    generateName,
    alloca',
    store',
    load',
    isPointer,
    typeOf,
    addVarInContext,
    varExists,
    getVarFromContext,
    getVarName,
    addFuncInContext,
    getFuncFromContext,
    funcExists,
    getFuncName,
    getCurrentFunc
) where

import LLVM.AST
import LLVM.AST.Constant
import Control.Monad.State

import Codegen.Intern.Types

import Lexer.Data

var' :: Type -> String -> Operand
var' t name = LocalReference t (Name (stringToShortByteString name))

generateName :: String -> State (Int, [(Type, String)], [AST]) String
generateName name = do
    (i, context, f) <- get
    put (i + 1, context, f)
    return (name ++ show i)

alloca' :: Type -> Instruction
alloca' t = Alloca t Nothing 0 []

store' :: Operand -> Operand -> Instruction
store' ptr' val = Store False ptr' val Nothing 0 []

load' :: Operand -> Instruction
load' ptr' = Load False ptr' Nothing 0 []

typeOf :: Operand -> Type
typeOf (LocalReference t _) = t
typeOf (ConstantOperand (Int 32 _)) = IntegerType 32
typeOf (ConstantOperand (Int 64 _)) = IntegerType 64
typeOf e                    = error ("Not a local reference" ++ show e)

isPointer :: Operand -> Bool
isPointer (LocalReference (PointerType _ _) _)  = True
isPointer _                                     = False

addVarInContext :: Type -> String -> State (Int, [(Type, String)], [AST]) ()
addVarInContext t name = do
    (i, context, f) <- get
    put (i, (t, name):context, f)

varExists :: String -> State (Int, [(Type, String)], [AST]) Bool
varExists name = do
    (_, context, _) <- get
    return (not (null (filter (\(_, n) -> n == name) context)))

getVarFromContext :: String -> State (Int, [(Type, String)], [AST]) (Type, String)
getVarFromContext name = do
    (_, context, _) <- get
    return (head (filter (\(_, n) -> n == name) context))

getVarName :: Operand -> String
getVarName (LocalReference _ (Name n)) = byteStringToString n
getVarName _                           = error "Not a local reference"

addFuncInContext :: AST -> State (Int, [(Type, String)], [AST]) ()
addFuncInContext f = do
    (i, context, fs) <- get
    put (i, context, f:fs)

getFuncName :: AST -> String
getFuncName (Func _ n _ _)  = n
getFuncName _               = error "Not a function"

findFunc :: String -> [AST] -> Maybe AST
findFunc name (f:fs) = if (name == (getFuncName f)) then Just f else findFunc name fs
findFunc _ []        = Nothing

getFuncFromContext :: String -> State (Int, [(Type, String)], [AST]) AST
getFuncFromContext name = do
    (_, _, fs)  <- get
    case findFunc name fs of
        Just f  -> return f
        Nothing -> error ("Function " ++ name ++ " not found")

funcExists :: String -> State (Int, [(Type, String)], [AST]) Bool
funcExists name = do
    (_, _, fs)  <- get
    case findFunc name fs of
        Just f  -> return True
        Nothing -> return False

getCurrentFunc :: State (Int, [(Type, String)], [AST]) AST
getCurrentFunc = do
    (_, _, f)   <- get
    let (a:_)   = f
    return a
