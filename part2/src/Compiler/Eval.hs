{-# LANGUAGE OverloadedStrings #-}

module Compiler.Eval (
    compilationEval
) where

import Lexer.Data
import Codegen.Functions

import LLVM.Module
import LLVM.Context
import LLVM.AST
import Control.Monad.State
import qualified LLVM.AST as AST

import Data.ByteString.Char8 as BS8
import System.Process

astToDefinition :: AST -> State (Int, [(Type, String)], [AST]) Definition
astToDefinition (Func (Type t) name params flines)  = do
    def <- createFunction (Type t) name params flines
    return def
astToDefinition (ProtoType (Type t) name params)    = do
    def <- createPrototype (Type t) name params
    return def
astToDefinition e                                   = error ("Invalid AST" ++ show e)

module' :: [Definition] -> AST.Module
module' defs = defaultModule
    {
        moduleName = "basic",
        moduleDefinitions = defs
    }


toLLVM :: [Definition] -> String ->  IO ()
toLLVM defs fileName = withContext $ \ctx -> do
    let outputName = Prelude.takeWhile (/= '.') fileName ++ ".ll"
    llvm <- withModuleFromAST ctx (module' defs) moduleLLVMAssembly
    BS8.putStrLn $ "Writing to " `BS8.append` BS8.pack outputName
    BS8.writeFile outputName llvm
    _  <- system $ "llc-9 -filetype=obj " ++ outputName
    BS8.putStrLn "Created object file"

evalASTs :: [AST] -> State (Int, [(Type, String)], [AST]) [Definition]
evalASTs []     = return []
evalASTs (x:xs) = do
    def <- astToDefinition x
    defs <- evalASTs xs
    return (def:defs)

compilationEval :: [AST] -> String-> IO ()
compilationEval asts fileName = do
    let defs = evalState (evalASTs asts) (0, [], [])
    toLLVM defs fileName