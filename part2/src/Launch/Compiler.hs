module Launch.Compiler (
    launchCompiler
) where

import Lexer.Lexer
import Compiler.Eval
import Lexer.Data

showAST :: AST -> String
showAST (Num n)                                  = show "(Num) " ++ show n
showAST (Var name)                               = show "(Var) " ++ show name
showAST (Type t)                                 = show "(Type) " ++ show t
showAST (Lexer.Data.Ret n)                       = show "(Ret) " ++ showAST n
showAST (Param ptype name)                       = show "(Param) " ++ showAST ptype ++ show " (Name) " ++ show name
showAST (Op n1 s n2)                             = show "((Op) ((Num) " ++ showAST n1 ++ show " (Num) " ++ show s ++ show " (Num) " ++ showAST n2 ++ show ")"
showAST (Func ret_type fname params flines)      = show "((Func) " ++ show ret_type ++ show " (Name) " ++ show fname ++ show " (Params) " ++ showASTs params ++ show " ((Lines) " ++ show flines ++ show "))"
showAST (FuncCall fname params)                  = show "((FuncCall) (Name) " ++ show fname ++ show " (Params) " ++ showASTs params ++ show ")"
showAST (WhileLoop cond lines')                  = show "((Loop) (Cond) " ++ showAST cond ++ show " (Lines) " ++ showASTs lines' ++ show ")"
showAST (ForLoop init' cond incr flines)         = show "((ForLoop) (Init) " ++ showAST init' ++ show " (Cond) " ++ showAST cond ++ show " (Incr) " ++ showAST incr ++ show " (Lines) " ++ showASTs flines ++ show ")"
showAST (Condition cond linesTrue linesFalse)    = show "((Condition) (Cond) " ++ showAST cond ++ show " (LinesTrue) " ++ showASTs linesTrue ++ show " (LinesFalse) " ++ showAST linesFalse ++ show ")"
showAST (Otherwise flines)                       = show "((Otherwise) (Lines) " ++ showASTs flines ++ show ")"
showAST (DefineVar vtype name value)             = show "((DefineVar) (Type) " ++ showAST vtype ++ show " (Name) " ++ show name ++ show " (Value) " ++ showAST value ++ show ")"
showAST (Empty _)                                = show "(Empty)"
showAST (ProtoType ret_type fname params)        = show "((ProtoType) (Type) " ++ showAST ret_type ++ show " (Name) " ++ show fname ++ show " (Params) " ++ showASTs params ++ show ")"

showASTs :: [AST] -> String
showASTs []     = ""
showASTs (x:xs) = showAST x ++ "\n" ++ showASTs xs

launchFile :: String -> IO ()
launchFile fileName = do
    fileContent <- readFile fileName
    case parseFile fileContent of
        Left _              -> putStrLn "Error"
        Right (astFile, _)  -> do
            putStrLn $ showASTs astFile
            compilationEval astFile fileName
            return ()

launchCompiler :: [String] -> IO ()
launchCompiler (x:xs)   = do
        launchFile x
        launchCompiler xs
launchCompiler []       = return ()
