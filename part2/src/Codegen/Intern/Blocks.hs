{-# LANGUAGE OverloadedStrings #-}

module Codegen.Intern.Blocks (
    genEmptyBlock,
    genBlockWithName,
    addInstruction,
    addInstructionWName,
    addTerminator,
    getBlockName
) where

import LLVM.AST
import Control.Monad.State
import Data.ByteString.Short.Internal as BS

import Codegen.Intern.Operation
import Codegen.Intern.Types

import Lexer.Data

genBlockWithName :: String -> Maybe BasicBlock -> State (Int, [(Type, String)], [AST]) BasicBlock
genBlockWithName name Nothing                               = do
    return $ BasicBlock (Name (stringToShortByteString name)) [] (Do $ Unreachable [])
genBlockWithName name (Just (BasicBlock (Name bname) _ _))  = do
    return $ BasicBlock (Name (stringToShortByteString name)) [] (Do $ Br (Name bname) [])
genBlockWithName _ _                                        = error "Invalid block"

genEmptyBlock :: State (Int, [(Type, String)], [AST]) BasicBlock
genEmptyBlock = genBlockWithName "entry" Nothing

instrToNamedInstr :: Instruction -> Named Instruction
instrToNamedInstr instr = Do instr

addInstruction :: Instruction -> BasicBlock -> BasicBlock
addInstruction instr (BasicBlock name instrs term) = BasicBlock name (instrs ++ [newInstr]) term
    where
        newInstr = instrToNamedInstr instr

addInstructionWName :: String -> Instruction -> BasicBlock -> State (Int, [(Type, String)], [AST]) BasicBlock
addInstructionWName name instr (BasicBlock bname instrs term) = do
    let newInstr = Name (stringToShortByteString name) := instr
    return $ BasicBlock bname (instrs ++ [newInstr]) term

addTerminator :: Named Terminator -> BasicBlock -> State (Int, [(Type, String)], [AST]) BasicBlock
addTerminator term (BasicBlock name instrs _) = do
    return $ BasicBlock name instrs term

getBlockName :: BasicBlock -> BS.ShortByteString
getBlockName (BasicBlock (Name name) _ _) = name
getBlockName _                           = error "Invalid block"
