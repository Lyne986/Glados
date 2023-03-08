module Codegen.Intern.Cond (
    jmpToBlock,
    jmpif,
    jmpIfTrue,
    eq',
    neq',
    gt',
    lt',
    gte',
    lte'
) where

import LLVM.AST
import LLVM.AST.IntegerPredicate as IP
import Data.ByteString.Short.Internal as BS

import Codegen.Intern.Blocks

br :: BS.ShortByteString -> Terminator
br name = Br (Name (name)) []

jmpToBlock :: BasicBlock -> Named Terminator
jmpToBlock block = Do $ br (getBlockName block)

eq' :: Operand -> Operand -> Instruction
eq' a b = ICmp IP.EQ a b []

neq' :: Operand -> Operand -> Instruction
neq' a b = ICmp IP.NE a b []

gt' :: Operand -> Operand -> Instruction
gt' a b = ICmp IP.SGT a b []

lt' :: Operand -> Operand -> Instruction
lt' a b = ICmp IP.SLT a b []

gte' :: Operand -> Operand -> Instruction
gte' a b = ICmp IP.SGE a b []

lte' :: Operand -> Operand -> Instruction
lte' a b = ICmp IP.SLE a b []

jmpif :: Operand -> BasicBlock -> BasicBlock -> Named Terminator
jmpif cond bTrue bFalse = Do $ CondBr cond (Name (getBlockName bTrue)) (Name (getBlockName bFalse)) []

jmpIfTrue :: Operand -> BasicBlock -> BasicBlock -> Named Terminator
jmpIfTrue cond bTrue bFalse = Do $ CondBr cond (Name (getBlockName bTrue)) (Name (getBlockName bFalse)) []