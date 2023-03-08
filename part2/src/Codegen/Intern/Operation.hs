module Codegen.Intern.Operation (
    add',
    sub',
    mul',
    div',
    mod',
    ret',
    retVoid',
    fadd',
    fsub',
    fmul',
    fdiv',
    frem'
) where

import LLVM.AST
import LLVM.AST.Constant as C


isFloat :: Operand -> Bool
isFloat (LocalReference (FloatingPointType _) _) = True
isFloat (ConstantOperand (C.Float _)) = True
isFloat _ = False

-- nsw = no signed overflow
-- nuw = no unsigned overflow
add' :: Operand -> Operand -> Instruction
-- add' a b = error (show a ++ " " ++ show b)
add' a b | isFloat a || isFloat b = fadd' a b
         | otherwise = LLVM.AST.Add {
    LLVM.AST.nsw = False,
    LLVM.AST.nuw = False,
    LLVM.AST.operand0 = a,
    LLVM.AST.operand1 = b,
    metadata = []
    }

sub' :: Operand -> Operand -> Instruction
sub' a b | isFloat a || isFloat b = fsub' a b
         | otherwise = LLVM.AST.Sub {
    LLVM.AST.nsw = False,
    LLVM.AST.nuw = False,
    LLVM.AST.operand0 = a,
    LLVM.AST.operand1 = b,
    metadata = []
    }

mul' :: Operand -> Operand -> Instruction
mul' a b | isFloat a || isFloat b = fmul' a b
         | otherwise = LLVM.AST.Mul {
    LLVM.AST.nsw = False,
    LLVM.AST.nuw = False,
    LLVM.AST.operand0 = a,
    LLVM.AST.operand1 = b,
    metadata = []
    }

div' :: Operand -> Operand -> Instruction
div' a b | isFloat a || isFloat b = fdiv' a b
         | otherwise = LLVM.AST.SDiv {
    LLVM.AST.exact = False,
    LLVM.AST.operand0 = a,
    LLVM.AST.operand1 = b,
    metadata = []
    }

mod' :: Operand -> Operand -> Instruction
mod' a b | isFloat a || isFloat b = error "mod' not implemented for floats"
         | otherwise = LLVM.AST.SRem {
    LLVM.AST.operand0 = a,
    LLVM.AST.operand1 = b,
    metadata = []
    }

ret' :: Operand -> Named Terminator
ret' a = Do $ Ret {
    returnOperand = Just a,
    metadata' = []
    }

retVoid' :: Named Terminator
retVoid' = Do $ Ret {
    returnOperand = Nothing,
    metadata' = []
    }

fadd' :: Operand -> Operand -> Instruction
fadd' a b = LLVM.AST.FAdd {
    fastMathFlags = LLVM.AST.noFastMathFlags,
    LLVM.AST.operand0 = a,
    LLVM.AST.operand1 = b,
    metadata = []
    }

fsub' :: Operand -> Operand -> Instruction
fsub' a b = LLVM.AST.FSub {
    fastMathFlags = LLVM.AST.noFastMathFlags,
    LLVM.AST.operand0 = a,
    LLVM.AST.operand1 = b,
    metadata = []
    }

fmul' :: Operand -> Operand -> Instruction
fmul' a b = LLVM.AST.FMul {
    fastMathFlags = LLVM.AST.noFastMathFlags,
    LLVM.AST.operand0 = a,
    LLVM.AST.operand1 = b,
    metadata = []
    }

fdiv' :: Operand -> Operand -> Instruction
fdiv' a b = LLVM.AST.FDiv {
    fastMathFlags = LLVM.AST.noFastMathFlags,
    LLVM.AST.operand0 = a,
    LLVM.AST.operand1 = b,
    metadata = []
    }

frem' :: Operand -> Operand -> Instruction
frem' a b = LLVM.AST.FRem {
    fastMathFlags = LLVM.AST.noFastMathFlags,
    LLVM.AST.operand0 = a,
    LLVM.AST.operand1 = b,
    metadata = []
    }

