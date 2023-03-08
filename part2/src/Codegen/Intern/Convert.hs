module Codegen.Intern.Convert (
    i32Toi64,
    convertTypeLLVM,
    extractTypeFromPointer
) where

import LLVM.AST
import LLVM.AST.Constant
import LLVM.AST.AddrSpace

import Codegen.Intern.Types
import Codegen.Intern.Cond
import Codegen.Intern.Operation

i32Toi64 :: Operand -> Instruction
i32Toi64 a = LLVM.AST.SExt {
    LLVM.AST.operand0 = a,
    LLVM.AST.type' = IntegerType 64,
    LLVM.AST.metadata = []
    }

trunc' :: Operand -> Instruction
trunc' a = LLVM.AST.Trunc {
    LLVM.AST.operand0 = a,
    LLVM.AST.type' = IntegerType 32,
    LLVM.AST.metadata = []
    }

fptoui' :: Operand -> Instruction
fptoui' a = LLVM.AST.FPToUI {
    LLVM.AST.operand0 = a,
    LLVM.AST.type' = IntegerType 32,
    LLVM.AST.metadata = []
    }

uitofp' :: Operand -> Instruction
uitofp' a = LLVM.AST.UIToFP {
    LLVM.AST.operand0 = a,
    LLVM.AST.type' = FloatingPointType FloatFP,
    LLVM.AST.metadata = []
    }

convertTypeLLVM :: Type -> Type -> Operand -> Instruction
convertTypeLLVM from to a = case from of
    IntegerType 32  -> case to of
        IntegerType 64  -> i32Toi64 a
        IntegerType 32  -> add' a (i32_as_op 0)
        IntegerType 1   -> eq' a (i32_as_op 0)
        FloatingPointType FloatFP    -> uitofp' a
        _ -> error "convertTypeLLVM: invalid conversion"
    IntegerType 64  -> case to of
        IntegerType 64  -> add' a (i64_as_op 0)
        IntegerType 32  -> trunc' a
        e               -> error ("convertTypeLLVM: invalid conversion" ++ show e)
    FloatingPointType FloatFP -> case to of
        FloatingPointType FloatFP    -> fadd' a (f32_as_op 0)
        IntegerType 32              -> fptoui' a
        IntegerType 64              -> fptosi' a
        e                           -> error ("convertTypeLLVM: invalid conversion" ++ show e)
    e               -> error ("convertTypeLLVM: invalid conversion" ++ show e)

extractTypeFromPointer :: Type -> Type
extractTypeFromPointer (PointerType t _)    = t
extractTypeFromPointer _                    = error "extractTypeFromPointer: invalid type"
