module Codegen.Intern.Types (
    stringToShortByteString,
    i32_as_op,
    i32,
    i64,
    bool,
    ptr,
    i32_as_instr,
    isI32,
    byteStringToString,
    i64_as_op,
    i64_as_instr,
    i_as_op,
    fptrunc',
    fptosi',
    f32_as_op,
    f_as_op,
    isFloat
) where

import LLVM.AST
import LLVM.AST.Constant
import LLVM.AST.Float
import LLVM.AST.AddrSpace
import Codegen.Intern.Operation
import Data.Word


import qualified Data.ByteString.Short as BS
import Data.Char

stringToShortByteString :: String -> BS.ShortByteString
stringToShortByteString str = BS.pack $ map (fromIntegral . ord) str

i32_as_op :: Integer -> Operand
i32_as_op n = ConstantOperand (Int 32 n)

i64_as_op :: Integer -> Operand
i64_as_op n = ConstantOperand (Int 64 n)

i32 :: Type
i32 = IntegerType 32

f32 :: Type
f32 = FloatingPointType FloatFP

i64 :: Type
i64 = IntegerType 64

bool :: Type
bool = IntegerType 1

ptr :: Type -> Type
ptr t = PointerType t (AddrSpace 0)

i32_as_instr :: Integer -> Instruction
i32_as_instr n = add' (i32_as_op n) (i32_as_op 0)

i64_as_instr :: Integer -> Instruction
i64_as_instr n = add' (i64_as_op n) (i64_as_op 0)

isI32 :: Operand -> Bool
isI32 (ConstantOperand (Int 32 _)) = True
isI32 _ = False

byteStringToString :: BS.ShortByteString -> String
byteStringToString bs = map (chr . fromIntegral) (BS.unpack bs)

i_as_op :: Integer -> Type -> Operand
i_as_op n (IntegerType 32) = i32_as_op n
i_as_op n (IntegerType 64) = i64_as_op n
i_as_op _ _ = error "i_as_op: invalid type"

f_as_op :: Float -> Type -> Operand
f_as_op n (FloatingPointType FloatFP) = f32_as_op n
f_as_op _ _ = error "f_as_op: invalid type"

fptrunc' :: Operand -> Instruction
fptrunc' a = LLVM.AST.FPTrunc {
    LLVM.AST.operand0 = a,
    LLVM.AST.type' = FloatingPointType FloatFP,
    LLVM.AST.metadata = []
    }

fptosi' :: Operand -> Instruction
fptosi' a = LLVM.AST.FPToSI {
    LLVM.AST.operand0 = a,
    LLVM.AST.type' = IntegerType 64,
    LLVM.AST.metadata = []
    }

f32_as_op :: Float -> Operand
f32_as_op n = ConstantOperand (Float (Single n))

isFloat :: Operand -> Bool
isFloat (ConstantOperand (Float _)) = True
isFloat _ = False