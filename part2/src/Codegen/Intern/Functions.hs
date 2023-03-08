module Codegen.Intern.Functions (
    call',
    getFunc
) where

import LLVM.AST as AST
import LLVM.AST.Constant
import LLVM.AST.CallingConvention as CC
import Codegen.Intern.Types as T

getFunc :: Type -> String -> [Type] -> Operand
getFunc retType fname types = ConstantOperand $ GlobalReference (T.ptr $ FunctionType retType types False) (Name $ stringToShortByteString fname)

call' :: Operand -> [Operand] -> Instruction
call' f fargs = Call {
    AST.tailCallKind = Nothing,
    AST.callingConvention = CC.C,
    AST.returnAttributes = [],
    AST.function = Right f,
    AST.arguments = (map (\x -> (x, [])) fargs),
    AST.functionAttributes = [],
    AST.metadata = []
}
