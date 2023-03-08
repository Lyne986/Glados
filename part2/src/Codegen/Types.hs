module Codegen.Types (
    convertType
) where

import LLVM.AST

convertType :: String -> Type
convertType "void"      = VoidType
convertType "int"       = IntegerType 32
convertType "long"      = IntegerType 64
convertType "float"     = FloatingPointType FloatFP
convertType "bool"      = IntegerType 1
convertType _           = error "Invalid type"
