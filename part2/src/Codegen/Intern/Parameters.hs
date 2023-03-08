{-# LANGUAGE OverloadedStrings #-}

module Codegen.Intern.Parameters (
    params,
    param
) where

import LLVM.AST

import Data.ByteString.Short.Internal as BS

param :: Type -> BS.ShortByteString -> Parameter
param t n = Parameter t (Name n) []

params :: [(Type, BS.ShortByteString)] -> ([Parameter], Bool)
params ps = (map (\(t, n) -> param t n) ps, False)

