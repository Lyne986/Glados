module UnitTestParserVariable where

import Test.HUnit
import Lexer.Data
import Lexer.Variable
import Parser.AST

getVarFromAST :: Either String (AST, String) -> String
getVarFromAST (Right (Var a, _)) = a
getVarFromAST _ = ""

variableGenericTest :: String -> String -> Test
variableGenericTest str target = TestCase (
    assertEqual "Basic variable test" expected res)
    where
        expected = target
        res = getVarFromAST (runParser variable str)

variableTest1 :: Test
variableTest1 = variableGenericTest "test" "test"

variableTest2 :: Test
variableTest2 = variableGenericTest "patate " "patate"

variableTest3 :: Test
variableTest3 = variableGenericTest " necropole   " "necropole"

variableTestList :: Test
variableTestList = TestList [
    TestLabel "variable test 1" variableTest1,
    TestLabel "variable test 2" variableTest2,
    TestLabel "variable test 3" variableTest3
    ]
