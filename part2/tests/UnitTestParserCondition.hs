module UnitTestParserCondition where

import Test.HUnit
import Lexer.Data
import Lexer.Lexer
import Parser.AST

conditionGenericTest :: String -> Test
conditionGenericTest str = TestCase (
    assertEqual "Basic condition test" True res)
    where
        res = getParsingLeft str == Just ""
        getParsingLeft str = case runParser condition str of
            Left _ -> Nothing
            Right (_, b) -> Just b

conditionTest1 :: Test
conditionTest1 = conditionGenericTest "if (1) { 1; }"

conditionTest2 :: Test
conditionTest2 = conditionGenericTest "if (1) { int a = b; a += 1; } else { 5;}"

conditionTest3 :: Test
conditionTest3 = conditionGenericTest "if (1) { int a = b; a += 1; } else { 5; if (1) { 1; } }"

conditionTest4 :: Test
conditionTest4 = conditionGenericTest "if (1) { int a = b; a += 1; } else { 5; if (1) { 1; } else { 2; } }"

conditionTestList :: Test
conditionTestList = TestList [
    TestLabel "condition test 1" conditionTest1,
    TestLabel "condition test 2" conditionTest2,
    TestLabel "condition test 3" conditionTest3,
    TestLabel "condition test 4" conditionTest4
    ]
