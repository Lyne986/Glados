module UnitTestParserWhile where

import Test.HUnit
import Lexer.Data
import Lexer.Lexer
import Parser.AST

whileGenericTest :: String -> Test
whileGenericTest str = TestCase (
    assertEqual "Basic while test" True res)
    where
        res = getParsingLeft str == Just ""
        getParsingLeft str = case runParser whileloop str of
            Left _ -> Nothing
            Right (_, b) -> Just b

whileTest1 :: Test
whileTest1 = whileGenericTest "while (1) { 1; }"

whileTest2 :: Test
whileTest2 = whileGenericTest "while (1) { 1; 2; }"

whileTest3 :: Test
whileTest3 = whileGenericTest "while (1) { 1; 2; 3; }"

whileTest4 :: Test
whileTest4 = whileGenericTest "while (1) { 1; int a = 3; 3; 4; }"

whileTest5 :: Test
whileTest5 = whileGenericTest "while (1) { 1; int a = 3; 3; 4; while (1) { 1; 2; 3; } }"

whileTestList :: Test
whileTestList = TestList [
    TestLabel "while test 1" whileTest1,
    TestLabel "while test 2" whileTest2,
    TestLabel "while test 3" whileTest3,
    TestLabel "while test 4" whileTest4,
    TestLabel "while test 5" whileTest5
    ]
