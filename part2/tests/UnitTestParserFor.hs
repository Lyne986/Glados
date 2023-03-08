module UnitTestParserFor where

import Test.HUnit
import Lexer.Data
import Lexer.Lexer
import Parser.AST

forGenericTest :: String -> Test
forGenericTest str = TestCase (
    assertEqual "Basic for test" True res)
    where
        res = getParsingLeft str == Just ""
        getParsingLeft str = case runParser forloop str of
            Left _ -> Nothing
            Right (_, b) -> Just b

forTest1 :: Test
forTest1 = forGenericTest "for (int i = 0; i < 10; i++) { 1; }"

forTest2 :: Test
forTest2 = forGenericTest "for (int i = 0; i < 10; i++) { 1; 2; }"

forTest3 :: Test
forTest3 = forGenericTest "for (int i = 0; i < 10; i++) { while (1) { 1; } }"

forTest4 :: Test
forTest4 = forGenericTest "for (int i = 0; i < 10; i++) { while (1) { 1; } if (1) { 1; } }"

forTest5 :: Test
forTest5 = forGenericTest "for (int i = 0; i < 10; i++) { while (1) { 1; } if (1) { 1; } else { 2; } }"

forTest6 :: Test
forTest6 = forGenericTest "for (int i = 0; i < 10; i++) { while (1) { 1; } if (1) { 1; } else { 2; } for (int i = 0; i < 10; i++) { 1; } }"

forTestList :: Test
forTestList = TestList [
    TestLabel "for test 1" forTest1,
    TestLabel "for test 2" forTest2,
    TestLabel "for test 3" forTest3,
    TestLabel "for test 4" forTest4,
    TestLabel "for test 5" forTest5,
    TestLabel "for test 6" forTest6
    ]
