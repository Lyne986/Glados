module UnitTestParserNumber where

import Test.HUnit

import Lexer.Data
import Lexer.Number
import Parser.AST

getNbFromAST :: Either String (AST, String) -> Integer
getNbFromAST (Right (Num a, _)) = a
getNbFromAST _ = 0

numberGenericTest :: String -> Integer -> Test
numberGenericTest str target = TestCase (
    assertEqual "Basic number test" expected res)
    where
        expected = target
        res = getNbFromAST (runParser number str)

numberTest1 :: Test
numberTest1 = numberGenericTest "1234" 1234

numberTest2 :: Test
numberTest2 = numberGenericTest "1234 " 1234

numberTest3 :: Test
numberTest3 = numberGenericTest " 1234" 1234

numberTest4 :: Test
numberTest4 = numberGenericTest " 1234 " 1234

numberTest5 :: Test
numberTest5 = numberGenericTest "-1234 " (-1234)

numberTest6 :: Test
numberTest6 = numberGenericTest "+1234 " 1234

numberTest7 :: Test
numberTest7 = numberGenericTest " +1234 " 1234

numberTest8 :: Test
numberTest8 = numberGenericTest " -1234 " (-1234)

numberTest9 :: Test
numberTest9 = numberGenericTest " -1234" (-1234)

numberTestList :: Test
numberTestList = TestList [
    TestLabel "number test 1" numberTest1,
    TestLabel "number test 2" numberTest2,
    TestLabel "number test 3" numberTest3,
    TestLabel "number test 4" numberTest4,
    TestLabel "number test 5" numberTest5,
    TestLabel "number test 6" numberTest6,
    TestLabel "number test 7" numberTest7,
    TestLabel "number test 8" numberTest8,
    TestLabel "number test 9" numberTest9
    ]
