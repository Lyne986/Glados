module UnitTestParserReturn where

import Test.HUnit
import Lexer.Data
import Lexer.Lexer
import Parser.AST

getNumberFromEither :: Either String (AST, String) -> Integer
getNumberFromEither (Right ((Ret (Num a)), _)) =  a
getNumberFromEither _ = 0

returnGenericTest :: String -> Integer -> Test
returnGenericTest str target = TestCase (
    assertEqual "Basic return test" expected res)
    where
        expected = target
        res = getNumberFromEither (runParser return' str)

returnTest1 :: Test
returnTest1 = returnGenericTest "return 1234;" 1234

returnTest2 :: Test
returnTest2 = returnGenericTest "return 1234 ;" 1234

returnTest3 :: Test
returnTest3 = returnGenericTest "   return 2; " 2

returnTestList :: Test
returnTestList = TestList [
    TestLabel "return test 1" returnTest1,
    TestLabel "return test 2" returnTest2,
    TestLabel "return test 3" returnTest3
    ]
