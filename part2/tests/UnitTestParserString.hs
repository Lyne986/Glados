module UnitTestParserString where

import Test.HUnit
import Lexer.Data
import Lexer.String
import Parser.AST

getValFromEither :: Either String (String, String) -> String
getValFromEither (Right (a, _)) = a
getValFromEither _ = ""

stringGenericTest :: String -> String -> Test
stringGenericTest str target = TestCase (
    assertEqual "Basic string test" expected res)
    where
        expected = target
        res = getValFromEither (runParser (string' str) target)

stringTest1 :: Test
stringTest1 = stringGenericTest "1234" "1234"

stringTest2 :: Test
stringTest2 = stringGenericTest " 1234 " " 1234 "

stringTest3 :: Test
stringTest3 = stringGenericTest "hello" "hello"

stringTest4 :: Test
stringTest4 = stringGenericTest "hello " "hello "

stringTest5 :: Test
stringTest5 = stringGenericTest "WE tried A Lot of Thing" "WE tried A Lot of Thing"

stringTestList :: Test
stringTestList = TestList [
    TestLabel "string test 1" stringTest1,
    TestLabel "string test 2" stringTest2,
    TestLabel "string test 3" stringTest3,
    TestLabel "string test 4" stringTest4,
    TestLabel "string test 5" stringTest5
    ]
