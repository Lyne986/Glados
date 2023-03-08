module UnitTestParserPrototype where

import Test.HUnit
import Lexer.Data
import Lexer.Lexer
import Parser.AST

prototypeGenericTest :: String -> Test
prototypeGenericTest str = TestCase (
    assertEqual "Basic prototype test" True res)
    where
        res = getParsingLeft str == Just ""
        getParsingLeft str = case runParser prototype str of
            Left _ -> Nothing
            Right (_, b) -> Just b

prototypeTest1 :: Test
prototypeTest1 = prototypeGenericTest "int a(int b, int c);"

prototypeTest2 :: Test
prototypeTest2 = prototypeGenericTest "long test(int b, int c, int d);"

prototypeTest3 :: Test
prototypeTest3 = prototypeGenericTest "int a(int b, int c, int d, int e);"

prototypeTest4 :: Test
prototypeTest4 = prototypeGenericTest "int a(int b, int c, int d, int e, int f);"

prototypeTestList :: Test
prototypeTestList = TestList [
    TestLabel "prototype test 1" prototypeTest1,
    TestLabel "prototype test 2" prototypeTest2,
    TestLabel "prototype test 3" prototypeTest3,
    TestLabel "prototype test 4" prototypeTest4
    ]
