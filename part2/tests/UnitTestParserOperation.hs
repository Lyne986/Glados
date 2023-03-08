module UnitTestParserOperation where

import Test.HUnit
import Lexer.Data
import Lexer.Lexer
import Parser.AST

operationGenericTest :: String -> Test
operationGenericTest str = TestCase (
    assertEqual "Basic operation test" True res)
    where
        res = getParsingLeft str == Just ""
        getParsingLeft str = case runParser operation str of
            Left _ -> Nothing
            Right (_, b) -> Just b

operationTest1 :: Test
operationTest1 = operationGenericTest "1 + 2"

operationTest2 :: Test
operationTest2 = operationGenericTest "1 + 2 + 3"

operationTest3 :: Test
operationTest3 = operationGenericTest "1 + 2 - 3 * 4"

operationTest4 :: Test
operationTest4 = operationGenericTest "1 + 2 - 3 * 4 / 5"

operationTest5 :: Test
operationTest5 = operationGenericTest "1 + 2 - 3 * 4 / 5 % 6"

operationTest6 :: Test
operationTest6 = operationGenericTest "(1 + (2 - 3) * 4 / 5 % 6 + 7)"

operationTest7 :: Test
operationTest7 = operationGenericTest "1 + 2 - 3 * 4 / 5 % 6 + 7"

operationTestList :: Test
operationTestList = TestList [
    TestLabel "operation test 1" operationTest1,
    TestLabel "operation test 2" operationTest2,
    TestLabel "operation test 3" operationTest3,
    TestLabel "operation test 4" operationTest4,
    TestLabel "operation test 5" operationTest5,
    TestLabel "operation test 6" operationTest6,
    TestLabel "operation test 7" operationTest7
    ]
