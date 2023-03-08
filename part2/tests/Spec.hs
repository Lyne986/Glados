import System.Exit
import Test.HUnit

import UnitTestParserNumber
import UnitTestParserString
import UnitTestParserVariable
import UnitTestParserReturn
import UnitTestParserWhile
import UnitTestParserCondition
import UnitTestParserPrototype
import UnitTestParserFor
import UnitTestParserOperation


runAllTests :: IO Counts
runAllTests = runTestTT $ TestList [
    TestLabel "operation" operationTestList,
    TestLabel "for" forTestList,
    TestLabel "prototype" prototypeTestList,
    TestLabel "condition" conditionTestList,
    TestLabel "while" whileTestList,
    TestLabel "return" returnTestList,
    TestLabel "string" stringTestList,
    TestLabel "variable" variableTestList,
    TestLabel "number" numberTestList]


main :: IO Counts
main = do
    counts <- runAllTests
    if (errors counts + failures counts) > 0
        then exitFailure
        else exitSuccess
