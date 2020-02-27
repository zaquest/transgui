import Test.HUnit
import qualified TestField
import qualified TestColumn


tests = test
  [
    TestField.tests,
    TestColumn.tests
  ]


main :: IO ()
main = runTestTT tests >> pure ()
