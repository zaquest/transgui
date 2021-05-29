import Test.HUnit
import qualified TestField
import qualified TestColumn
import qualified TestTimeFormat


tests = test
  [
    TestField.tests,
    TestColumn.tests,
    TestTimeFormat.tests
  ]


main :: IO ()
main = runTestTT tests >> pure ()
