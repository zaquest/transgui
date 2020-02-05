import Test.HUnit
import qualified TestField


tests = test
  [
    TestField.tests
  ]


main :: IO ()
main = runTestTT tests >> pure ()
