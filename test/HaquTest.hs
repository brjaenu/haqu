module Main (main) where
import Haqu.Web ( e, ea )
import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "e works" $
      e "p" "hallo" @?= "<p>hallo</p>",

    testCase "ea with single attribute works" $
      ea "p" [("style", "color:blue;")] "hallo" @?= "<p style='color:blue;'>hallo</p>",

    testCase "ea with multiple attribute works" $
      ea "p" [("style", "color:blue;"), ("id", "42")] "hallo" @?= "<p style='color:blue;' id='42'>hallo</p>"
  ]