import Test.Tasty
import Test.Tasty.HUnit

import Booklet

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [testPageCalcs]

testPageCalcs :: TestTree
testPageCalcs = testGroup "page calcs unit tests"
    [ param1 "bookPageCount" bookPageCount $ zip [3..9] [4, 4, 8, 8, 8, 8, 12]

    , let
        four = [4, 1, 2, 3]
        eight = [8, 1, 2, 7, 6, 3, 4, 5]
        twelve = [12, 1, 2, 11, 10, 3, 4, 9, 8, 5, 6, 7]
      in param1 "pageOrder" pageOrder $ zip
        [1..12]
        [four, four, four, four, eight, eight, eight, eight, twelve, twelve, twelve, twelve]

    , param1 "pageStrFunc" (\n -> map (pageStrFunc n) [1..8])
      [ (5, ["A1", "A2", "A3", "A4", "A5", "B1", "B1", "B1"])
      , (6, ["A1", "A2", "A3", "A4", "A5", "A6", "B1", "B1"])
      , (7, ["A1", "A2", "A3", "A4", "A5", "A6", "A7", "B1"])
      , (8, ["A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8"])
      ]
    ]

-- parameterize tests of unary function
param1 :: (Show a, Show b, Eq b) => String -> (a -> b) -> [(a, b)] -> TestTree
param1 name f testParams =
    let
        tname a = name ++ " " ++ (show a)
        test (a, b) = testCase (tname a) $ f a @?= b
    in testGroup name $ map test testParams
