module Main (main) where

import Test.Core (TestTree, testGroup)
import Test.SWAR qualified
import Test.Tasty (defaultMain)

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain testTree

testTree :: TestTree
testTree =
  testGroup
    "test"
    [ Test.SWAR.testTree
    ]
