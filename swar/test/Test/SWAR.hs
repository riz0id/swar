module Test.SWAR (testTree) where

import Test.Core (TestTree, testGroup)
import Test.SWAR.Vec4x8 qualified 

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "swar"
    [ Test.SWAR.Vec4x8.testTree
    ]