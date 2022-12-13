{-# LANGUAGE CPP #-}

module Test.Core
  ( Property,
    PropertyT,
    TestTree,
    forAll,
    property,
    testCases,
    testGroup,
    testProp,
  )
where

import Control.Exception (assert)

import Data.String (fromString)

import Hedgehog (Property, PropertyT, TestLimit, forAll, property, withTests)

import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.Hedgehog qualified as Tasty

--------------------------------------------------------------------------------

testCases :: TestName -> TestLimit -> PropertyT IO () -> TestTree
testCases name n = assert (n > 0) . testProp name . withTests n . property

#if MIN_VERSION_tasty_hedgehog(1,2,0)

testProp :: TestName -> Property -> TestTree
testProp name = Tasty.testPropertyNamed name (fromString name)

#else

testProp :: TestName -> Property -> TestTree
testProp = Tasty.testProperty

#endif
