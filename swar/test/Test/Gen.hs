module Test.Gen 
  ( vec4x8,
  ) 
where

import Data.SWAR.Vec4x8 (Vec4x8)
import Data.SWAR.Vec4x8 qualified as Vec4x8

import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

--------------------------------------------------------------------------------

vec4x8 :: Gen Vec4x8
vec4x8 =
  Vec4x8.pack
    <$> Gen.word8 Range.constantBounded
    <*> Gen.word8 Range.constantBounded
    <*> Gen.word8 Range.constantBounded
    <*> Gen.word8 Range.constantBounded