module Test.SWAR.Vec4x8 (testTree) where

import Control.Exception (evaluate)

import Data.SWAR.Vec4x8 qualified as Vec4x8
import Data.Bits (Bits (..))

import Hedgehog (annotate, evalIO, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Test.Core (TestTree, forAll, testCases, testGroup)

import Text.Printf (printf)
import qualified Test.Gen as Gen

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "vec4x8"
    [ testGroup
        "broadcast"
        [ testCases "unpack" 1_000 do 
            x <- forAll (Gen.word8 Range.constantBounded)
            vec <- evalIO (evaluate (Vec4x8.broadcast x))
            annotate ("broadcast x = " ++ printf "0x%x" vec)

            Vec4x8.unpack vec === (x, x, x, x)
        , testCases "index" 1_000 do 
            x <- forAll (Gen.word8 Range.constantBounded)
            vec <- evalIO (evaluate (Vec4x8.broadcast x))
            annotate ("broadcast x = " ++ printf "0x%x" vec)

            Vec4x8.index vec 0 === x
            Vec4x8.index vec 1 === x
            Vec4x8.index vec 2 === x
            Vec4x8.index vec 3 === x
        ]
    , testGroup
        "pack"
        [ testCases "unpack" 1_000 do
            x <- forAll (Gen.word8 Range.constantBounded)
            y <- forAll (Gen.word8 Range.constantBounded)
            z <- forAll (Gen.word8 Range.constantBounded)
            w <- forAll (Gen.word8 Range.constantBounded)
            vec <- evalIO (evaluate (Vec4x8.pack x y z w))
            annotate ("pack x y z w = " ++ printf "0x%x" vec)

            Vec4x8.unpack vec === (x, y, z, w)
        , testCases "index" 1_000 do 
            x <- forAll (Gen.word8 Range.constantBounded)
            y <- forAll (Gen.word8 Range.constantBounded)
            z <- forAll (Gen.word8 Range.constantBounded)
            w <- forAll (Gen.word8 Range.constantBounded)
            vec <- evalIO (evaluate (Vec4x8.pack x y z w))
            annotate ("pack x y z w = " ++ printf "0x%x" vec)

            Vec4x8.index vec 0 === x
            Vec4x8.index vec 1 === y
            Vec4x8.index vec 2 === z
            Vec4x8.index vec 3 === w
        ]
    , testGroup 
        "bits"
        [ testCases "and" 1_000 do 
            vecX <- forAll Gen.vec4x8
            vecY <- forAll Gen.vec4x8
            vec  <- evalIO (evaluate (vecX .&. vecY))
            annotate ("vecX .&. vecY = " ++ printf "0x%x" vec)

            Vec4x8.index vec 0 === (Vec4x8.index vecX 0 .&. Vec4x8.index vecY 0)
            Vec4x8.index vec 1 === (Vec4x8.index vecX 1 .&. Vec4x8.index vecY 1)
            Vec4x8.index vec 2 === (Vec4x8.index vecX 2 .&. Vec4x8.index vecY 2)
            Vec4x8.index vec 3 === (Vec4x8.index vecX 3 .&. Vec4x8.index vecY 3)
        , testCases "or" 1_000 do 
            vecX <- forAll Gen.vec4x8
            vecY <- forAll Gen.vec4x8
            vec  <- evalIO (evaluate (vecX .|. vecY))
            annotate ("vecX .|. vecY = " ++ printf "0x%x" vec)

            Vec4x8.index vec 0 === (Vec4x8.index vecX 0 .|. Vec4x8.index vecY 0)
            Vec4x8.index vec 1 === (Vec4x8.index vecX 1 .|. Vec4x8.index vecY 1)
            Vec4x8.index vec 2 === (Vec4x8.index vecX 2 .|. Vec4x8.index vecY 2)
            Vec4x8.index vec 3 === (Vec4x8.index vecX 3 .|. Vec4x8.index vecY 3)
        , testCases "xor" 1_000 do 
            vecX <- forAll Gen.vec4x8
            vecY <- forAll Gen.vec4x8
            vec  <- evalIO (evaluate (xor vecX vecY))
            annotate ("xor vecX vecY = " ++ printf "0x%x" vec)

            Vec4x8.index vec 0 === xor (Vec4x8.index vecX 0) (Vec4x8.index vecY 0)
            Vec4x8.index vec 1 === xor (Vec4x8.index vecX 1) (Vec4x8.index vecY 1)
            Vec4x8.index vec 2 === xor (Vec4x8.index vecX 2) (Vec4x8.index vecY 2)
            Vec4x8.index vec 3 === xor (Vec4x8.index vecX 3) (Vec4x8.index vecY 3)
        , testCases "complement" 1_000 do 
            vec0 <- forAll Gen.vec4x8
            vec1 <- evalIO (evaluate (complement vec0))
            annotate ("complement vec0 = " ++ printf "0x%x" vec1)

            Vec4x8.index vec1 0 === complement (Vec4x8.index vec0 0) 
            Vec4x8.index vec1 1 === complement (Vec4x8.index vec0 1) 
            Vec4x8.index vec1 2 === complement (Vec4x8.index vec0 2) 
            Vec4x8.index vec1 3 === complement (Vec4x8.index vec0 3) 
        , testCases "shiftL" 100 do 
            bits <- forAll (Gen.int $ Range.constant 0 8)
            vec0 <- forAll Gen.vec4x8
            vec1 <- evalIO (evaluate (shiftL vec0 bits))
            annotate ("shiftL vec0 bits = " ++ printf "0x%x" vec1)

            Vec4x8.index vec1 0 === shiftL (Vec4x8.index vec0 0) bits
            Vec4x8.index vec1 1 === shiftL (Vec4x8.index vec0 1) bits
            Vec4x8.index vec1 2 === shiftL (Vec4x8.index vec0 2) bits
            Vec4x8.index vec1 3 === shiftL (Vec4x8.index vec0 3) bits
        , testCases "shiftR" 100 do 
            bits <- forAll (Gen.int $ Range.constant 0 8)
            vec0 <- forAll Gen.vec4x8
            vec1 <- evalIO (evaluate (shiftR vec0 bits))
            annotate ("shiftR vec0 bits = " ++ printf "0x%x" vec1)

            Vec4x8.index vec1 0 === shiftR (Vec4x8.index vec0 0) bits
            Vec4x8.index vec1 1 === shiftR (Vec4x8.index vec0 1) bits
            Vec4x8.index vec1 2 === shiftR (Vec4x8.index vec0 2) bits
            Vec4x8.index vec1 3 === shiftR (Vec4x8.index vec0 3) bits
        ]
    , testGroup
        "fold"
        [ testCases "sum" 1_000 do 
            x <- forAll (Gen.word8 Range.constantBounded)
            y <- forAll (Gen.word8 Range.constantBounded)
            z <- forAll (Gen.word8 Range.constantBounded)
            w <- forAll (Gen.word8 Range.constantBounded)
            vec <- evalIO (evaluate (Vec4x8.pack x y z w))
            annotate ("pack x y z w = " ++ printf "0x%x" vec)

            Vec4x8.sum vec === foldr ((+) . fromIntegral) 0 [x, y, z, w]
        ]
    ]
