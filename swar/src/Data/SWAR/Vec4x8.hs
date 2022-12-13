{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.SWAR.Vec4x8
-- Copyright   :  (c) Jacob Leach, 2022
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
--
-- @since 1.0.0
module Data.SWAR.Vec4x8
  ( Vec4x8 (Vec4x8#, getVec4x8#, ..),

    -- * Basic Operations
    broadcast,
    pack,
    unpack,

    -- * Index
    index,

    -- * Folds
    sum,
  )
where

import Data.Bits (Bits (..))
import Data.Kind (Type)
import Data.SWAR.Vec4x8.Prim qualified as Prim
import Data.Word (Word32, Word8)

import GHC.Exts (Int (..), Word32#)
import GHC.Exts qualified as GHC
import GHC.Word (Word32 (..), Word8 (..))

import Prelude hiding (and, sum)

import Text.Printf (PrintfArg, formatArg)

--------------------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
newtype Vec4x8 :: Type where
  Vec4x8 :: {getVec4x8 :: Word32} -> Vec4x8

-- | TODO: docs
--
-- @since 1.0.0
pattern Vec4x8# :: Word32# -> Vec4x8
pattern Vec4x8# {getVec4x8#} = Vec4x8 (W32# getVec4x8#)

{-# COMPLETE Vec4x8# #-}

-- | @since 1.0.0
instance Bits Vec4x8 where
  bit i = Vec4x8 (bit i)
  {-# INLINE bit #-}

  testBit (Vec4x8 x) = testBit x 
  {-# INLINE testBit #-}

  Vec4x8# x# .&. Vec4x8# y# = Vec4x8# (GHC.andWord32# x# y#)
  {-# INLINE (.&.) #-}

  Vec4x8# x# .|. Vec4x8# y# = Vec4x8# (GHC.orWord32# x# y#)
  {-# INLINE (.|.) #-}

  xor (Vec4x8# x#) (Vec4x8# y#) = Vec4x8# (GHC.xorWord32# x# y#)
  {-# INLINE xor #-}

  complement (Vec4x8# x#) = Vec4x8# (GHC.notWord32# x#)
  {-# INLINE complement #-}

  shiftL (Vec4x8# x#) (I# n#) = Vec4x8# (Prim.uncheckedShiftL# x# n#)
  {-# INLINE shiftL #-}

  shiftR (Vec4x8# x#) (I# n#) = Vec4x8# (Prim.uncheckedShiftR# x# n#)
  {-# INLINE shiftR #-}

  popCount (Vec4x8 x) = popCount x 
  {-# INLINE popCount #-}

  bitSizeMaybe _ = Just 32
  {-# INLINE CONLIKE bitSizeMaybe #-}

  bitSize _ = 32
  {-# INLINE CONLIKE bitSize #-}

  isSigned _ = False
  {-# INLINE CONLIKE isSigned #-}

-- | @since 1.0.0
instance Eq Vec4x8 where
  Vec4x8# x# == Vec4x8# y# =
    case GHC.eqWord32# x# y# of
      1# -> True
      _  -> False
  {-# INLINE (==) #-}

  Vec4x8# x# /= Vec4x8# y# =
    case GHC.neWord32# x# y# of
      1# -> True
      _  -> False
  {-# INLINE (/=) #-}

-- | @since 1.0.0
instance PrintfArg Vec4x8 where
  formatArg (Vec4x8 u32) = formatArg u32
  {-# INLINE formatArg #-}

-- | @since 1.0.0
instance Show Vec4x8 where
  show x = show (unpack x)
  {-# INLINE show #-}

-- Vec4x8 - Basic Operations ---------------------------------------------------

-- | \(\mathcal{O}(1)\). TODO: docs
--
-- @since 1.0.0
broadcast :: Word8 -> Vec4x8
broadcast (W8# x#) = Vec4x8# (Prim.broadcast# x#)
{-# INLINE broadcast #-}

-- | \(\mathcal{O}(1)\). TODO: docs
--
-- @since 1.0.0
pack :: Word8 -> Word8 -> Word8 -> Word8 -> Vec4x8
pack (W8# x#) (W8# y#) (W8# z#) (W8# w#) = Vec4x8# (Prim.pack# x# y# z# w#)
{-# INLINE pack #-}

-- | \(\mathcal{O}(1)\). TODO: docs
--
-- @since 1.0.0
unpack :: Vec4x8 -> (Word8, Word8, Word8, Word8)
unpack (Vec4x8# u32#) = case Prim.unpack# u32# of
  (# x#, y#, z#, w# #) -> (W8# x#, W8# y#, W8# z#, W8# w#)
{-# INLINE unpack #-}

-- Vec4x8 - Index --------------------------------------------------------------

-- | \(\mathcal{O}(1)\). TODO: docs
--
-- @since 1.0.0
index :: Vec4x8 -> Int -> Word8
index (Vec4x8# u32#) (I# i#) = W8# (Prim.index# u32# i#)
{-# INLINE index #-}

-- Vec4x8 - Write --------------------------------------------------------------


-- Vec4x8 - Fold ---------------------------------------------------------------

-- | \(\mathcal{O}(1)\). TODO: docs
--
-- @since 1.0.0
sum :: Vec4x8 -> Word32
sum (Vec4x8# u32#) = W32# (Prim.sum# u32#)
{-# INLINE sum #-}
