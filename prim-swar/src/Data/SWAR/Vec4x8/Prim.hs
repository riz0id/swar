{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.SWAR.Vec4x8.Prim
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
module Data.SWAR.Vec4x8.Prim
  ( -- * Basic Operations
    broadcast#,
    pack#,
    unpack#,

    -- * Index
    index#,

    -- * Write

    -- * Bitwise Operations
    uncheckedShiftL#,
    uncheckedShiftR#,

    -- * Fold
    sum#,
  )
where

import GHC.Exts (Int#, Word32#, Word8#)
import GHC.Exts qualified as GHC

--------------------------------------------------------------------------------

infixl 8 >>!#, <<!#
infixl 7 &&#
infixl 5 ||#

(>>!#) :: Word32# -> Int# -> Word32#
(>>!#) = GHC.uncheckedShiftRLWord32#

(<<!#) :: Int# -> Word32# -> Word32#
(<<!#) i# u32# = GHC.uncheckedShiftLWord32# u32# i#

(&&#) :: Word32# -> Word32# -> Word32#
(&&#) = GHC.andWord32#

(||#) :: Word32# -> Word32# -> Word32#
(||#) = GHC.orWord32#

-- | TODO: docs
--
-- @since 1.0.0
word8ToWord32# :: Word8# -> Word32#
word8ToWord32# x# = GHC.wordToWord32# (GHC.word8ToWord# x#)

-- | TODO: docs
--
-- @since 1.0.0
word32ToWord8# :: Word32# -> Word8#
word32ToWord8# x# = GHC.wordToWord8# (GHC.word32ToWord# x#)

-- Vec4x8# - Basic Operations --------------------------------------------------

-- | \(\mathcal{O}(1)\). TODO: docs
--
-- @since 1.0.0
broadcast# :: Word8# -> Word32#
broadcast# x# = pack# x# x# x# x#

-- | TODO: docs
--
-- @since 1.0.0
pack# :: Word8# -> Word8# -> Word8# -> Word8# -> Word32#
pack# x# y# z# w# =
  let !leftHalf#  = (8# <<!# word8ToWord32# x#) ||# word8ToWord32# y#
      !rightHalf# = (8# <<!# word8ToWord32# z#) ||# word8ToWord32# w#
   in (16# <<!# leftHalf#) ||# rightHalf#

-- | TODO: docs
--
-- @since 1.0.0
unpack# :: Word32# -> (# Word8#, Word8#, Word8#, Word8# #)
unpack# u32# =
  let !x# = index# u32# 0#
      !y# = index# u32# 1#
      !z# = index# u32# 2#
      !w# = index# u32# 3#
   in (# x#, y#, z#, w# #)

-- Index -----------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
index# :: Word32# -> Int# -> Word8#
index# u32# 0# = word32ToWord8# (u32# >>!# 24#)
index# u32# 1# = word32ToWord8# (u32# >>!# 16#)
index# u32# 2# = word32ToWord8# (u32# >>!# 8#)
index# u32# 3# = word32ToWord8# (u32# >>!# 0#)
index# _    _  = errorWithoutStackTrace (shows 'index# ": index out of bounds")

-- Vec4x8# - Write -------------------------------------------------------------

-- Bitwise Operations ----------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
uncheckedShiftL# :: Word32# -> Int# -> Word32#
uncheckedShiftL# u32# = \case
  0# -> u32#
  1# -> GHC.wordToWord32# 0xfefefefe## &&# (1# <<!# u32#)
  2# -> GHC.wordToWord32# 0xfcfcfcfc## &&# (2# <<!# u32#)
  3# -> GHC.wordToWord32# 0xf8f8f8f8## &&# (3# <<!# u32#)
  4# -> GHC.wordToWord32# 0xf0f0f0f0## &&# (4# <<!# u32#)
  5# -> GHC.wordToWord32# 0xe0e0e0e0## &&# (5# <<!# u32#)
  6# -> GHC.wordToWord32# 0xc0c0c0c0## &&# (6# <<!# u32#)
  7# -> GHC.wordToWord32# 0x80808080## &&# (7# <<!# u32#)
  8# -> GHC.wordToWord32# 0##
  _  -> errorWithoutStackTrace (shows 'uncheckedShiftL# ": index out of bounds")

-- | TODO: docs
--
-- @since 1.0.0
uncheckedShiftR# :: Word32# -> Int# -> Word32#
uncheckedShiftR# u32# = \case
  0# -> u32#
  1# -> GHC.wordToWord32# 0x7f7f7f7f## &&# (u32# >>!# 1#)
  2# -> GHC.wordToWord32# 0x3f3f3f3f## &&# (u32# >>!# 2#)
  3# -> GHC.wordToWord32# 0x1f1f1f1f## &&# (u32# >>!# 3#)
  4# -> GHC.wordToWord32# 0x0f0f0f0f## &&# (u32# >>!# 4#)
  5# -> GHC.wordToWord32# 0x07070707## &&# (u32# >>!# 5#)
  6# -> GHC.wordToWord32# 0x03030303## &&# (u32# >>!# 6#)
  7# -> GHC.wordToWord32# 0x01010101## &&# (u32# >>!# 7#)
  8# -> GHC.wordToWord32# 0##
  _  -> errorWithoutStackTrace (shows 'uncheckedShiftR# ": index out of bounds")

-- Vec4x8# - Fold --------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
sum# :: Word32# -> Word32#
sum# u32# =
  let mask0# = GHC.wordToWord32# 0x00ff00ff##
      mask1# = GHC.wordToWord32# 0x000003ff##
      x# = GHC.plusWord32# (u32# &&# mask0#) ((u32# >>!# 8#) &&# mask0#)
   in GHC.plusWord32# x# (x# >>!# 16#) &&# mask1#
