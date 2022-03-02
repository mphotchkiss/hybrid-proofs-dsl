{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}

module Data.BitString
    ( Bit(..)
    , BitString(..)
    , BitWidth(..)
    , ppBits
    , xor
    , append
    , bitSingle
    , encN
    , bytesToBits
    , bits8
    , bits16
    , bits32
    , bits64
    , bits128
    ) where

import Data.Bits (shift, (.&.), testBit)
import Data.ByteArray (Bytes, unpack)
import Data.Foldable

data Bit = B0 | B1
    deriving(Eq)

instance Show Bit where
    show B0 = "0"
    show B1 = "1"

bitXor :: Bit -> Bit -> Bit
bitXor B0 B0 = B0
bitXor B1 B0 = B1
bitXor B0 B1 = B1
bitXor B1 B1 = B0

ppBits :: [Bit] -> String
ppBits []     = ""
ppBits (b:bs) = show b ++ ppBits bs

newtype BitString = BitString { toBits :: [Bit] }
    deriving(Eq)
instance Show BitString where
    show (BitString []) = ""
    show (BitString (bit:bs)) =
        show bit ++ show (BitString bs)

newtype BitWidth = BitWidth Int
    deriving(Show, Eq, Ord)

bits8 :: BitWidth
bits8 = BitWidth 8

bits16 :: BitWidth
bits16 = BitWidth 16

bits32 :: BitWidth
bits32 = BitWidth 32

bits64 :: BitWidth
bits64 = BitWidth 64

bits128 :: BitWidth
bits128 = BitWidth 128

xor :: BitString -> BitString -> BitString
xor (BitString s1) (BitString s2) = BitString $ zipWith bitXor s1 s2

append :: BitString -> BitString -> BitString
append (BitString s1) (BitString s2) = BitString (s1 ++ s2)

bitSingle :: Bit -> BitString
bitSingle b = BitString [b]

encN :: BitWidth -> Int -> BitString
encN (BitWidth size) n = BitString $ map b [1..size]
  where
    b i = if (shift n (-((size - 1) - i))) .&. 1 == 0
            then B0
            else B1

bytesToBits :: Bytes -> [Bit]
bytesToBits bs = fold $ map wordToBits $ unpack bs
  where
    wordToBits w = map (generate w) [0..7]
    generate w n = if testBit w n
                        then B1
                        else B0
