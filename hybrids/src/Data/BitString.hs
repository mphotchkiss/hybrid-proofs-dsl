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
    , bytesToBits
    , BitString(..)
    , FromBits(..)
    , toBits
    , xor
    , append
    , bitSingle
    , nat8
    , nat16
    , encN
    ) where

import Control.Applicative
import Data.Bits (shift, (.&.), testBit)
import Data.ByteArray (Bytes, unpack)
import Data.Foldable
import Data.Type.Combinator
import Data.Type.Fin
import Data.Type.Nat
import Data.Type.Vector
import Type.Class.Known
import Type.Family.Nat

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

bytesToBits :: Bytes -> [Bit]
bytesToBits bs = fold $ map wordToBits $ unpack bs
  where
    wordToBits w = map (generate w) [0..7]
    generate w n = if testBit w n
                        then B1
                        else B0

newtype BitString n = BitString {unBitString :: Vec n Bit }
    deriving(Eq)
instance Show (BitString 'Z) where
    show _ = ""
instance Show (BitString n) => Show (BitString ('S n)) where
    show (BitString ((I bit) :* bs)) =
        show bit ++ show (BitString bs)

nat8 :: Nat N8
nat8 = (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ Z_))))))))

nat16 :: Nat (N8 + N8)
nat16 = (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ Z_))))))))))))))))

xor :: BitString n -> BitString n -> BitString n
xor (BitString s1) (BitString s2) = BitString $ vap (liftA2 bitXor) s1 s2

append :: BitString n -> BitString m -> BitString (n + m)
append (BitString s1) (BitString s2) = BitString (s1 .++ s2)

bitSingle :: Bit -> BitString N1
bitSingle b = BitString ((I b) :* ØV)

class FromBits bs where
    fromBits :: [Bit] -> Maybe (BitString bs)

instance FromBits 'Z where
    fromBits []     = Just $ BitString ØV
    fromBits (b:bs) = Nothing

instance FromBits n => FromBits ('S n) where
    fromBits []     = Nothing
    fromBits (b:bs) = do
        rest <- fromBits bs
        return $ append (bitSingle b) rest

toBits :: BitString n -> [Bit]
toBits (BitString ØV)            = []
toBits (BitString ((I b) :* bs)) = b : toBits (BitString bs)

encN :: Nat n -> Int -> BitString n
encN size n = BitString $ vgen size b
  where
    b i = if (shift n (-((natVal size - 1) - fin i))) .&. 1 == 0
            then pure B0
            else pure B1
