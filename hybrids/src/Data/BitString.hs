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
{-# LANGUAGE IncoherentInstances #-}

module Data.BitString
    ( Bit(..)
    , BitString(..)
    , FromBits(..)
    , xor
    , append
    , enc8
    , bitSingle
    ) where

import Control.Applicative
import Data.Bits (shift, (.&.))
import Data.Type.Combinator
import Data.Type.Fin
import Data.Type.Nat
import Data.Type.Vector
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

newtype BitString n = BitString {unBitString :: Vec n Bit }
    deriving(Eq)
instance Show (BitString N0) where
    show _ = ""
instance Show (BitString n) => Show (BitString ('S n)) where
    show (BitString ((I bit) :* bs)) =
        show bit ++ show (BitString bs)

nat8 :: Nat N8
nat8 = (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ Z_))))))))

xor :: BitString n -> BitString n -> BitString n
xor (BitString s1) (BitString s2) = BitString $ vap (liftA2 bitXor) s1 s2

append :: BitString n -> BitString m -> BitString (n + m)
append (BitString s1) (BitString s2) = BitString (s1 .++ s2)

bitSingle :: Bit -> BitString N1
bitSingle b = BitString ((I b) :* ØV)

class FromBits bs where
    fromBits :: [Bit] -> Maybe bs

instance FromBits (BitString N0) where
    fromBits []     = Just $ BitString ØV
    fromBits (b:bs) = Nothing

instance FromBits (BitString n) => FromBits (BitString ('S n)) where
    fromBits []     = Nothing
    fromBits (b:bs) = do
        rest <- fromBits bs
        return $ append (bitSingle b) rest

instance FromBits (BitString (n :: N)) where
    fromBits = fromBits

enc8 :: Int -> BitString N8
enc8 n = BitString $ vgen nat8 b
  where
    b i = if (shift n (-(7 - fin i))) .&. 1 == 0
            then pure B0
            else pure B1
