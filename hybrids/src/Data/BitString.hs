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
    , xor
    , enc8
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
instance Show (BitString ('S n)) where
    show (BitString ((I bit) :* string)) =
        show bit ++ case string of
                      ØV     -> ""
                      b :* s -> show (BitString $ b :* s)

nat8 :: Nat N8
nat8 = (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ Z_))))))))

xor :: BitString n -> BitString n -> BitString n
xor (BitString s1) (BitString s2) = BitString $ vap (liftA2 bitXor) s1 s2

enc8 :: Int -> BitString N8
enc8 n = BitString $ vgen nat8 b
  where
    b i = if (shift n (-(7 - fin i))) .&. 1 == 0
            then pure B0
            else pure B1
