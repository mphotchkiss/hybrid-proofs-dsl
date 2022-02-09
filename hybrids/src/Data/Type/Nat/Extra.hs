{-# LANGUAGE TypeOperators #-}

module Data.Type.Nat.Extra where

import Data.Type.Nat
import Type.Family.Nat

-- Definitions for N and Nat for common powers of 2

nat8 :: Nat N8
nat8 = (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ Z_))))))))

type N16 = N2 ^ N4

nat16 :: Nat N16
nat16 = (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ Z_))))))))))))))))

type N32 = N2 ^ N5

nat32 :: Nat (N8 + N8 + N8 + N8)
nat32 = (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_
        (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_
        Z_))))))))))))))))))))))))))))))))

type N64 = N2 ^ N6

nat64 :: Nat (N2 ^ N6)
nat64 =
    (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_
    (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_
    (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_
    (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_
      Z_))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

type N128 = Nat (N2 ^ N7)

nat128 :: N128
nat128 = 
    (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_
    (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_
    (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_
    (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_
    (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_
    (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_
    (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_
    (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_
        Z_))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

