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

module Hybrids where

import Data.BitString
import Data.Type.Fin
import Type.Family.Nat

data HVar n = String

data HExpr n where
    Variable    :: HVar n -> HExpr n
    Literal     :: HExpr n -> HExpr n
    Xor         :: HExpr n -> HExpr n -> HExpr n
    Append      :: HExpr a -> HExpr b -> HExpr (a + b)

data HTerm where
    Assignment  :: HVar n -> HExpr n -> HTerm
    Gets        :: HVar n -> Fin n   -> HTerm

main :: IO ()
main = do
    let ones = enc8 255
    putStrLn (show ones)
