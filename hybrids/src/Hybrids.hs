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
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Hybrids where

import           Data.BitString
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Proxy
import           Data.Type.Fin
import           Type.Family.Nat

type HName = String
data HVar (size :: N) = HVar HName

data HExpr (size :: N) where
    Variable    :: HVar n -> HExpr n
    Literal     :: BitString n -> HExpr n
    Xor         :: HExpr n -> HExpr n -> HExpr n
    Append      :: HExpr a -> HExpr b -> HExpr (a + b)
    -- Call        :: HVar m -> [(forall (n :: N). (HVar n, HExpr n))] -> HExpr m

data HTerm (size :: N) where
    Assign   :: HVar n -> HExpr n -> HTerm N0
    Gets     :: HVar n -> HTerm N0
    (:>)     :: HTerm n -> HTerm m -> HTerm m
    Return   :: HExpr n -> HTerm n
    -- Routine  :: HVar n -> [(forall (n :: N). HVar n)] -> HTerm n -> HTerm _Z

type HVarContext = Map HName (Maybe [Bit])
-- type HFnContext = forall (size :: N). Proxy size -> HName -> Maybe (HTerm size)

data HContext = 
     HContext { ctxVariables :: HVarContext
              -- , ctxFunctions :: HFnContext
              }

type Error = String

hVarLookup :: HContext -> HName -> Either Error (BitString n)
hVarLookup ctx name =
    case M.lookup name $ ctxVariables ctx of
        Just (Just bs) -> case fromBits bs of
                     Just str -> Right str
                     Nothing  -> Left ("Error: Variable size mismatch")
        _ -> Left ("Error: " <> name <> " used before defintion")

otp8_test :: HTerm N8
otp8_test = Gets (HVar "k")
         :> Assign (HVar "m") (Literal $ enc8 100)
         :> Return (Xor (Variable $ HVar "k") (Variable $ HVar "m"))

evalHExpr :: HContext -> HExpr n -> Either Error (BitString n)
evalHExpr ctx (Variable (HVar vName)) =
    hVarLookup ctx vName
evalHExpr ctx (Literal bs)   =
    Right bs
evalHExpr ctx (Xor e1 e2)    = do
    b1 <- evalHExpr ctx e1
    b2 <- evalHExpr ctx e2
    return (b1 `xor` b2)
evalHExpr ctx (Append e1 e2) = do
    b1 <- evalHExpr ctx e1
    b2 <- evalHExpr ctx e2
    return (b1 `append` b2)
-- evalHExpr ctx (Call fn args) = undefined

evalHTerm :: HContext -> HTerm n -> Either Error (HContext, HTerm m)
evalHTerm = undefined

test :: IO ()
test = do
    let ones = enc8 255
    putStrLn (show ones)
