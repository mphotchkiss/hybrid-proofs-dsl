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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Hybrids where

import           Crypto.Random
import           Data.BitString
import           Data.Kind (Type)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Proxy
import           Data.Type.Fin
import           Data.Type.Nat
import           Type.Class.Known
import           Type.Family.Nat
import           Type.Family.List

type HName = String
data HVar (size :: N) = HVar HName

data HExpr (size :: N) where
    Variable    :: FromBits n => HVar n -> HExpr n
    Literal     :: BitString n -> HExpr n
    Xor         :: HExpr n -> HExpr n -> HExpr n
    Append      :: HExpr a -> HExpr b -> HExpr (a + b)
    Call        :: FromBits n => HName -> HArgs n as -> HExpr n

data HArgs (ret :: N) (as :: [N]) where
    HArgsBase :: HArgs n '[]
    HArgsArg  :: FromBits m => HExpr m -> HArgs n as -> HArgs n (m :< as)

data HTerm where
    Assign   :: HVar n -> HExpr n -> HTerm
    Gets     :: FromBits n => Nat n -> HVar n -> HTerm
    (:>)     :: HTerm -> HTerm -> HTerm
    Return   :: HExpr n -> HTerm
    Routine  :: HName -> HSig n as -> HTerm -> HTerm

data HSig (ret :: N) (argc :: N) where
    HSigBase :: HSig n Z
    HSigArg  :: HName -> HSig n c -> HSig n (S c)

sigArgs :: HSig n c -> [HName]
sigArgs HSigBase          = []
sigArgs (HSigArg name as) = name : sigArgs as

type HVarContext = Map HName [Bit]
type HFnContext = Map HName ([HName], HTerm)

data HContext = 
     HContext { ctxVars :: HVarContext
              , ctxFns  :: HFnContext
              , ctxRand :: SystemDRG
              }

instance Show HContext where
    show = const "HContext"

type Error = String

emptyCtx :: IO HContext
emptyCtx = do
    sDrg <- getSystemDRG
    return $ HContext M.empty M.empty sDrg

hVarDef :: HName -> BitString n -> HContext -> HContext
hVarDef name val ctx = ctx { ctxVars = vars' }
  where
    vars' = M.insert name (toBits val) (ctxVars ctx)

hVarLookup :: FromBits n => HContext -> HName -> Either Error (BitString n)
hVarLookup ctx name =
    case M.lookup name $ ctxVars ctx of
        Just bs -> case fromBits bs of
                     Just str -> Right str
                     Nothing  -> Left ("Error: Variable size mismatch")
        Nothing -> Left ("Error: Variable " <> name <> " used before defintion")

hFnDef :: HName -> HSig ret c -> HTerm -> HContext -> HContext
hFnDef name sig body ctx = ctx { ctxFns = fns' }
  where
    fns' = M.insert name (sigArgs sig, body) $ ctxFns ctx

hFnLookup :: HContext -> HName -> Either Error ([HName], HTerm)
hFnLookup ctx name =
    case M.lookup name $ ctxFns ctx of
        Just fn -> Right fn
        Nothing -> Left ("Error: Function " <> name <> " used before definition")

addFnCtx :: [HName] -> HArgs r as -> HContext -> Either Error HContext
addFnCtx []     HArgsBase         ctx = Right ctx
addFnCtx (n:ns) (HArgsArg e args) ctx = do
    bs <- evalHExpr ctx e
    ctx' <- addFnCtx ns args ctx
    return $ hVarDef n bs ctx'
addFnCtx _      _                 _   = Left "Error: Function call got messed up"

otp8_real :: HTerm
otp8_real = 
    Routine "Enc" (HSigArg "k" (HSigArg "m" HSigBase)) (
        Return (Xor (Variable @N8 $ HVar "k") (Variable $ HVar "m"))
    )
 :> Gets nat8 (HVar @N8 "key")
 :> Assign (HVar "message") (Literal $ enc8 100)
 :> Return (Call "Enc" (HArgsArg (Variable @N8 $ HVar "key")
                       (HArgsArg (Variable @N8 $ HVar "message")
                       (HArgsBase :: HArgs N8 '[]))))

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
evalHExpr ctx (Call fnm args) = do
    (vs, f) <- hFnLookup ctx fnm
    ctx' <- addFnCtx vs args ctx
    (_ctx, mBs) <- evalHTerm ctx' f
    case mBs of
        Just bs -> case fromBits bs of
            Just bstr -> return bstr
            Nothing   -> Left "Error: function return size mismatch."
        Nothing -> Left ("Error: function " <> fnm <> " did not return.")

evalHTerm :: HContext -> HTerm -> Either Error (HContext, Maybe [Bit])
evalHTerm ctx (Assign (HVar name) e) = do
    bs <- evalHExpr ctx e
    return (hVarDef name bs ctx, Nothing)
evalHTerm ctx (Gets n var) =
    let (bs, g') = randomBytesGenerate ((natVal n `div` 8) + 1) (ctxRand ctx)
    in case fromBits $ take (natVal n) $ bytesToBits bs of
        Nothing      -> Left "Error: gets size mismatch"
        Just bString -> do
            (ctx', _) <- evalHTerm ctx (Assign var (Literal bString))
            return (ctx' { ctxRand = g' }, Nothing)
evalHTerm ctx ((:>) t1 t2) = do
    (ctx', res) <- evalHTerm ctx t1
    case res of
        Nothing -> evalHTerm ctx' t2
        Just r  -> return (ctx', Just r)
evalHTerm ctx (Return e) = do
    bs <- evalHExpr ctx e
    pure (ctx, Just $ toBits bs)
evalHTerm ctx (Routine name sig body) =
    return (hFnDef name sig body ctx, Nothing)

test :: IO ()
test = do
    let ones = enc8 255
    putStrLn (show ones)
