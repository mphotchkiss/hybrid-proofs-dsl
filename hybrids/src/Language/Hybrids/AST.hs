module Language.Hybrids.AST
    ( HName
    , HVar(..)
    , HExpr(..)
    , HArgs(..)
    , HTerm(..)
    , HSig(..)
    , Error
    , HContext
    , emptyCtx
    , evalHExpr
    , evalHTerm
    ) where

import           Crypto.Random
import           Data.BitString
import           Data.Map (Map)
import qualified Data.Map as M

type HName = String
newtype HVar = HVar HName

instance Show HVar where
    show (HVar name) = name

data HExpr where
    Variable    :: HVar -> HExpr
    Literal     :: BitString -> HExpr
    Xor         :: HExpr -> HExpr -> HExpr
    Append      :: HExpr -> HExpr -> HExpr
    Call        :: HName -> HArgs -> HExpr

instance Show HExpr where
    show (Variable (HVar name)) = name
    show (Literal bs)           = "0b" ++ ppBits (toBits bs)
    show (Xor e1 e2)            = show e1 ++ " (+) " ++ show e2
    show (Append e1 e2)         = show e1 ++ " || " ++ show e2
    show (Call name args)       = name ++ "(" ++ show args ++ ")"

newtype HArgs = HArgs [HExpr]

instance Show HArgs where
    show (HArgs [])     = ""
    show (HArgs [e])    = show e
    show (HArgs (e:as)) = show e ++ ", " ++ show (HArgs as)

data HTerm where
    Assign   :: HVar -> HExpr -> HTerm
    Gets     :: BitWidth -> HVar -> HTerm
    (:>)     :: HTerm -> HTerm -> HTerm
    Return   :: HExpr -> HTerm
    Routine  :: HName -> HSig -> HTerm -> HTerm

instance Show HTerm where
    show = printHTerm 0

newtype HSig = HSig { sigArgs :: [HName] }

instance Show HSig where
    show (HSig [])     = ""
    show (HSig [e])    = e
    show (HSig (e:as)) = e ++ ", " ++ show (HSig as)

printHTerm :: Int -> HTerm -> String
printHTerm lvl (Assign v e)  =
  replicate lvl '\t' ++ show v ++ " := " ++ show e
printHTerm lvl (Gets bits v) =
  replicate lvl '\t' ++ show v ++ " <- {0, 1}^" ++ show bits
printHTerm lvl ((:>) t1 t2)  =
  printHTerm lvl t1 ++ ";\n" ++ printHTerm lvl t2
printHTerm lvl (Return e)    =
  replicate lvl '\t' ++ "return " ++ show e
printHTerm lvl (Routine name args body) =
  replicate lvl '\t' ++ "def " ++ name ++ "(" ++ show args ++ "):\n"
                     ++ printHTerm (lvl + 1) body

type HVarContext = Map HName BitString
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

hVarDef :: HName -> BitString -> HContext -> HContext
hVarDef name val ctx = ctx { ctxVars = vars' }
  where
    vars' = M.insert name val (ctxVars ctx)

hVarLookup :: HContext -> HName -> Either Error BitString
hVarLookup ctx name =
    case M.lookup name $ ctxVars ctx of
        Just bs -> Right bs
        Nothing -> Left ("Error: Variable " <> name <> " used before defintion")

hFnDef :: HName -> HSig -> HTerm -> HContext -> HContext
hFnDef name sig body ctx = ctx { ctxFns = fns' }
  where
    fns' = M.insert name (sigArgs sig, body) $ ctxFns ctx

hFnLookup :: HContext -> HName -> Either Error ([HName], HTerm)
hFnLookup ctx name =
    case M.lookup name $ ctxFns ctx of
        Just fn -> Right fn
        Nothing -> Left ("Error: Function " <> name <> " used before definition")

addFnCtx :: [HName] -> HArgs -> HContext -> Either Error HContext
addFnCtx []     (HArgs [])       ctx = Right ctx
addFnCtx (n:ns) (HArgs (e:args)) ctx = do
    bs <- evalHExpr ctx e
    ctx' <- addFnCtx ns (HArgs args) ctx
    return $ hVarDef n bs ctx'
addFnCtx _      _                 _   = Left "Error: Function call got messed up"

evalHExpr :: HContext -> HExpr -> Either Error BitString
evalHExpr ctx (Variable (HVar vName)) =
    hVarLookup ctx vName
evalHExpr _ctx (Literal bs)   =
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
        Just bs -> return bs
        Nothing -> Left ("Error: function " <> fnm <> " did not return.")

evalHTerm :: HContext -> HTerm -> Either Error (HContext, Maybe BitString)
evalHTerm ctx (Assign (HVar name) e) = do
    bs <- evalHExpr ctx e
    return (hVarDef name bs ctx, Nothing)
evalHTerm ctx (Gets (BitWidth n) var) = do
    let (bs, g') = randomBytesGenerate ((n `div` 8) + 1) (ctxRand ctx)
        bString = BitString $ take n $ bytesToBits bs
    (ctx', _) <- evalHTerm ctx (Assign var (Literal bString))
    return (ctx' { ctxRand = g' }, Nothing)
evalHTerm ctx ((:>) t1 t2) = do
    (ctx', res) <- evalHTerm ctx t1
    case res of
        Nothing -> evalHTerm ctx' t2
        Just r  -> return (ctx', Just r)
evalHTerm ctx (Return e) = do
    bs <- evalHExpr ctx e
    pure (ctx, Just bs)
evalHTerm ctx (Routine name sig body) =
    return (hFnDef name sig body ctx, Nothing)
