module Language.Hybrids.AST
    ( HName
    , HVar(..)
    , HExpr(..)
    , HArgs(..)
    , Bound(..)
    , Boolean(..)
    , HTerm(..)
    , HSig(..)
    , Routine(..)
    , Block(..)
    , Library(..)
    , Error
    , HContext
    , emptyCtx
    , evalHExpr
    , evalHTerm
    , call
    ) where

import           Crypto.Random
import           Data.BitString
import           Data.Map (Map)
import qualified Data.Map as M
import System.IO
import System.Exit

type HName = String
newtype HVar = HVar HName

instance Show HVar where
    show (HVar name) = name

data Boolean where
    Eq          :: HExpr -> HExpr -> Boolean
    Gt          :: HExpr -> HExpr -> Boolean
    Lt          :: HExpr -> HExpr -> Boolean
    Neq         :: HExpr -> HExpr -> Boolean
    Gte         :: HExpr -> HExpr -> Boolean
    Lte         :: HExpr -> HExpr -> Boolean
    Empty       :: HExpr -> Boolean
    Undef       :: HExpr -> Boolean

instance Show Boolean where
    show (Eq e1 e2) = show e1 ++ " == " ++ show e2
    show (Gt e1 e2) = show e1 ++ " > " ++ show e2
    show (Lt e1 e2) = show e1 ++ " < " ++ show e2
    show (Neq e1 e2) = show e1 ++ " != " ++ show e2
    show (Gte e1 e2) = show e1 ++ " >= " ++ show e2
    show (Lte e1 e2) = show e1 ++ " =< " ++ show e2
    show (Empty e) = show e ++ " empty?"
    show (Undef e) = show e ++ " undefined?"

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

newtype Bound = Bound (Either Int HVar)

instance Show Bound where
    show (Bound (Left n)) = show n
    show (Bound (Right v)) = show v

data HTerm where
    Assign   :: HVar -> HExpr -> HTerm
    Gets     :: BitWidth -> HVar -> HTerm
    (:>)     :: HTerm -> HTerm -> HTerm
    Return   :: HExpr -> HTerm
    Routine  :: HName -> HSig -> HTerm -> HTerm
    Loop     :: Bound -> Bound -> HTerm -> HTerm
    If       :: Boolean -> HTerm -> HTerm

instance Show HTerm where
    show = printHTerm 0

newtype HSig = HSig { sigArgs :: [HName] }

instance Show HSig where
    show (HSig [])     = ""
    show (HSig [e])    = e
    show (HSig (e:as)) = e ++ ", " ++ show (HSig as)

data Routine where
    Rout :: Maybe HTerm -> HName -> HSig -> HTerm -> Routine

instance Show Routine where
    show (Rout Nothing name sig term) =
        name ++ "(" ++ show sig ++ "):" ++ "\n" ++ printHTerm 1 term
    show (Rout pre name sig term) =
        show pre ++ "\n" ++ name ++ "(" ++ show sig ++ "):" ++ "\n" ++ printHTerm 1 term

newtype Block = Block [Routine]

instance Show Block where
    show (Block []) = ""
    show (Block [r]) = show r
    show (Block (r:rs)) = show r ++ show rs

data Library where
    Lib :: HName -> Block -> Maybe Block -> Library

instance Show Library where
    show (Lib name main Nothing) = "L_" ++ name ++ "\n" ++ show main
    show (Lib name main (Just sub)) = "L_" ++ name ++ "\n" ++ show main ++ "\n" ++ subroutines sub

subroutines :: Block -> String
subroutines (Block []) = ""
subroutines (Block (x:xs)) = "◇" ++ show x ++ subroutines (Block xs)

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
printHTerm lvl (Loop b1 b2 t) = 
  replicate lvl '\t' ++ "for i = " ++ show b1 ++ " to " ++ show b2 ++ ":\n" ++ printHTerm (lvl+1) t
printHTerm lvl (If b t) = 
  replicate lvl '\t' ++ "if (" ++ show b ++ "):\n" ++ printHTerm (lvl+1) t

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
evalHTerm _ (Loop _ _ _) = error "Loop evaluation not yet defined"
evalHTerm _ (If _ _) = error "Loop evaluation not yet defined"


findRout :: Block -> String -> Maybe HTerm
findRout (Block []) _ = Nothing
findRout (Block ((Rout (Just a) n b c):xs)) name =
    if n == name then Just (Routine n b (a :> c))
    else findRout (Block xs) name
findRout (Block ((Rout Nothing  n b c):xs)) name =
    if n == name then Just (Routine n b c)
    else findRout (Block xs) name

callRout :: HTerm -> Maybe HTerm -> IO ()
callRout _   Nothing = hPutStrLn stderr "Method not found"
callRout req (Just rout) = do
    ctx <- emptyCtx
    case evalHTerm ctx rout of
        Left e -> do
            hPutStrLn stderr e
            exitFailure
        Right (ctx', _b) ->
            case evalHTerm ctx' req of
                Left e -> do
                    hPutStrLn stderr e
                    exitFailure
                Right (_ctx, bs) -> do
                    print (show bs)
                    exitSuccess

call :: String -> [(String, Int, BitWidth)] -> Library -> IO ()
call name args (Lib _n block _blk) = callRout (fnctCall name args) (findRout block name)

fnctCall :: String -> [(String, Int, BitWidth)] -> HTerm
fnctCall name [] = Return(Call name (HArgs []))
fnctCall name vars = generateAssignments vars :> Return(Call name (HArgs (generateArgs vars)))

generateAssignments :: [(String, Int, BitWidth)] -> HTerm
generateAssignments [] = error "Empty assignments list"
generateAssignments [(s,i,b)] = Assign (HVar s) (Literal (encN b i))
generateAssignments ((s,i,b):ss) = Assign (HVar s) (Literal (encN b i))
                                    :> generateAssignments ss

generateArgs :: [(String, Int, BitWidth)] -> [HExpr]
generateArgs [] = []
generateArgs ((s,_i,_b):ss) = Variable (HVar s) : generateArgs ss

