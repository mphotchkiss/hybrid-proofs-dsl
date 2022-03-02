module Language.Hybrids.Transform
    ( Interchangeable
    , Indistinguishable
    , Transform(..)
    , composeTransform
    , unwrapT
    , unsafeAnyInter
    , unsafeAnyIndist
    , inline
    ) where

import           Data.Char
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import           Language.Hybrids.AST

newtype Interchangeable = Inter (HTerm -> HTerm)

newtype Indistinguishable = Indis (HTerm -> HTerm)

data Transform = InterT Interchangeable
               | IndisT Indistinguishable

unwrapT :: Transform -> HTerm -> HTerm
unwrapT (InterT (Inter f)) = f
unwrapT (IndisT (Indis f)) = f

composeTransform :: Transform -> Transform -> Transform
composeTransform (InterT (Inter t1)) (InterT (Inter t2)) =
    InterT (Inter (t1 . t2))
composeTransform t1                  t2                  =
    IndisT (Indis ((unwrapT t1) . (unwrapT t2)))

extractRoutine :: HName -> HTerm -> Maybe (HSig, HTerm)
extractRoutine fName (Routine name sig body)
    | fName == name = Just (sig, body)
    | otherwise     = Nothing
extractRoutine fName ((:>) t1 t2) =
    case extractRoutine fName t1 of
        Just res -> Just res
        Nothing  -> extractRoutine fName t2
extractRoutine _     _            = Nothing

-- inline: transformation replaces all instances of named function with
-- function contents, then removes function.
inline :: HName -> Interchangeable
inline fName = Inter trans
  where
    trans term | Just routine <- extractRoutine fName term =
        fromMaybe term $ replace routine term
               | otherwise = term

    containsE :: HExpr -> Maybe HArgs
    containsE (Variable _)     = Nothing
    containsE (Literal _)      = Nothing
    containsE (Xor e1 e2)
        | Just args <- containsE e1 = Just args
        | otherwise                 = containsE e2
    containsE (Append e1 e2)
        | Just args <- containsE e1 = Just args
        | otherwise                 = containsE e2
    containsE (Call name args) = if name == fName
                                    then Just args
                                    else Nothing

    replaceE :: HTerm -> HExpr -> HExpr
    replaceE body (Xor e1 e2)         = Xor (replaceE body e1) (replaceE body e2)
    replaceE body (Append e1 e2)      = Append (replaceE body e1) (replaceE body e2)
    replaceE _    call@(Call name _)
        | name == fName = Variable (HVar $ map toLower fName)
        | otherwise     = call
    replaceE _    e                   = e

    rewriteCtx :: HSig -> HArgs -> Map HName HExpr
    rewriteCtx (HSig [])     (HArgs [])     = M.empty
    rewriteCtx (HSig (v:vs)) (HArgs (e:es)) = M.insert v e $ rewriteCtx (HSig vs) (HArgs es)
    rewriteCtx _             _              = error "Call argument mismatch during inline rewrite"

    rewriteBody :: Map HName HExpr -> HTerm -> HTerm
    rewriteBody ctx ((:>) t1 t2) = rewriteBody ctx t1 :> rewriteBody ctx t2
    rewriteBody ctx (Return e)   = Assign (HVar $ map toLower fName) $ rwrtBodyE ctx e
    rewriteBody ctx (Assign v e) = Assign v $ rwrtBodyE ctx e
    rewriteBody _   body         = body

    rwrtBodyE :: Map HName HExpr -> HExpr -> HExpr
    rwrtBodyE _   e@(Literal _)            = e
    rwrtBodyE ctx v@(Variable (HVar name)) = case M.lookup name ctx of 
                                              Just e -> e
                                              Nothing -> v
    rwrtBodyE ctx (Xor e1 e2)              = Xor (rwrtBodyE ctx e1) (rwrtBodyE ctx e2)
    rwrtBodyE ctx (Append e1 e2)           = Append (rwrtBodyE ctx e1) (rwrtBodyE ctx e2)
    rwrtBodyE ctx (Call name (HArgs es))   = Call name (HArgs (map (rwrtBodyE ctx) es))

    replace :: (HSig, HTerm) -> HTerm -> Maybe HTerm
    replace (sig, body) ((:>) t1 t2)  =
        case replace (sig, body) t1 of
            Nothing  -> replace (sig, body) t2
            Just t1' -> case replace (sig, body) t2 of 
                Nothing  -> Just t1'
                Just t2' -> Just (t1' :> t2')
    replace (sig, body) term@(Assign v e)
        | Just args <- containsE e = Just (rewriteBody (rewriteCtx sig args) body 
                                            :> Assign v (replaceE body e))
        | otherwise                = Just term
    replace (sig, body) term@(Return e) 
        | Just args <- containsE e = Just (rewriteBody (rewriteCtx sig args) body 
                                            :> Return (replaceE body e))
        | otherwise                = Just term
    replace _    f@(Routine n _ _) = if n == fName then Nothing else Just f
    replace _    hterm             = Just hterm

unsafeAnyInter :: HTerm -> Interchangeable
unsafeAnyInter replace = Inter (const replace)

unsafeAnyIndist :: HTerm -> Indistinguishable
unsafeAnyIndist replace = Indis (const replace)
