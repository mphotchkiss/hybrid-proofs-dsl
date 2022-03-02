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

import Data.Char
import Data.Maybe
import Language.Hybrids.AST

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

extractRoutine :: HName -> HTerm -> Maybe HTerm
extractRoutine fName (Routine name _sig body)
    | fName == name = Just body
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
    trans term | Just body <- extractRoutine fName term =
        fromMaybe term $ replace body term
               | otherwise = term

    containsE :: HExpr -> Bool
    containsE (Variable _)   = False
    containsE (Literal _)    = False
    containsE (Xor e1 e2)    = containsE e1 || containsE e2
    containsE (Append e1 e2) = containsE e1 || containsE e2
    containsE (Call name _)  = name == fName

    replaceE :: HTerm -> HExpr -> HExpr
    replaceE body (Xor e1 e2)         = Xor (replaceE body e1) (replaceE body e2)
    replaceE body (Append e1 e2)      = Append (replaceE body e1) (replaceE body e2)
    replaceE _    call@(Call name _)
        | name == fName = Variable (HVar $ map toLower fName)
        | otherwise     = call
    replaceE _    e                   = e

    rewriteBody ((:>) t1 t2) = rewriteBody t1 :> rewriteBody t2
    rewriteBody (Return e)   = Assign (HVar $ map toLower fName) e
    rewriteBody body         = body

    replace :: HTerm -> HTerm -> Maybe HTerm
    replace body ((:>) t1 t2)  =
        case replace body t1 of
            Nothing  -> replace body t2
            Just t1' -> case replace body t2 of 
                Nothing  -> Just t1'
                Just t2' -> Just (t1' :> t2')
    replace body term@(Assign v e)
        | containsE e = Just (rewriteBody body :> Assign v (replaceE body e))
        | otherwise   = Just term
    replace body term@(Return e) 
        | containsE e = Just (rewriteBody body :> Return (replaceE body e))
        | otherwise   = Just term
    replace _    f@(Routine n _ _) = if n == fName then Nothing else Just f
    replace _    hterm             = Just hterm

unsafeAnyInter :: HTerm -> Interchangeable
unsafeAnyInter replace = Inter (const replace)

unsafeAnyIndist :: HTerm -> Indistinguishable
unsafeAnyIndist replace = Indis (const replace)
