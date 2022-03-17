module Language.Hybrids.Examples where

import Data.BitString
import Language.Hybrids.AST

callOtp :: BitWidth -> Int -> IO ()
callOtp size n = call "EAVESDROP" [("m", n, size)] (otpReal size)

otpReal :: BitWidth -> Library
otpReal size =
    Lib "otp-real" (Block [Rout Nothing "EAVESDROP" (HSig ["m"])
        (
            Gets size (HVar "k")
            :> Assign (HVar "c") (Xor (Variable $ HVar "k") (Variable $ HVar "m"))
            :> Return (Variable $ HVar "c")
        )
    ]) Nothing

otpRand :: BitWidth -> Library 
otpRand size = 
    Lib "otp-rand" (Block [Rout Nothing "EAVESDROP" (HSig ["m"])
        (
            Gets size (HVar "c")
            :> Return (Variable $ HVar "c")
        )
    ]) Nothing

prgReal :: BitWidth -> Library
prgReal size =
    Lib "prg-real" (Block [Rout Nothing "QUERY" (HSig [])
        (
            Gets size (HVar "s")
            :> Return (Call "G" (HArgs [Variable (HVar "s")]))
        )
    ]) Nothing

prgRand :: BitWidth -> Library 
prgRand (BitWidth n) = 
    Lib "prg-rand" (Block [Rout Nothing "QUERY" (HSig [])
        (
            Gets (BitWidth (n*2)) (HVar "r")
            :> Return (Variable $ HVar "r")
        )
    ]) Nothing

prfReal :: BitWidth -> BitWidth -> Library 
prfReal (BitWidth n1) _=
    Lib "prf-real" (Block [
        Rout (Just (Gets (BitWidth n1) (HVar "k"))) "LOOKUP" (HSig ["x"]) 
            (
                Return (Call "F" (HArgs[Variable (HVar "k"), Variable (HVar "x")]))
            )
    ]) Nothing

prfRand :: BitWidth -> BitWidth -> Library 
prfRand _ (BitWidth n2) = 
    Lib "prf-rand" (Block [
        Rout (Just (Assign (HVar "T") (Variable (HVar "[]")))) "LOOKUP" (HSig ["x"])
        (
            If (Undef (Variable (HVar "T[x]"))) (Gets (BitWidth n2) (HVar "T[x]"))
            :> Return (Variable (HVar "T[x]"))
        )
    ]) Nothing

cpaDollaReal :: Library 
cpaDollaReal = 
    Lib "cpa$-real" (Block [
        Rout (Just (Assign (HVar "k") (Variable (HVar "KeyGen")))) "CTXT" (HSig ["m"])
        (
            Assign (HVar "c") (Call "Enc" (HArgs [Variable (HVar "k"), Variable (HVar "m")]))
            :> Return (Variable (HVar "c"))
        )
    ]) Nothing

cpaDollaRand :: Library 
cpaDollaRand = 
    Lib "cpa$-rand" (Block [
        Rout Nothing "CTXT" (HSig ["m"])
        (
            Assign (HVar "c") (Variable (HVar "C"))
            :> Return (Variable (HVar "c"))
        )
    ]) Nothing