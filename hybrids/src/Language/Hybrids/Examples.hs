module Language.Hybrids.Examples where

import Data.BitString
import System.IO
import System.Exit
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