module Language.Hybrids.Examples where

import Data.BitString
import Data.Type.Nat
import Data.Type.Nat.Extra
import Language.Hybrids.AST
import System.IO
import System.Exit

otp_real :: forall n. FromBits n => Nat n -> HTerm
otp_real size = 
    Routine "Enc" (HSigArg "k" (HSigArg "m" HSigBase)) (
        Assign (HVar "res") (Xor (Variable $ HVar @n "k") (Variable $ HVar "m"))
     :> Return (Variable $ HVar @n "res")
    )
 :> Gets size (HVar @n "key")
 :> Assign (HVar "message") (Literal $ encN size 100)
 :> Return (Call "Enc" (HArgsArg (Variable $ HVar @n "key")
                       (HArgsArg (Variable $ HVar @n "message")
                       (HArgsBase :: HArgs n '[]))))


example_otp :: IO ()
example_otp = do
    ctx <- emptyCtx
    case evalHTerm ctx (otp_real nat128) of
        Left e           -> do
            hPutStrLn stderr e
            exitFailure
        Right (_ctx, bs) -> do
            putStrLn (show bs)
            exitSuccess
