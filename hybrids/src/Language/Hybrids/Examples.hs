module Language.Hybrids.Examples where

import Data.BitString
import Language.Hybrids.AST
import System.IO
import System.Exit

otp_real :: BitWidth -> HTerm
otp_real size = 
    Routine "Enc" (HSig ["k", "m"]) (
        Assign (HVar "res") (Xor (Variable $ HVar "k") (Variable $ HVar "m"))
     :> Return (Variable $ HVar "res")
    )
 :> Gets size (HVar "key")
 :> Assign (HVar "message") (Literal $ encN size 100)
 :> Return (Call "Enc" (HArgs [ Variable $ HVar "key"
                              , Variable $ HVar "message"
                              ]))


example_otp :: IO ()
example_otp = do
    ctx <- emptyCtx
    case evalHTerm ctx (otp_real bits128) of
        Left e           -> do
            hPutStrLn stderr e
            exitFailure
        Right (_ctx, bs) -> do
            putStrLn (show bs)
            exitSuccess
