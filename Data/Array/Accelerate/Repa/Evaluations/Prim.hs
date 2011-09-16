{-# LANGUAGE GADTs, ScopedTypeVariables #-}
-- |
-- Module     : Data.Array.Accelerate.Repa.Evaluations.Prim
--
-- Maintainer : Ben Lambert-Smith <blambo+github@gmail.com>
--
-- Defines evaluation of primitives

module Data.Array.Accelerate.Repa.Evaluations.Prim
   ( evalPrim
   , evalPrimConst
   )
   where

import Data.Array.Accelerate.AST

evalPrimConst :: PrimConst a -> String

evalPrimConst (PrimMinBound ty) = "minBound"
evalPrimConst (PrimMaxBound ty) = "maxBound"
evalPrimConst (PrimPi       ty) = "pi"


evalPrim :: PrimFun p -> String -> String

evalPrim (PrimAdd ty) s         = "( (fst " ++ s ++ ") + (snd " ++ s ++ ") )"
evalPrim (PrimSub ty) s         = "( (fst " ++ s ++ ") - (snd " ++ s ++ ") )"
evalPrim (PrimMul ty) s         = "( (fst " ++ s ++ ") * (snd " ++ s ++ ") )"
evalPrim (PrimNeg ty) s         = "(negate " ++ s ++ ")"
evalPrim (PrimAbs ty) s         = "(abs " ++ s ++ ")"
evalPrim (PrimSig ty) s         = "(signum " ++ s ++ ")"
evalPrim (PrimQuot ty) s        = "((fst " ++ s ++ ") `quot` (snd " ++ s ++ "))"
evalPrim (PrimRem ty) s         = "( (fst " ++ s ++ ") `rem` (snd " ++ s ++ "))"
evalPrim (PrimIDiv ty) s        = "( (fst " ++ s ++ ") `div` (snd " ++ s ++ "))"
evalPrim (PrimMod ty) s         = "( (fst " ++ s ++ ") `mod` (snd " ++ s ++ "))"
evalPrim (PrimBAnd ty) s        = "( (fst " ++ s ++ ") .&. (snd " ++ s ++ ") )"
evalPrim (PrimBOr ty) s         = "( (fst " ++ s ++ ") .|. (snd " ++ s ++ ") )"
evalPrim (PrimBXor ty) s        = "((fst " ++ s ++ ") `xor` (snd " ++ s ++ "))"
evalPrim (PrimBNot ty) s        = "(complement " ++ s ++ ")"
evalPrim (PrimBShiftL ty) s     = "(shiftL (fst " ++ s ++ ") (snd " ++ s ++ "))"
evalPrim (PrimBShiftR ty) s     = "(shiftR (fst " ++ s ++ ") (snd " ++ s ++ "))"
evalPrim (PrimBRotateL ty) s    = "(rotateL (fst " ++ s ++ ") (snd " ++ s++ "))"
evalPrim (PrimBRotateR ty) s    = "(rotateR (fst " ++ s ++ ") (snd " ++ s++ "))"
evalPrim (PrimFDiv ty) s        = "((fst " ++ s ++ ") / (snd " ++ s ++ ") )"
evalPrim (PrimRecip ty) s       = "(recip " ++ s ++ ")"
evalPrim (PrimSin ty) s         = "(sin " ++ s ++ ")"
evalPrim (PrimCos ty) s         = "(cos " ++ s ++ ")"
evalPrim (PrimTan ty) s         = "(tan " ++ s ++ ")"
evalPrim (PrimAsin ty) s        = "(asin " ++ s ++ ")"
evalPrim (PrimAcos ty) s        = "(acos " ++ s ++ ")"
evalPrim (PrimAtan ty) s        = "(atan " ++ s ++ ")"
evalPrim (PrimAsinh ty) s       = "(asinh " ++ s ++ ")"
evalPrim (PrimAcosh ty) s       = "(acosh " ++ s ++ ")"
evalPrim (PrimAtanh ty) s       = "(atanh " ++ s ++ ")"
evalPrim (PrimExpFloating ty) s = "(exp " ++ s ++ ")"
evalPrim (PrimSqrt ty) s        = "(sqrt " ++ s ++ ")"
evalPrim (PrimLog ty) s         = "(log " ++ s ++ ")"
evalPrim (PrimFPow ty) s        = "( (fst " ++ s ++ ") ** (snd " ++ s ++ ") )"
evalPrim (PrimLogBase ty) s     = "(logbase (fst " ++ s ++ ") (snd " ++ s ++"))"
evalPrim (PrimTruncate ta tb) s = "(truncate " ++ s ++ ")"
evalPrim (PrimRound ta tb) s    = "(round " ++ s ++ ")"
evalPrim (PrimFloor ta tb) s    = "(floor " ++ s ++ ")"
evalPrim (PrimCeiling ta tb) s  = "(ceiling " ++ s ++ ")"
evalPrim (PrimAtan2 ty) s       = "(atan2 " ++ s ++ ")"
evalPrim (PrimLt ty) s          = "( (fst " ++ s ++ ") < (snd " ++ s ++ ") )"
evalPrim (PrimGt ty) s          = "( (fst " ++ s ++ ") > (snd " ++ s ++ ") )"
evalPrim (PrimLtEq ty) s        = "( (fst " ++ s ++ ") <= (snd " ++ s ++ ") )"
evalPrim (PrimGtEq ty) s        = "( (fst " ++ s ++ ") >= (snd " ++ s ++ ") )"
evalPrim (PrimEq ty) s          = "( (fst " ++ s ++ ") == (snd " ++ s ++ ") )"
evalPrim (PrimNEq ty) s         = "( (fst " ++ s ++ ") /= (snd " ++ s ++ ") )"
evalPrim (PrimMax ty) s         = "((fst " ++ s ++ ") `max` (snd " ++ s ++ "))"
evalPrim (PrimMin ty) s         = "((fst " ++ s ++ ") `min` (snd " ++ s ++ "))"
evalPrim (PrimLAnd) s           = "( (fst " ++ s ++ ") && (snd " ++ s ++ ") )"
evalPrim (PrimLOr) s            = "( (fst " ++ s ++ ") || (snd " ++ s ++ ") )"
evalPrim (PrimLNot) s           = "(not " ++ s ++ ")"
evalPrim (PrimOrd) s            = "(ord " ++ s ++ ")"
evalPrim (PrimChr) s            = "(chr " ++ s ++ ")"
evalPrim (PrimBoolToInt) s      = "(fromEnum " ++ s ++ ")"
evalPrim (PrimFromIntegral ta tb) s = "(fromIntegral " ++ s ++ ")"
