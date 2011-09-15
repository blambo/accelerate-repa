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
evalPrimConst _ = "PrimConst"

evalPrim :: PrimFun p -> String -> String

evalPrim (PrimAdd ty) s = "( (fst " ++ s ++ ") + (snd " ++ s ++ ") )"

evalPrim (PrimSub ty) s = "sub"

evalPrim (PrimMul ty) s = "mul"

evalPrim (PrimNeg ty) s = "neg"

evalPrim (PrimAbs ty) s = "abs"

evalPrim (PrimSig ty) s = "sig"

evalPrim (PrimQuot ty) s = "quot"

evalPrim (PrimRem ty) s = "rem"

evalPrim (PrimIDiv ty) s = "idiv"

evalPrim (PrimMod ty) s = "mod"

evalPrim (PrimBAnd ty) s = "bAnd"

evalPrim (PrimBOr ty) s = "bOr"

evalPrim (PrimBXor ty) s = "bXor"

evalPrim (PrimBNot ty) s = "bNot"

evalPrim (PrimBShiftL ty) s = "bShiftl"

evalPrim (PrimBShiftR ty) s = "bShiftr"

evalPrim (PrimBRotateL ty) s = "bRotatel"

evalPrim (PrimBRotateR ty) s = "bRotater"

evalPrim (PrimFDiv ty) s = "fdiv"

evalPrim (PrimRecip ty) s = "recip"

evalPrim (PrimSin ty) s = "sin"

evalPrim (PrimCos ty) s = "cos"

evalPrim (PrimTan ty) s = "tan"

evalPrim (PrimAsin ty) s = "asin"

evalPrim (PrimAcos ty) s = "acos"

evalPrim (PrimAtan ty) s = "atan"

evalPrim (PrimAsinh ty) s = "asinh"
evalPrim (PrimAcosh ty) s = "acosh"
evalPrim (PrimAtanh ty) s = "atanh"
evalPrim (PrimExpFloating ty) s = "expfloating"
evalPrim (PrimSqrt ty) s = "sqrt"
evalPrim (PrimLog ty) s = "log"
evalPrim (PrimFPow ty) s = "fpow"
evalPrim (PrimLogBase ty) s = "logbase"
evalPrim (PrimTruncate ta tb) s = "truncate"
evalPrim (PrimRound ta tb) s = "round"
evalPrim (PrimFloor ta tb) s = "floor"
evalPrim (PrimCeiling ta tb) s = "ceiling"
evalPrim (PrimAtan2 ty) s = "atan2"
evalPrim (PrimLt ty) s = "lt"
evalPrim (PrimGt ty) s = "gt"
evalPrim (PrimLtEq ty) s = "lteq"
evalPrim (PrimGtEq ty) s = "gteq"
evalPrim (PrimEq ty) s = "eq"
evalPrim (PrimNEq ty) s = "neq"
evalPrim (PrimMax ty) s = "max"
evalPrim (PrimMin ty) s = "min"
evalPrim (PrimLAnd) s = "lAnd"
evalPrim (PrimLOr) s = "lOr"
evalPrim (PrimLNot) s = "lNot"
evalPrim (PrimOrd) s = "ord"
evalPrim (PrimChr) s = "chr"
evalPrim (PrimBoolToInt) s = "booltoint"
evalPrim (PrimFromIntegral ta tb) s = "fromIntegral"
