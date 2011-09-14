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

evalPrim :: forall a. a -> String
evalPrim _ = "prim"

--evalPrim :: PrimFun (a->r) -> String
--evalPrim _ = "prim"

--evalPrim (PrimAdd ty) = "add"
{-
evalPrim (PrimSub ty) = "sub"

evalPrim (PrimMul ty) = "mul"

evalPrim (PrimNeg ty) = "neg"

evalPrim (PrimAbs ty) = "abs"

evalPrim (PrimSig ty) = "sig"

evalPrim (PrimQuot ty) = "quot"

evalPrim (PrimRem ty) = "rem"

evalPrim (PrimIDiv ty) = "idiv"

evalPrim (PrimMod ty) = "mod"

evalPrim (PrimBAnd ty) = "bAnd"

evalPrim (PrimBOr ty) = "bOr"

evalPrim (PrimBXor ty) = "bXor"

evalPrim (PrimBNot ty) = "bNot"

evalPrim (PrimBShiftL ty) = "bShiftl"

evalPrim (PrimBShiftR ty) = "bShiftr"

evalPrim (PrimBRotateL ty) = "bRotatel"

evalPrim (PrimBRotateR ty) = "bRotater"

evalPrim (PrimFDiv ty) = "fdiv"

evalPrim (PrimRecip ty) = "recip"

evalPrim (PrimSin ty) = "sin"

evalPrim (PrimCos ty) = "cos"

evalPrim (PrimTan ty) = "tan"

evalPrim (PrimAsin ty) = "asin"

evalPrim (PrimAcos ty) = "acos"

evalPrim (PrimAtan ty) = "atan"

evalPrim (PrimAsinh ty) = "asinh"
evalPrim (PrimAcosh ty) = "acosh"
evalPrim (PrimAtanh ty) = "atanh"
evalPrim (PrimExpFloating ty) = "expfloating"
evalPrim (PrimSqrt ty) = "sqrt"
evalPrim (PrimLog ty) = "log"
evalPrim (PrimFPow ty) = "fpow"
evalPrim (PrimLogBase ty) = "logbase"
evalPrim (PrimTruncate ta tb) = "truncate"
evalPrim (PrimRound ta tb) = "round"
evalPrim (PrimFloor ta tb) = "floor"
evalPrim (PrimCeiling ta tb) = "ceiling"
evalPrim (PrimAtan2 ty) = "atan2"
evalPrim (PrimLt ty) = "lt"
evalPrim (PrimGt ty) = "gt"
evalPrim (PrimLtEq ty) = "lteq"
evalPrim (PrimGtEq ty) = "gteq"
evalPrim (PrimEq ty) = "eq"
evalPrim (PrimNEq ty) = "neq"
evalPrim (PrimMax ty) = "max"
evalPrim (PrimMin ty) = "min"
evalPrim (PrimLAnd) = "lAnd"
evalPrim (PrimLOr) = "lOr"
evalPrim (PrimLNot) = "lNot"
evalPrim (PrimOrd) = "ord"
evalPrim (PrimChr) = "chr"
evalPrim (PrimBoolToInt) = "booltoint"
evalPrim (PrimFromIntegral ta tb) = "fromIntegral"
-}
