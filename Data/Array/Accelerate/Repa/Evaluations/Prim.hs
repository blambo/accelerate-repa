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

import Text.PrettyPrint

import Data.Array.Accelerate.Repa.Traverse

evalPrimConst :: PrimConst a -> Doc

evalPrimConst (PrimMinBound ty) = text "minBound"
evalPrimConst (PrimMaxBound ty) = text "maxBound"
evalPrimConst (PrimPi       ty) = text "pi"


evalPrim :: PrimFun p -> RepaExp -> Doc

evalPrim (PrimAdd ty) s             = binOp   (char '+')            s
evalPrim (PrimSub ty) s             = binOp   (char '-')            s
evalPrim (PrimMul ty) s             = binOp   (char '*')            s
evalPrim (PrimNeg ty) s             = unaryOp (text "negate")       s
evalPrim (PrimAbs ty) s             = unaryOp (text "abs")          s
evalPrim (PrimSig ty) s             = unaryOp (text "signum")       s
evalPrim (PrimQuot ty) s            = binOp   (text "`quot`")       s
evalPrim (PrimRem ty) s             = binOp   (text "`rem`")        s
evalPrim (PrimIDiv ty) s            = binOp   (text "`div`")        s
evalPrim (PrimMod ty) s             = binOp   (text "`mod`")        s
evalPrim (PrimBAnd ty) s            = binOp   (text ".&.")          s
evalPrim (PrimBOr ty) s             = binOp   (text ".|.")          s
evalPrim (PrimBXor ty) s            = binOp   (text "`xor`")        s
evalPrim (PrimBNot ty) s            = unaryOp (text "complement")   s
evalPrim (PrimBShiftL ty) s         = binOp   (text "`shiftL`")     s
evalPrim (PrimBShiftR ty) s         = binOp   (text "`shiftR`")     s
evalPrim (PrimBRotateL ty) s        = binOp   (text "`rotateL`")    s
evalPrim (PrimBRotateR ty) s        = binOp   (text "`rotateR`")    s
evalPrim (PrimFDiv ty) s            = binOp   (char '/')            s
evalPrim (PrimRecip ty) s           = unaryOp (text "recip")        s
evalPrim (PrimSin ty) s             = unaryOp (text "sin")          s
evalPrim (PrimCos ty) s             = unaryOp (text "cos")          s
evalPrim (PrimTan ty) s             = unaryOp (text "tan")          s
evalPrim (PrimAsin ty) s            = unaryOp (text "asin")         s
evalPrim (PrimAcos ty) s            = unaryOp (text "acos")         s
evalPrim (PrimAtan ty) s            = unaryOp (text "atan")         s
evalPrim (PrimAsinh ty) s           = unaryOp (text "asinh")        s
evalPrim (PrimAcosh ty) s           = unaryOp (text "acosh")        s
evalPrim (PrimAtanh ty) s           = unaryOp (text "atanh")        s
evalPrim (PrimExpFloating ty) s     = unaryOp (text "exp")          s
evalPrim (PrimSqrt ty) s            = unaryOp (text "sqrt")         s
evalPrim (PrimLog ty) s             = unaryOp (text "log")          s
evalPrim (PrimFPow ty) s            = binOp   (text "**")           s
evalPrim (PrimLogBase ty) s         = binOp   (text "`logbase`")    s
evalPrim (PrimTruncate ta tb) s     = unaryOp (text "truncate")     s
evalPrim (PrimRound ta tb) s        = unaryOp (text "round")        s
evalPrim (PrimFloor ta tb) s        = unaryOp (text "floor")        s
evalPrim (PrimCeiling ta tb) s      = unaryOp (text "ceiling")      s
evalPrim (PrimAtan2 ty) s           = unaryOp (text "atan2")        s
evalPrim (PrimLt ty) s              = binOp   (char '<')            s
evalPrim (PrimGt ty) s              = binOp   (char '>')            s
evalPrim (PrimLtEq ty) s            = binOp   (text "<=")           s
evalPrim (PrimGtEq ty) s            = binOp   (text ">=")           s
evalPrim (PrimEq ty) s              = binOp   (text "==")           s
evalPrim (PrimNEq ty) s             = binOp   (text "/=")           s
evalPrim (PrimMax ty) s             = binOp   (text "`max`")        s
evalPrim (PrimMin ty) s             = binOp   (text "`min`")        s
evalPrim (PrimLAnd) s               = binOp   (text "&&")           s
evalPrim (PrimLOr) s                = binOp   (text "||")           s
evalPrim (PrimLNot) s               = unaryOp (text "not")          s
evalPrim (PrimOrd) s                = unaryOp (text "ord")          s
evalPrim (PrimChr) s                = unaryOp (text "chr")          s
evalPrim (PrimBoolToInt) s          = unaryOp (text "fromEnum")     s
evalPrim (PrimFromIntegral ta tb) s = unaryOp (text "fromIntegral") s


fstS :: Doc
fstS = text "fst"

sndS :: Doc
sndS = text "snd"

binOp :: Doc -> RepaExp -> Doc
binOp op (RepaTuple (a:b:[]))
   = parens a <+> op <+> parens b
binOp _   _
   = error "Binary operation not given corrent number of operands"


unaryOp :: Doc -> RepaExp -> Doc
unaryOp op e = op <+> (toDoc e)
