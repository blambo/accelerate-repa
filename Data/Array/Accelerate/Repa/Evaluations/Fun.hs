{-# LANGUAGE GADTs #-}
-- |
-- Module     : Data.Array.Accelerate.Repa.Evaluations.Fun
--
-- Maintainer : Ben Lambert-Smith <blambo+github@gmail.com>
--
-- Defines evaluation of Accelerate function expressions,
--  borrowed in part from the Accelerate Interpreter module

module Data.Array.Accelerate.Repa.Evaluations.Fun
   ( evalFun
   )
   where

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Array.Sugar as Sugar

import Data.Array.Accelerate.Repa.Evaluations.Exp
import Data.Array.Accelerate.Repa.RepaParsed

evalFun :: Fun aenv t -> Val aenv -> RepaParsed t
evalFun f aenv = evalOpenFun f 0 Empty aenv

evalOpenFun :: OpenFun env aenv t -> Int -> Val env -> Val aenv -> RepaParsed t
evalOpenFun (Body e) lamLevel env aenv
 = RepaParsed $ evalOpenExp e lamLevel env aenv
evalOpenFun (Lam f)  lamLevel env aenv
 = RepaParsed ("\\" ++ varName ++ " -> " ++ funS)
 where
   RepaParsed funS = evalOpenFun f (lamLevel+1) (env `Push` (error "Lam")) aenv
   varName = "x" ++ (show lamLevel)
