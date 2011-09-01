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

evalFun :: Fun aenv t -> Val aenv -> t
evalFun f aenv = evalOpenFun f Empty aenv

evalOpenFun :: OpenFun env aenv t -> Val env -> Val aenv -> t
evalOpenFun (Body e) env aenv
   = evalOpenExp e env aenv
evalOpenFun (Lam f)  env aenv
   = \x -> evalOpenFun f (env `Push` Sugar.fromElt x) aenv
