-- {-# LANGUAGE GADTs #-}
-- |
-- Module     : Data.Array.Accelerate.Repa
--
-- Maintainer : Ben Lambert-Smith <blambo+github@gmail.com>
--
-- This module implements the Repa back-end for the accelerate EDSL
-- The current structure follows closely on 
-- Data.Array.Accelerate.Interpreter

module Data.Array.Accelerate.Repa
   ( Arrays
   , run
   )
   where

import Data.Array.Accelerate.AST
import qualified Data.Array.Accelerate.Smart as Smart

import Data.Array.Accelerate.Repa.Evaluations (evalAcc)

-- | Used to compile and run an embedded array program using the Repa backend
run :: Arrays a => Smart.Acc a -> a
run = evalAcc . Smart.convertAcc


