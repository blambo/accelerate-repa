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

import Text.PrettyPrint

-- | Used to compile and run an embedded array program using the Repa backend
--run :: Arrays a => Smart.Acc a -> String
--run = evalAcc . Smart.convertAcc

run :: Arrays a => Smart.Acc a -> String
run acc = show $
   headS $$ (nest 1 (evalAcc $ Smart.convertAcc acc))
         $$ tailS

headS :: Doc
{-# INLINE headS #-}
headS =
   text "import Data.Array.Repa as Repa" $+$
   text "import Data.Bits -- required for Prim ops" $+$
   text "import Data.Char -- required for Prim ops" $+$
   text "import Data.List (sortBy)  -- required for permute" $+$
   text "import Data.Ord  (compare) -- required for permute" $+$
   text "main = putStrLn $ show $"

tailS :: Doc
{-# INLINE tailS #-}
tailS = empty
