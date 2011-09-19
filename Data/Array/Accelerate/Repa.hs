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
--run :: Arrays a => Smart.Acc a -> String
--run = evalAcc . Smart.convertAcc

run :: Arrays a => Smart.Acc a -> String
run acc = headS ++ (evalAcc $ Smart.convertAcc acc) ++ tailS

headS :: String
{-# INLINE headS #-}
headS =
   "import Data.Array.Repa as Repa\n" ++
   "main = putStrLn $ show $ toList $ "

tailS :: String
{-# INLINE tailS #-}
tailS =
   "\n" ++
   "\n"
   {-"\n" ++
   "\n" ++
   "size :: (Shape sh) => sh -> Int\n" ++
   "size  Z     = 1\n" ++
   "size (h:.t) = t * (Main.size h)\n"-}
