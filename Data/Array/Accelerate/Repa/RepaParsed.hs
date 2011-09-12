-- |
-- Module     : Data.Array.Accelerate.Repa.RepaParsed
--
-- Maintainer : Ben Lambert-Smith <blambo+github@gmail.com>
--
-- This module defines a data type for use when parsing the Accelerate AST

module Data.Array.Accelerate.Repa.RepaParsed
   ( RepaParsed(..)
   , PossVar(..)
   , showVar
   )
   where

data RepaParsed a = RepaParsed PossVar String

data PossVar = VarUnit
             | VarTup  PossVar Int 

showVar :: Int -> String
showVar n = "y" ++ (show n)