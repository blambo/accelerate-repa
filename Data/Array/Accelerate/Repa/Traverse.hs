-- |
-- Module     : Data.Array.Accelerate.Repa.Traverse
--
-- Maintainer : Ben Lambert-Smith <blambo+github@gmail.com>
--
-- This module defines data type for use when traversing the Accelerate AST

module Data.Array.Accelerate.Repa.Traverse
   ( RepaAcc(..)
   , RepaExp(..)
   , toDoc
   )
   where

import Text.PrettyPrint

data RepaAcc = RepaAcc Doc

data RepaExp = RepaExp    Doc
             | RepaTuple [Doc]

toDoc :: RepaExp -> Doc
{-# INLINE toDoc #-}
toDoc (RepaExp d)    = d
toDoc (RepaTuple ds) = parens $ printTuple ds

printTuple :: [Doc] -> Doc
printTuple [] = empty
printTuple (d:ds) = case (isEmpty rest) of
                        True  -> d
                        False -> d <> comma <+> rest
                  where
                     rest = printTuple ds
