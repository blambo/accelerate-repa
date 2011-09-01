{-# LANGUAGE GADTs #-}
-- |
-- Module     : Data.Array.Accelerate.Repa.Evaluations.Acc
--
-- Maintainer : Ben Lambert-Smith <blambo+github@gmail.com>
--
-- Defines evaluation of Accelerate collective array computations,
--  borrowed in part from the Accelerate Interpreter module

module Data.Array.Accelerate.Repa.Evaluations.Acc
   ( evalAcc
   )
   where

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Repa.Evaluations.Fun
import Data.Array.Accelerate.Repa.Evaluations.Exp

evalAcc :: Acc a -> IO ()
evalAcc acc = evalOpenAcc acc Empty

-- | Unpacks AST by removing 'OpenAcc' shell
evalOpenAcc :: OpenAcc aenv a -> Val aenv -> IO ()
evalOpenAcc (OpenAcc acc) = evalPreOpenAcc acc

-- | Traverses over AST
evalPreOpenAcc :: PreOpenAcc OpenAcc aenv a -> Val aenv -> IO ()

evalPreOpenAcc (Let acc1 acc2) aenv
 = do
      print "Let {"
      evalOpenAcc acc1 aenv
      --evalOpenAcc acc2 aenv
      print "}"

evalPreOpenAcc (Let2 acc1 acc2) aenv
 = do
      print "Let2 {"
      evalOpenAcc acc1 aenv
      --evalOpenAcc acc2 aenv
      print "}"

evalPreOpenAcc (PairArrays acc1 acc2) aenv
 = do
      print "PairArrays {"
      evalOpenAcc acc1 aenv
      evalOpenAcc acc2 aenv
      print "}"

evalPreOpenAcc (Avar _idx) _aenv
 = do
      print "Avar {"
      print "}"

evalPreOpenAcc (Apply (Alam (Abody funAcc)) acc) aenv
 = do
      print "Apply {"
      evalOpenAcc acc aenv
      --evalOpenAcc funAcc aenv
      print "}"
evalPreOpenAcc (Apply _afun _acc) _aenv
   = error "GHC pattern matching does not detect that this case is impossible"

evalPreOpenAcc (Acond cond acc1 acc2) aenv
 = do
      print "Acond {"
      --evalOpenAcc cond aenv
      evalOpenAcc acc1 aenv
      evalOpenAcc acc2 aenv
      print "}"

evalPreOpenAcc (Use _arr) _aenv
 = do
      print "Use {"
      print "}"

evalPreOpenAcc (Unit e) aenv
 = do
      print "Unit {"
      print $ evalExp e aenv
      print "}"

evalPreOpenAcc (Reshape _e acc) aenv
 = do
      print "Reshape {"
      evalOpenAcc acc aenv
      print "}"

evalPreOpenAcc (Generate _sh _f) _aenv
 = do
      print "Generate {"
      print "}"

evalPreOpenAcc (Replicate _sliceIndex _slix acc) aenv
 = do
      print "Replicate {"
      evalOpenAcc acc aenv
      print "}"

evalPreOpenAcc (Index _sliceIndex acc _slix) aenv
 = do
      print "Index {"
      evalOpenAcc acc aenv
      print "}"

evalPreOpenAcc (Map f acc) aenv
 = do
      print "Map {"
      --evalFun f aenv
      evalOpenAcc acc aenv
      print "}"

evalPreOpenAcc (ZipWith _f acc1 acc2) aenv
 = do
      print "ZipWith {"
      evalOpenAcc acc1 aenv
      evalOpenAcc acc2 aenv
      print "}"

evalPreOpenAcc (Fold _f _e acc) aenv
 = do
      print "Fold {"
      evalOpenAcc acc aenv
      print "}"

evalPreOpenAcc (Fold1 _f acc) aenv
 = do
      print "Fold1 {"
      evalOpenAcc acc aenv
      print "}"

evalPreOpenAcc (FoldSeg _f _e acc1 acc2) aenv
 = do
      print "FoldSeg {"
      evalOpenAcc acc1 aenv
      evalOpenAcc acc2 aenv
      print "}"

evalPreOpenAcc (Fold1Seg _f acc1 acc2) aenv
 = do
      print "Fold1Seg {"
      evalOpenAcc acc1 aenv
      evalOpenAcc acc2 aenv
      print "}"

evalPreOpenAcc (Scanl _f _e acc) aenv
 = do
      print "Scanl {"
      evalOpenAcc acc aenv
      print "}"

evalPreOpenAcc (Scanl' _f _e acc) aenv
 = do
      print "Scanl' {"
      evalOpenAcc acc aenv
      print "}"

evalPreOpenAcc (Scanl1 _f acc) aenv
 = do
      print "Scanl1 {"
      evalOpenAcc acc aenv
      print "}"

evalPreOpenAcc (Scanr _f _e acc) aenv
 = do
      print "Scanr {"
      evalOpenAcc acc aenv
      print "}"

evalPreOpenAcc (Scanr' _f _e acc) aenv
 = do
      print "Scanr' {"
      evalOpenAcc acc aenv
      print "}"

evalPreOpenAcc (Scanr1 _f acc) aenv
 = do
      print "Scanr1 {"
      evalOpenAcc acc aenv
      print "}"

evalPreOpenAcc (Permute _f dftAcc _p acc) aenv
 = do
      print "Permute {"
      evalOpenAcc dftAcc aenv
      evalOpenAcc acc aenv
      print "}"

evalPreOpenAcc (Backpermute _e _p acc) aenv
 = do
      print "Backpermute {"
      evalOpenAcc acc aenv
      print "}"

evalPreOpenAcc (Stencil _sten _bndy acc) aenv
 = do
      print "Stencil {"
      evalOpenAcc acc aenv
      print "}"

evalPreOpenAcc (Stencil2 _sten _bndy1 acc1 _bndy2 acc2) aenv
 = do
      print "Stencil2 {"
      evalOpenAcc acc1 aenv
      evalOpenAcc acc2 aenv
      print "}"

evalPreOpenAcc _ _ = error "Not yet implemented"
