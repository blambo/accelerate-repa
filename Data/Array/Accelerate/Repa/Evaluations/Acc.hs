{-# LANGUAGE CPP, GADTs, BangPatterns, TypeOperators, PatternGuards #-}
{-# LANGUAGE TypeFamilies, ScopedTypeVariables, FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
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
import Data.Array.Accelerate.Array.Sugar as Sugar

import Data.Array.Accelerate.Repa.Evaluations.Fun
import Data.Array.Accelerate.Repa.Evaluations.Exp
import Data.Array.Accelerate.Repa.RepaParsed

evalAcc :: Acc a -> String
evalAcc acc
 = parsedS
 where
   RepaParsed _ parsedS = evalOpenAcc acc Empty

-- | Unpacks AST by removing 'OpenAcc' shell
evalOpenAcc :: OpenAcc aenv a -> Val aenv -> RepaParsed a
evalOpenAcc (OpenAcc acc) = evalPreOpenAcc acc

-- | Traverses over AST
evalPreOpenAcc :: PreOpenAcc OpenAcc aenv a -> Val aenv -> RepaParsed a

evalPreOpenAcc (Let acc1 acc2) aenv
{- = "let " ++ arr1 ++ " in\n\t" ++
   (evalOpenAcc acc2
      (aenv `Push`  (error "Strict environment typing prevents new strings")))
 where
   RepaParsed arr1 arr1S = evalOpenAcc acc1 aenv
-}
 = error "let"

evalPreOpenAcc (Let2 _acc1 _acc2) _aenv
 = error "Let2"

evalPreOpenAcc (PairArrays _acc1 _acc2) _aenv
 = error "PairArrays"

evalPreOpenAcc (Avar _idx) _aenv
 = error "Avar"

evalPreOpenAcc (Apply (Alam (Abody _funAcc)) _acc) _aenv
 = error "Apply"
evalPreOpenAcc (Apply _afun _acc) _aenv
   = error "GHC pattern matching does not detect that this case is impossible"

evalPreOpenAcc (Acond _cond _acc1 _acc2) _aenv
 = error "Acond"

evalPreOpenAcc (Use _arr) _aenv
-- = error "Use"
 = RepaParsed (error "use") "use"

evalPreOpenAcc (Unit e) aenv
 = RepaParsed (error "Unit") expS
 where
   expS = evalExp e aenv
-- = error "unit"

evalPreOpenAcc (Reshape e acc) aenv
-- = "reshape (" ++ (evalExp e aenv)       ++ ") ("
--               ++ (evalOpenAcc acc aenv) ++ ")"
 = error "reshape"

evalPreOpenAcc (Generate sh f) aenv
 = error "Generate"
-- = "fromFunction (" ++ (evalExp sh aenv) ++ ") ("
--                    ++ (evalFun f aenv)  ++ ")"

evalPreOpenAcc (Replicate _sliceIndex _slix _acc) _aenv
 = error "Replicate"

evalPreOpenAcc (Index _sliceIndex _acc _slix) _aenv
 = error "Index"

evalPreOpenAcc (Map f acc) aenv
-- = error "Map"
 = RepaParsed (error "no map computation") ("map " ++ funS ++ " " ++ arrS)
 where
   RepaParsed _fun funS = evalFun     f   aenv
   RepaParsed _arr arrS = evalOpenAcc acc aenv

evalPreOpenAcc (ZipWith f acc1 acc2) aenv
 = error "ZipWith"
-- = "zipwith " ++ evalFun f aenv ++ " " ++ (evalOpenAcc acc1 aenv)
--                                ++ " " ++ (evalOpenAcc acc2 aenv)

evalPreOpenAcc (Fold f e acc) aenv
-- = "fold " ++ evalFun     f   aenv ++ " "
--           ++ evalExp e   aenv ++ " "
--           ++ evalOpenAcc acc aenv
 = error "fold"

evalPreOpenAcc (Fold1 _f _acc) _aenv
 = error "Fold1"

evalPreOpenAcc (FoldSeg _f _e _acc1 _acc2) _aenv
 = error "FoldSeg"

evalPreOpenAcc (Fold1Seg _f _acc1 _acc2) _aenv
 = error "Fold1Seg"

evalPreOpenAcc (Scanl _f _e _acc) _aenv
 = error "Scanl"

evalPreOpenAcc (Scanl' _f _e _acc) _aenv
 = error "Scanl'"

evalPreOpenAcc (Scanl1 _f _acc) _aenv
 = error "Scanl1"

evalPreOpenAcc (Scanr _f _e _acc) _aenv
 = error "Scanr"

evalPreOpenAcc (Scanr' _f _e _acc) _aenv
 = error "Scanr'"

evalPreOpenAcc (Scanr1 _f _acc) _aenv
 = error "Scanr1"

evalPreOpenAcc (Permute _f _dftAcc _p _acc) _aenv
 = error "Permute"

evalPreOpenAcc (Backpermute _e _p _acc) _aenv
 = error "Backpermute"

evalPreOpenAcc (Stencil _sten _bndy _acc) _aenv
 = error "Stencil"

evalPreOpenAcc (Stencil2 _sten _bndy1 _acc1 _bndy2 _acc2) _aenv
 = error "Stencil2"

evalPreOpenAcc _ _ = error "Not yet implemented"

-- | instance declaration for Arrays type class for allowing strings
--instance Arrays String where
--   arrays = ArraysRunit
