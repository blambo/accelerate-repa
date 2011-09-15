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
   RepaParsed parsedS = evalOpenAcc acc 0 Empty

-- | Unpacks AST by removing 'OpenAcc' shell
evalOpenAcc :: OpenAcc aenv a -> Int -> Val aenv -> RepaParsed a
evalOpenAcc (OpenAcc acc) = evalPreOpenAcc acc

-- | Traverses over AST
evalPreOpenAcc :: PreOpenAcc OpenAcc aenv a -> Int -> Val aenv -> RepaParsed a

evalPreOpenAcc (Let acc1 acc2) letLevel aenv
 = RepaParsed returnString
 where
   RepaParsed arr1S = evalOpenAcc acc1 letLevel      aenv
   RepaParsed arr2S = evalOpenAcc acc2 (letLevel+1) (aenv `Push` error "from let")

   returnString = "let y" ++ (show letLevel) ++ " = (" ++ arr1S ++ ") in " ++ arr2S


evalPreOpenAcc (Let2 acc1 acc2) letLevel aenv
 = RepaParsed returnString
 where
   RepaParsed arr1S = evalOpenAcc acc1 letLevel aenv
   RepaParsed arr2S = evalOpenAcc acc2 (letLevel+2) (aenv `Push` (error "let2,1") 
                                                          `Push` (error "let2,2"))

   var1 = "y" ++ (show letLevel)
   var2 = "y" ++ (show (letLevel + 1))
   returnString = "let (" ++ var1 ++ ", " ++ var2 ++ ") = (" ++ arr1S
                          ++ ") in " ++ arr2S


evalPreOpenAcc (PairArrays acc1 acc2) letLevel aenv
 = RepaParsed returnString
 where
   returnString     = "( (" ++ arr1S ++ "), (" ++ arr2S ++ ") )"
   RepaParsed arr1S = evalOpenAcc acc1 letLevel aenv
   RepaParsed arr2S = evalOpenAcc acc2 letLevel aenv


evalPreOpenAcc (Avar idx) letLevel _aenv
 = RepaParsed var
 where
   var    = "y" ++ show (letLevel - varNum - 1)
   varNum = getVarNum idx


evalPreOpenAcc (Apply (Alam (Abody _funAcc)) _acc) _letLevel _aenv
 = error "Apply"
evalPreOpenAcc (Apply _afun _acc) _letLevel _aenv
   = error "GHC pattern matching does not detect that this case is impossible"


evalPreOpenAcc (Acond _cond _acc1 _acc2) _letLevel _aenv
 = error "Acond"

-- TODO
evalPreOpenAcc (Use arr) _letLevel _aenv
 = RepaParsed "use"


evalPreOpenAcc (Unit e) _letLevel aenv
 = RepaParsed expS
 where
   expS = evalExp e aenv


evalPreOpenAcc (Reshape e acc) letLevel aenv
 = RepaParsed returnString
 where
   RepaParsed arrS = evalOpenAcc acc letLevel aenv
   returnString = "reshape (" ++ (evalExp e aenv) ++ ") (" ++ arrS ++ ")"


evalPreOpenAcc (Generate sh f) _letLevel aenv
 = RepaParsed returnString
 where
   expS            = evalExp sh aenv
   RepaParsed funS = evalFun f aenv
   returnString    = "fromFunction (" ++ expS ++ ") (" ++ funS ++ ")"


evalPreOpenAcc (Replicate _sliceIndex _slix _acc) _letLevel _aenv
 = error "Replicate"


evalPreOpenAcc (Index _sliceIndex _acc _slix) _letLevel _aenv
 = error "Index"


evalPreOpenAcc (Map f acc) letLevel aenv
 = RepaParsed ("map (" ++ funS ++ ") (" ++ arrS ++ ")")
 where
   RepaParsed funS = evalFun     f   aenv
   RepaParsed arrS = evalOpenAcc acc letLevel aenv


evalPreOpenAcc (ZipWith f acc1 acc2) letLevel aenv
 = RepaParsed s
 where
   s = "zipwith (" ++ funS ++ ") (" ++ arr1S ++ ") (" ++ arr2S ++ ")"
   RepaParsed funS  = evalFun f aenv
   RepaParsed arr1S = (evalOpenAcc acc1 letLevel aenv) 
   RepaParsed arr2S = (evalOpenAcc acc2 letLevel aenv) 


evalPreOpenAcc (Fold f e acc) letLevel aenv
 = RepaParsed returnS
 where
   returnS         = "fold (" ++ funS ++ ") (" ++ expS ++ ") (" ++ arrS ++ ")"
   RepaParsed funS = evalFun     f            aenv
   expS            = evalExp     e            aenv
   RepaParsed arrS = evalOpenAcc acc letLevel aenv


evalPreOpenAcc (Fold1 _f _acc) _letLevel _aenv
 = error "Fold1"


evalPreOpenAcc (FoldSeg _f _e _acc1 _acc2) _letLevel _aenv
 = error "FoldSeg"


evalPreOpenAcc (Fold1Seg _f _acc1 _acc2) _letLevel _aenv
 = error "Fold1Seg"


evalPreOpenAcc (Scanl _f _e _acc) _letLevel _aenv
 = error "Scanl"


evalPreOpenAcc (Scanl' _f _e _acc) _letLevel _aenv
 = error "Scanl'"


evalPreOpenAcc (Scanl1 _f _acc) _letLevel _aenv
 = error "Scanl1"


evalPreOpenAcc (Scanr _f _e _acc) _letLevel _aenv
 = error "Scanr"


evalPreOpenAcc (Scanr' _f _e _acc) _letLevel _aenv
 = error "Scanr'"


evalPreOpenAcc (Scanr1 _f _acc) _letLevel _aenv
 = error "Scanr1"


evalPreOpenAcc (Permute _f _dftAcc _p _acc) _letLevel _aenv
 = error "Permute"


evalPreOpenAcc (Backpermute _e _p _acc) _letLevel _aenv
 = error "Backpermute"


evalPreOpenAcc (Stencil _sten _bndy _acc) _letLevel _aenv
 = error "Stencil"


evalPreOpenAcc (Stencil2 _sten _bndy1 _acc1 _bndy2 _acc2) _letLevel _aenv
 = error "Stencil2"


evalPreOpenAcc _ _ _ = error "Not yet implemented"



getVarNum :: Idx env t -> Int
getVarNum ZeroIdx = 0
getVarNum (SuccIdx idx) = 1 + (getVarNum idx)

genVars :: Idx env t -> PossVar
genVars ZeroIdx = VarTup VarUnit 0
genVars (SuccIdx idx) = VarTup vs (1 + last)
                        where
                           vs@(VarTup _ last) = genVars idx
