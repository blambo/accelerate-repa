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
 = RepaParsed returnVars returnString
 where
   RepaParsed _   arr1S = evalOpenAcc acc1 aenv
   RepaParsed var arr2S = evalOpenAcc acc2 (aenv `Push` error "from let")

   returnString = case var of
                     VarUnit          -> "let x" ++ " = (" ++ arr1S ++ ") in\n\t"
                                                 ++ arr2S
                     VarTup vars curr -> "let " ++ (showVar curr) ++ " = (" ++ arr1S
                                                ++ ") in\n\t" ++ arr2S
   returnVars   = case var of
                     VarUnit          -> VarUnit
                     VarTup vars curr -> vars


evalPreOpenAcc (Let2 acc1 acc2) aenv
 = RepaParsed returnVars returnString
 where
   RepaParsed _arr1V arr1S = evalOpenAcc acc1 aenv
   RepaParsed arr2V arr2S = evalOpenAcc acc2 (aenv `Push` (error "let2,1") 
                                                   `Push` (error "let2,2"))

   var1 = case arr2V of
            VarUnit         -> "_"
            VarTup _vs curr -> (showVar curr)
   var2 = case arr2V of
            VarUnit         -> "_"
            VarTup vs _curr -> case vs of
                                 VarUnit         -> "_"
                                 VarTup vs' curr -> (showVar curr)
   returnVars = case arr2V of
            VarUnit      -> VarUnit
            VarTup vs _c -> case vs of
                              VarUnit       -> VarUnit
                              VarTup vs' _c -> vs'
   returnString = "let (" ++ var1 ++ ", " ++ var2 ++ ") = (" ++ arr1S
                          ++ ") in \n\t" ++ arr2S


--TODO: Need better handling of variables being passed from either side of tuple
evalPreOpenAcc (PairArrays acc1 acc2) aenv
 = RepaParsed arr1 $ "( (" ++ arr1S ++ "), (" ++ arr2S ++ ") )"
 where
   RepaParsed arr1 arr1S = evalOpenAcc acc1 aenv
   RepaParsed _arr2 arr2S = evalOpenAcc acc2 aenv

evalPreOpenAcc (Avar idx) aenv
 = RepaParsed allVars var
 where
   var    = "y" ++ varNum
   varNum = show $ getVarNum idx
   allVars = genVars idx

evalPreOpenAcc (Apply (Alam (Abody _funAcc)) _acc) _aenv
 = error "Apply"
evalPreOpenAcc (Apply _afun _acc) _aenv
   = error "GHC pattern matching does not detect that this case is impossible"

evalPreOpenAcc (Acond _cond _acc1 _acc2) _aenv
 = error "Acond"

evalPreOpenAcc (Use _arr) _aenv
-- = error "Use"
 = RepaParsed VarUnit "use"

evalPreOpenAcc (Unit e) aenv
 = RepaParsed VarUnit expS
 where
   expS = evalExp e aenv
-- = error "unit"

evalPreOpenAcc (Reshape e acc) aenv
-- = "reshape (" ++ (evalExp e aenv)       ++ ") ("
--               ++ (evalOpenAcc acc aenv) ++ ")"
 = error "reshape"

evalPreOpenAcc (Generate sh f) aenv
 = RepaParsed VarUnit returnString
-- = "fromFunction (" ++ (evalExp sh aenv) ++ ") ("
--                    ++ (evalFun f aenv)  ++ ")"
 where
   expS                   = evalExp sh aenv
   RepaParsed funVar funS = evalFun f aenv
   returnString           = "fromFunction (" ++ expS ++ ") (" ++ funS ++ ")"

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

getVarNum :: Idx env t -> Int
getVarNum ZeroIdx = 0
getVarNum (SuccIdx idx) = 1 + (getVarNum idx)

genVars :: Idx env t -> PossVar
genVars ZeroIdx = VarTup VarUnit 0
genVars (SuccIdx idx) = VarTup vs (1 + last)
                        where
                           vs@(VarTup _ last) = genVars idx
