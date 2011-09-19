{-# LANGUAGE CPP, GADTs, BangPatterns, TypeOperators, PatternGuards #-}
{-# LANGUAGE TypeFamilies, ScopedTypeVariables, FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-- |
-- Module     : Data.Array.Accelerate.Repa.Evaluations
--
-- Maintainer : Ben Lambert-Smith <blambo+github@gmail.com>
--
-- This module re-exports a number of modules for evaluating Accelerate AST nodes

module Data.Array.Accelerate.Repa.Evaluations
   ( evalAcc
   )
   where

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Array.Sugar as Sugar
import Data.Array.Accelerate.Tuple

import Data.Array.Accelerate.Repa.Evaluations.Prim
import Data.Array.Accelerate.Repa.RepaParsed


---------------
-- ACC NODES --
---------------

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


evalPreOpenAcc (Acond cond acc1 acc2) letLevel aenv
 = RepaParsed returnString
 where
   returnString = "if (" ++ expS ++ ") then (" ++ arr1S ++ ") else (" ++ arr2S ++ ")"
   expS             = evalExp cond letLevel aenv
   RepaParsed arr1S = evalOpenAcc acc1 letLevel aenv
   RepaParsed arr2S = evalOpenAcc acc2 letLevel aenv


-- TODO
evalPreOpenAcc (Use arr) _letLevel _aenv
 = RepaParsed "use"


evalPreOpenAcc (Unit e) letLevel aenv
 = RepaParsed expS
 where
   expS = evalExp e letLevel aenv


evalPreOpenAcc (Reshape e acc) letLevel aenv
 = RepaParsed returnString
 where
   RepaParsed arrS = evalOpenAcc acc letLevel aenv
   returnString = "reshape (" ++ (evalExp e letLevel aenv) ++ ") (" ++ arrS ++ ")"


evalPreOpenAcc (Generate sh f) letLevel aenv
 = RepaParsed returnString
 where
   expS            = evalExp sh letLevel aenv
   RepaParsed funS = evalFun f letLevel aenv
   returnString    = "fromFunction (" ++ expS ++ ") (" ++ funS ++ ")"

--TODO
evalPreOpenAcc (Replicate _sliceIndex _slix _acc) _letLevel _aenv
 = error "Replicate"

--TODO
evalPreOpenAcc (Index _sliceIndex _acc _slix) _letLevel _aenv
 = error "Index"


evalPreOpenAcc (Map f acc) letLevel aenv
 = RepaParsed ("map (" ++ funS ++ ") (" ++ arrS ++ ")")
 where
   RepaParsed funS = evalFun     f   letLevel aenv
   RepaParsed arrS = evalOpenAcc acc letLevel aenv


evalPreOpenAcc (ZipWith f acc1 acc2) letLevel aenv
 = RepaParsed s
 where
   s = "zipwith (" ++ funS ++ ") (" ++ arr1S ++ ") (" ++ arr2S ++ ")"
   RepaParsed funS  = evalFun f letLevel aenv
   RepaParsed arr1S = (evalOpenAcc acc1 letLevel aenv) 
   RepaParsed arr2S = (evalOpenAcc acc2 letLevel aenv) 


evalPreOpenAcc (Fold f e acc) letLevel aenv
 = RepaParsed returnS
 where
   returnS         = "fold (" ++ funS ++ ") (" ++ expS ++ ") (" ++ arrS ++ ")"
   RepaParsed funS = evalFun     f   letLevel aenv
   expS            = evalExp     e   letLevel aenv
   RepaParsed arrS = evalOpenAcc acc letLevel aenv

--TODO
evalPreOpenAcc (Fold1 _f _acc) _letLevel _aenv
 = error "Fold1"

--TODO
evalPreOpenAcc (FoldSeg _f _e _acc1 _acc2) _letLevel _aenv
 = error "FoldSeg"

--TODO
evalPreOpenAcc (Fold1Seg _f _acc1 _acc2) _letLevel _aenv
 = error "Fold1Seg"


evalPreOpenAcc (Scanl f e acc) letLevel aenv
 = RepaParsed returnString
 where
   RepaParsed funS = evalFun     f   letLevel aenv
   RepaParsed arrS = evalOpenAcc acc letLevel aenv
   expS            = evalExp     e   letLevel aenv

   returnString    = "traverse (" ++ arrS ++ ")" ++ " (\\(Z:.i) -> (Z:.(i+1))) "
                   ++ "(let newVal (origVal) (Z:.pos) "
                   ++ "| pos == 0 = " ++ expS ++ " ; "
                   ++ "| otherwise = " ++ "(" ++ funS ++ ")" ++
                                             "(newVal origVal (Z:.(pos-1))) " ++
                                             "(origVal (Z:.(pos-1)))"


--TODO
evalPreOpenAcc (Scanl' _f _e _acc) _letLevel _aenv
 = error "Scanl'"

--TODO
evalPreOpenAcc (Scanl1 _f _acc) _letLevel _aenv
 = error "Scanl1"

--TODO
evalPreOpenAcc (Scanr _f _e _acc) _letLevel _aenv
 = error "Scanr"

--TODO
evalPreOpenAcc (Scanr' _f _e _acc) _letLevel _aenv
 = error "Scanr'"

--TODO
evalPreOpenAcc (Scanr1 _f _acc) _letLevel _aenv
 = error "Scanr1"

--TODO
evalPreOpenAcc (Permute _f _dftAcc _p _acc) _letLevel _aenv
 = error "Permute"

--TODO
evalPreOpenAcc (Backpermute _e _p _acc) _letLevel _aenv
 = error "Backpermute"

--TODO
evalPreOpenAcc (Stencil _sten _bndy _acc) _letLevel _aenv
 = error "Stencil"

--TODO
evalPreOpenAcc (Stencil2 _sten _bndy1 _acc1 _bndy2 _acc2) _letLevel _aenv
 = error "Stencil2"


evalPreOpenAcc _ _ _ = error "Not yet implemented"

--------------------
-- FUNCTION NODES --
--------------------

evalFun :: Fun aenv t -> Int-> Val aenv -> RepaParsed t
evalFun f letL aenv = evalOpenFun f 0 letL Empty aenv

evalOpenFun :: OpenFun env aenv t -> Int -> Int -> Val env -> Val aenv -> RepaParsed t
evalOpenFun (Body e) lamL letL env aenv
 = RepaParsed $ evalOpenExp e lamL letL env aenv
evalOpenFun (Lam f)  lamL letL env aenv
 = RepaParsed ("\\" ++ varName ++ " -> " ++ funS)
 where
   RepaParsed funS = evalOpenFun f (lamL+1) letL (env `Push` (error "Lam")) aenv
   varName = "x" ++ (show lamL)

----------------------
-- EXPRESSION NODES --
----------------------

-- Evaluate an open expression
evalOpenExp :: forall a env aenv .
               OpenExp env aenv a -> Int -> Int -> Val env -> Val aenv -> String

evalOpenExp (Var idx) lamL letL env _
   = "x" ++ (show varNum)
   where
      varNum = lamL - (getVarNum idx) - 1

evalOpenExp (Const c) _ _ _ _
   = show ((Sugar.toElt c) :: a)

evalOpenExp (Tuple tup) lamL letL env aenv 
   = evalTuple tup lamL letL env aenv

--TODO
evalOpenExp (Prj idx e) _lamL _letL env aenv 
   = "Prj"

evalOpenExp IndexNil _ _ _ _
   = "Z"

evalOpenExp (IndexCons sh i) lamL letL env aenv 
   = (evalOpenExp sh lamL letL env aenv) ++ " :. ("
     ++ (evalOpenExp i lamL letL env aenv) ++ suffix
   where
      suffix = case i of
                  Const _   -> " :: Int)"
                  otherwise -> ")"

evalOpenExp (IndexHead ix) lamL letL env aenv 
   = "case (" ++ expS ++ ") of _:.h -> h"
   where
      expS = evalOpenExp ix lamL letL env aenv

evalOpenExp (IndexTail ix) lamL letL env aenv 
   = "case (" ++ expS ++ ") of t:._ -> t"
   where
      expS = evalOpenExp ix lamL letL env aenv

evalOpenExp (IndexAny) _ _ _ _
   = "Any"

evalOpenExp (Cond c t e) lamL letL env aenv 
   = "if (" ++ condS ++ ") then (" ++ exp1S ++ ") else (" ++ exp2S ++ ")"
   where
      condS = evalOpenExp c lamL letL env aenv
      exp1S = evalOpenExp t lamL letL env aenv
      exp2S = evalOpenExp e lamL letL env aenv

evalOpenExp (PrimConst c) _ _ _ _
   = evalPrimConst c

evalOpenExp (PrimApp p arg) lamL letL env aenv 
   = evalPrim p argS
   where
      argS = evalOpenExp arg lamL letL env aenv

--TODO
evalOpenExp (IndexScalar acc ix) lamL letL env aenv 
   = "IndexScalar"

--TODO
evalOpenExp (Shape acc) _lamL _letL _env _aenv 
   = "Shape"

evalOpenExp (Size acc) _lamL letL _env aenv 
   = "Size"
   where
      RepaParsed arrS = evalOpenAcc acc letL aenv

-- Evaluate a closed expression
--
evalExp :: PreExp OpenAcc aenv t -> Int -> Val aenv -> String
evalExp e letL aenv = evalOpenExp e 0 letL Empty aenv

------------
-- TUPLES --
------------

evalTuple :: Tuple (OpenExp env aenv) t -> Int -> Int -> Val env -> Val aenv -> String
evalTuple tup lamL letL env aenv = "(" ++ evalTuple' tup lamL letL env aenv ++ ")"

evalTuple' :: Tuple (OpenExp env aenv) t1 -> Int -> Int -> Val env -> Val aenv -> String
evalTuple' NilTup _ _ _env _aenv = ""
evalTuple' (e1 `SnocTup` e2) lamL letL env aenv
   = case tupS of
      ""        -> evalOpenExp e2 lamL letL env aenv
      otherwise -> tupS ++ ", " ++ evalOpenExp e2 lamL letL env aenv
   where
      tupS = evalTuple' e1 lamL letL env aenv

---------------------
-- VARIABLE HELPER --
---------------------

getVarNum :: Idx env t -> Int
getVarNum ZeroIdx = 0
getVarNum (SuccIdx idx) = 1 + (getVarNum idx)
