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

import Data.Typeable
import Text.PrettyPrint

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Array.Sugar as Sugar
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Type
import qualified Data.Array.Accelerate.Array.Representation as Repr

import Data.Array.Accelerate.Repa.Evaluations.Prim
import Data.Array.Accelerate.Repa.Traverse


---------------
-- ACC NODES --
---------------

evalAcc :: Acc a -> Doc
evalAcc acc
 = parsedS
 where
   RepaAcc parsedS = evalOpenAcc acc 0 Empty

-- | Unpacks AST by removing 'OpenAcc' shell
evalOpenAcc :: OpenAcc aenv a -> Int -> Val aenv -> RepaAcc
evalOpenAcc (OpenAcc acc) = evalPreOpenAcc acc

-- | Traverses over AST
evalPreOpenAcc :: PreOpenAcc OpenAcc aenv a -> Int -> Val aenv -> RepaAcc

evalPreOpenAcc (Let acc1 acc2) letLevel aenv
 = RepaAcc returnDoc
 where
   RepaAcc arr1 = evalOpenAcc acc1 letLevel      aenv
   RepaAcc arr2 = evalOpenAcc acc2 (letLevel+1) (aenv `Push` error "from let")

   var          = text "y" <> int letLevel
   returnDoc    = text "let" <+> var
              <+> equals <+> parens arr1
               $$ text "in"
               $$ nest 1 arr2


evalPreOpenAcc (Let2 acc1 acc2) letLevel aenv
 = RepaAcc returnDoc
 where
   RepaAcc arr1 = evalOpenAcc acc1 letLevel aenv
   RepaAcc arr2 = evalOpenAcc acc2 (letLevel+2) (aenv `Push` (error "let2,1") 
                                                         `Push` (error "let2,2"))
   var1         = text "y" <> int letLevel
   var2         = text "y" <> int (letLevel + 1)
   returnDoc    = text "let" <+> parens (var1 <> comma <+> var2)
              <+> equals
              <+> (parens $ nest 1 arr1)
               $$ text "in"
               $$ nest 1 arr2


evalPreOpenAcc (PairArrays acc1 acc2) letLevel aenv
 = RepaAcc returnDoc
 where
   RepaAcc arr1 = evalOpenAcc acc1 letLevel aenv
   RepaAcc arr2 = evalOpenAcc acc2 letLevel aenv

   returnDoc    = parens (parens arr1 <> comma <+> parens arr2)


evalPreOpenAcc (Avar idx) letLevel _aenv
 = RepaAcc var
 where
   var    = text "y" <> int (letLevel - varNum - 1)
   varNum = getVarNum idx


-- TODO
evalPreOpenAcc (Apply (Alam (Abody _funAcc)) _acc) _letLevel _aenv
 = RepaAcc $ text "<ERROR:Apply>"
evalPreOpenAcc (Apply _afun _acc) _letLevel _aenv
 = error "GHC pattern matching does not detect that this case is impossible"


evalPreOpenAcc (Acond cond acc1 acc2) letLevel aenv
 = RepaAcc returnDoc
 where
   exp          = toDoc $ evalExp cond letLevel aenv
   RepaAcc arr1 = evalOpenAcc acc1 letLevel aenv
   RepaAcc arr2 = evalOpenAcc acc2 letLevel aenv

   returnDoc    = text "if" <+> exp
               $$ text "then" $$ (nest 1 arr1)
               $$ text "else" $$ (nest 1 arr2)



evalPreOpenAcc (Use arr@(Array sh e)) letLevel aenv
 = RepaAcc returnDoc
 where
   shS       = printShape sh
   arrL      = toList arr
   arrData   = text $ show $ arrL
   listType  = text $ (showsTypeRep $ typeOf $ arrL) ""

   returnDoc = text "fromList"
           <+> parens shS
           <+> parens (arrData <+> colon <> colon <+> listType)


evalPreOpenAcc (Unit e) letLevel aenv
 = RepaAcc returnDoc
 where
   exp       = toDoc $ evalExp e letLevel aenv
   returnDoc = text "fromList Z" <+> brackets exp


evalPreOpenAcc (Reshape e acc) letLevel aenv
 = RepaAcc returnDoc
 where
   RepaAcc arr = evalOpenAcc acc letLevel aenv
   exp         = toDoc $ evalExp e letLevel aenv

   returnDoc   = text "reshape" <+> parens exp <+> parens arr


evalPreOpenAcc (Generate sh f) letLevel aenv
 = RepaAcc returnDoc
 where
   exp         = toDoc $ evalExp sh letLevel aenv
   RepaAcc fun = evalFun f letLevel aenv

   returnDoc   = text "fromFunction"
             <+> parens exp
             <+> parens fun


--TODO
evalPreOpenAcc (Replicate _sliceIndex _slix _acc) _letLevel _aenv
 = RepaAcc $ text "<ERROR:Replicate>"


--TODO
evalPreOpenAcc (Index _sliceIndex _acc _slix) _letLevel _aenv
 = RepaAcc $ text "<ERROR:Index>"


evalPreOpenAcc (Map f acc) letLevel aenv
 = RepaAcc returnDoc
 where
   RepaAcc fun = evalFun     f   letLevel aenv
   RepaAcc arr = evalOpenAcc acc letLevel aenv

   returnDoc   = text "Repa.map"
             <+> (parens fun
              $$ parens arr)


evalPreOpenAcc (ZipWith f acc1 acc2) letLevel aenv
 = RepaAcc returnDoc
 where
   RepaAcc fun  = evalFun f letLevel aenv
   RepaAcc arr1 = (evalOpenAcc acc1 letLevel aenv) 
   RepaAcc arr2 = (evalOpenAcc acc2 letLevel aenv) 

   returnDoc       = text "Repa.zipWith"
                 <+> (parens fun
                  $$ parens arr1
                  $$ parens arr2)


evalPreOpenAcc (Fold f e acc) letLevel aenv
 = RepaAcc returnDoc
 where
   RepaAcc fun = evalFun     f   letLevel aenv
   exp         = toDoc $ evalExp e letLevel aenv
   RepaAcc arr = evalOpenAcc acc letLevel aenv

   returnDoc      = text "fold"
                <+> (parens fun
                 $$ parens exp
                 $$ parens arr)


--TODO
evalPreOpenAcc (Fold1 _f _acc) _letLevel _aenv
 = RepaAcc $ text "<ERROR:Fold1>"


--TODO
evalPreOpenAcc (FoldSeg _f _e _acc1 _acc2) _letLevel _aenv
 = RepaAcc $ text "<ERROR:FoldSeg>"


--TODO
evalPreOpenAcc (Fold1Seg _f _acc1 _acc2) _letLevel _aenv
 = RepaAcc $ text "<ERROR:Fold1Seg>"


-- Current generated code will be grossly inefficient, will need to generate more
-- efficient code later, but currently working
evalPreOpenAcc (Scanl f e acc) letLevel aenv
 = RepaAcc returnDoc
 where
   RepaAcc fun = evalFun     f   letLevel aenv
   RepaAcc arr = evalOpenAcc acc letLevel aenv
   exp         = toDoc $ evalExp     e   letLevel aenv


   returnDoc   = text "traverse"
             <+> (parens arr $$ parens shapeDoc $$ parens newValDoc)
   shapeDoc    = text "\\(Z:.i) -> (Z:.(i+1))"
   newValDoc   = text "let newVal orig (Z:.pos)"
             <+> ((text "| pos == 0" <+> equals <+> exp)
              $$  (text "| otherwise" <+> equals
             <+> (parens fun
              $$ parens (text "newVal orig (Z:.(pos-1))")
              $$ parens (text "orig (Z:.(pos-1))"))))
              $$ text "in newVal"


-- Generated code is quite inefficient due to repeated computations
-- TODO: Is probably incorrect will need to rewrite to do left scan
evalPreOpenAcc (Scanl' f e acc) letLevel aenv
 = RepaAcc returnDoc
 where
   RepaAcc fun = evalFun     f   letLevel aenv
   RepaAcc arr = evalOpenAcc acc letLevel aenv
   exp         = toDoc $ evalExp     e   letLevel aenv

   returnDoc   = parens (first $$ comma $$ second)
   first       = text "traverse"
             <+> (parens arr $$ text "(id)" $$ parens newValDoc)
   second      = text "fold" <+> parens fun <+> parens exp <+> parens arr
   newValDoc   = text "let newVal orig (Z:.pos)"
             <+> ((text "| pos == 0" <+> equals <+> exp)
              $$ nest 1 (text "| otherwise" <+> equals
             <+> (parens fun
              $$ parens (text "newVal orig (Z:.(pos-1))")
              $$ parens (text "orig (Z:.(pos-1))"))))
              $$ text "in newVal"


evalPreOpenAcc (Scanl1 f acc) letLevel aenv
 = RepaAcc returnDoc
 where
   RepaAcc fun = evalFun     f   letLevel aenv
   RepaAcc arr = evalOpenAcc acc letLevel aenv

   returnDoc   = text "traverse"
             <+> (parens arr $$ text "(id)" $$ parens newValDoc)
   newValDoc   = text "let newVal orig sh@(Z:.pos)"
             <+> ((text "| pos == 0" <+> equals <+> text "orig sh")
              $$ (text "| otherwise" <+> equals
             <+> (parens fun
              $$ parens (text "newVal orig (Z:.(pos-1))")
              $$ parens (text "orig sh"))))
              $$ text "in newVal"


evalPreOpenAcc (Scanr f e acc) letLevel aenv
 = RepaAcc returnDoc
 where
   RepaAcc fun = evalFun     f   letLevel aenv
   RepaAcc arr = evalOpenAcc acc letLevel aenv
   exp         = toDoc $ evalExp     e   letLevel aenv

   returnDoc   = text "traverse"
             <+> (parens arr $$ shapeDoc $$ parens newValDoc)
   shapeDoc    = parens $ text "\\(Z:.i) -> (Z:.(i+1))"
   newValDoc   = text "let newVal orig sh@(Z:.pos)"
             <+> ((text "| pos ==" <+> last <+> equals <+> exp)
              $$ (text "| otherwise" <+> equals
             <+> (parens fun
              $$ parens (text "newVal orig (Z:.(pos+1))")
              $$ parens (text "orig sh"))))
              $$ text "in newVal"
   last = parens $ text "size $ extent $" <+> arr


evalPreOpenAcc (Scanr' f e acc) letLevel aenv
 = RepaAcc returnDoc
 where
   RepaAcc fun = evalFun     f   letLevel aenv
   RepaAcc arr = evalOpenAcc acc letLevel aenv
   exp         = toDoc $ evalExp     e   letLevel aenv

   returnDoc   = text "let res" <+> equals <+> text "traverse"
             <+> (parens arr $$ parens shapeDoc $$ parens newVarDoc)
              $$ text "in"
             <+> tuple

   tuple       = parens (first $$ comma $$ second)

   first       = text "traverse res (\\(Z:.i) -> (Z:.(i-1)))"
             <+> (parens $ text "\\orig (Z:.pos) -> orig (Z:.(pos+1))")
   second      = text "fromList Z [(res!(Z:.0))]"

   shapeDoc    = text "\\(Z:.i) -> (Z:.(i+1))"
   newVarDoc   = text "let newVal orig sh@(Z:.pos)"
             <+> ((text "| pos ==" <+> last <+> equals <+> exp)
              $$ (text "| otherwise" <+> equals
             <+> (parens fun
              $$ parens (text "newVal orig (Z:.(pos+1))")
              $$ parens (text "orig sh"))))
              $$ text "in newVal"
   last        = parens $ text "size $ extent $" <+> arr


evalPreOpenAcc (Scanr1 f acc) letLevel aenv
 = RepaAcc returnDoc
 where
   RepaAcc fun = evalFun     f   letLevel aenv
   RepaAcc arr = evalOpenAcc acc letLevel aenv

   returnDoc   = text "traverse"
             <+> (parens arr $$ shapeDoc $$ parens newVarDoc)
   shapeDoc    = parens $ text "id"
   newVarDoc   = text "let newVal orig sh@(Z:.pos)"
             <+> ((text "| pos ==" <+> parens last <+> text "- 1"
                         <+> equals <+> text "orig sh")
              $$ (text "| otherwise" <+> equals
             <+> (parens fun
              $$ parens (text "newVal orig (Z:.(pos+1))")
              $$ parens (text "orig sh"))))
              $$ text "in newVal" 
   last        = parens $ text "size $ extent $" <+> arr


--TODO
evalPreOpenAcc (Permute f dftAcc p acc) letLevel aenv
 = RepaAcc $ text "<ERROR:Permute>"
 where
   {-
   RepaAcc dftArrS = evalOpenAcc dftAcc letLevel aenv
   RepaAcc arrS    = evalOpenAcc acc    letLevel aenv
   RepaAcc funS    = evalFun     f      letLevel aenv
   permS              = evalFun     p      letLevel aenv

   returnS = "(let " ++ lookupS ++ " in " ++ fromFunctionS ++ ")"
   -- Defines the resulting array
   fromFunctionS = "(fromFunction (extent " ++ dftArrS ++ ") " ++
      "(\\sh -> case lookup' sh ??? of " ++
                "Nothing -> " ++ dftArrS ++ " ! sh ; " ++
                "Just xs -> Prelude.foldl " ++ funS ++ " (" ++ dftArrS ++ " ! sh) xs))"
   -- Builds the association list by going over all source array elements and
   --  recording a mapping between the resulting index after applying the
   --  permutation function and the value in source array (before the mapping)
   assListS = ""
   -- Defines a function to look up the generated association list for all
   --  matching values to the destination index
   lookupS = 
      "(lookup' :: Eq a => a -> [(a,b)] -> Maybe [b] ; " ++
      "lookup' _key [] = Nothing ; " ++
      "lookup' key ((x,y):xys) " ++
         "| key == x = (case (lookup' key xys) of " ++
                           "Just ys -> Just (y:ys) ; " ++
                           "Nothing -> Just [y]) " ++
         "| otherwise = lookup' key xys)"
   -}
--TODO
evalPreOpenAcc (Backpermute _e _p _acc) _letLevel _aenv
 = RepaAcc $ text "<ERROR:Backpermute>"

--TODO
evalPreOpenAcc (Stencil _sten _bndy _acc) _letLevel _aenv
 = RepaAcc $ text "<ERROR:Stencil>"

--TODO
evalPreOpenAcc (Stencil2 _sten _bndy1 _acc1 _bndy2 _acc2) _letLevel _aenv
 = RepaAcc $ text "<ERROR:Stencil2>"

evalPreOpenAcc _ _ _ = RepaAcc $ text "<UNDEFINED>"

--------------------
-- FUNCTION NODES --
--------------------

evalFun :: Fun aenv t -> Int-> Val aenv -> RepaAcc
evalFun f letL aenv = evalOpenFun f 0 letL Empty aenv

evalOpenFun :: OpenFun env aenv t -> Int -> Int -> Val env -> Val aenv -> RepaAcc
evalOpenFun (Body e) lamL letL env aenv
 = RepaAcc $ toDoc $ evalOpenExp e lamL letL env aenv
evalOpenFun (Lam f)  lamL letL env aenv
 = RepaAcc (text "\\" <> varName <+> text "->" <+> funS)
 where
   RepaAcc funS = evalOpenFun f (lamL+1) letL (env `Push` (error "Lam")) aenv
   varName = text "x" <> int lamL


----------------------
-- EXPRESSION NODES --
----------------------

-- Evaluate an open expression
evalOpenExp :: forall a env aenv .
               OpenExp env aenv a -> Int -> Int -> Val env -> Val aenv -> RepaExp

evalOpenExp (Var idx) lamL letL env _
   = RepaExp $ char 'x' <> int varNum
   where
      varNum = lamL - (getVarNum idx) - 1

evalOpenExp (Const c) _ _ _ _
   = RepaExp $ val <+> colon <> colon <+> typeS
   where
      val   = text $ show ((Sugar.toElt c) :: a)
      typeS = text $ (showsTypeRep $ typeOf ((Sugar.toElt c) :: a)) ""

evalOpenExp (Tuple tup) lamL letL env aenv 
   = evalTuple tup lamL letL env aenv

evalOpenExp (Prj idx e) lamL letL env aenv 
   = RepaExp $ text "let" <+> parens prjS <+> equals <+> parens expS
           <+> text "in" <+> prjVarName
   where
      prjS = evalPrj (tupSize $ expType e) (tupIdx idx)
      expS = toDoc $ evalOpenExp e lamL letL env aenv

evalOpenExp IndexNil _ _ _ _
   = RepaExp $ char 'Z'

evalOpenExp (IndexCons sh i) lamL letL env aenv 
   = RepaExp $ shS <+> text ":." <+> parens ix
   where
      shS = toDoc $ evalOpenExp sh lamL letL env aenv
      ix  = toDoc $ evalOpenExp i lamL letL env aenv 

evalOpenExp (IndexHead ix) lamL letL env aenv 
   = RepaExp $ text "case" <+> parens exp <+> text "of (_:.h) -> h"
   where
      exp = toDoc $ evalOpenExp ix lamL letL env aenv

evalOpenExp (IndexTail ix) lamL letL env aenv 
   = RepaExp $ text "case" <+> parens exp <+> text "of (t:._) -> t"
   where
      exp = toDoc $ evalOpenExp ix lamL letL env aenv

evalOpenExp (IndexAny) _ _ _ _
   = RepaExp $ text "Any"

evalOpenExp (Cond c t e) lamL letL env aenv 
   = RepaExp $ text "if" <+> cond
  $$ (nest 1 $ text "then" <+> exp1)
  $$ (nest 1 $ text "else" <+> exp2)
   where
      cond = toDoc $ evalOpenExp c lamL letL env aenv
      exp1 = toDoc $ evalOpenExp t lamL letL env aenv
      exp2 = toDoc $ evalOpenExp e lamL letL env aenv

evalOpenExp (PrimConst c) _ _ _ _
   = RepaExp $ evalPrimConst c

evalOpenExp (PrimApp p arg) lamL letL env aenv 
   = RepaExp $ evalPrim p argS
   where
      argS = evalOpenExp arg lamL letL env aenv

evalOpenExp (IndexScalar acc ix) lamL letL env aenv 
   = RepaExp $ parens arr <+> char '!' <+> parens idx
   where
      RepaAcc arr = evalOpenAcc acc letL aenv
      RepaExp idx = evalOpenExp ix lamL letL env aenv

--TODO
evalOpenExp (Shape acc) _lamL _letL _env _aenv 
   = RepaExp $ text "<ERROR:Shape>"

evalOpenExp (Size acc) _lamL letL _env aenv 
   = RepaExp $ text "Repa.size" <+> parens arr
   where
      RepaAcc arr = evalOpenAcc acc letL aenv

evalOpenExp _ _ _ _ _ = RepaExp $ text "<UNDEFINED>"

-- Evaluate a closed expression
--
evalExp :: PreExp OpenAcc aenv t -> Int -> Val aenv -> RepaExp
evalExp e letL aenv = evalOpenExp e 0 letL Empty aenv

------------
-- TUPLES --
------------
evalTuple :: Tuple (OpenExp env aenv) t -> Int -> Int -> Val env -> Val aenv -> RepaExp
evalTuple tup lamL letL env aenv = RepaTuple $ evalTuple' tup lamL letL env aenv

evalTuple' :: Tuple (OpenExp env aenv) t -> Int -> Int -> Val env -> Val aenv -> [Doc]
evalTuple' NilTup _ _ _env _aenv = []
evalTuple' (e1 `SnocTup` e2) lamL letL env aenv
   = tup ++ [t]
   where
      t   = toDoc $ evalOpenExp e2 lamL letL env aenv
      tup = evalTuple' e1 lamL letL env aenv

---------------------
-- VARIABLE HELPER --
---------------------

getVarNum :: Idx env t -> Int
getVarNum ZeroIdx = 0
getVarNum (SuccIdx idx) = 1 + (getVarNum idx)

------------------
-- SHAPE STRING --
------------------

printShape :: Repr.Shape sh => sh -> Doc
printShape sh = text (printShape' $ Repr.shapeToList sh)

printShape' :: [Int] -> String
printShape' (x:xs) = (printShape' xs) ++ " :. (" ++ (show x) ++ " :: Int)"
printShape' []     = "Z"

-------------------------
-- EVAL PRJ EXPRESSION --
-------------------------

evalPrj :: Int -> Int -> Doc
evalPrj tup 0   = parens $ evalPrj' (tup-1) (-1)    <+> prjVarName
evalPrj tup idx = parens $ evalPrj' (tup-1) (idx-1) <+> char '_'

evalPrj' :: Int -> Int -> Doc
evalPrj' 1   0   = prjVarName <> comma
evalPrj' 1   idx = char '_'   <> comma
evalPrj' tup 0   = evalPrj' (tup-1) (-1)    <+> prjVarName <> comma
evalPrj' tup idx = evalPrj' (tup-1) (idx-1) <+> char '_'   <> comma

prjVarName :: Doc
prjVarName = text "tVar"

-- Returns the number of members in a tuple
tupSize :: TupleType a -> Int
tupSize UnitTuple       = 0
tupSize (PairTuple a b) = (tupSize a) + (tupSize b)
tupSize (SingleTuple _) = 1

-- Returns how many members of a tuple from the 'left' we are referencing
tupIdx :: TupleIdx t e -> Int
tupIdx (SuccTupIdx idx) = 1 + tupIdx idx
tupIdx ZeroTupIdx       = 0

-- Copied from Data.Array.Accelerate.Analysis.Type
tupleIdxType :: forall t e. TupleIdx t e -> TupleType (EltRepr e)
tupleIdxType ZeroTupIdx       = eltType (undefined::e)
tupleIdxType (SuccTupIdx idx) = tupleIdxType idx

-- Adapted from Data.Array.Accelerate.Analysis.Type
expType :: OpenExp aenv env t -> TupleType (EltRepr t)
expType = preExpType

preExpType :: forall acc aenv env t. PreOpenExp acc aenv env t
           -> TupleType (EltRepr t)
preExpType e =
   case e of
    Var _             -> eltType (undefined::t)
    Const _           -> eltType (undefined::t)
    Tuple _           -> eltType (undefined::t)
    Prj idx _         -> tupleIdxType idx
    IndexNil          -> eltType (undefined::t)
    IndexCons _ _     -> eltType (undefined::t)
    IndexHead _       -> eltType (undefined::t)
    IndexTail _       -> eltType (undefined::t)
    IndexAny          -> eltType (undefined::t)
    Cond _ t _        -> preExpType t
    PrimConst _       -> eltType (undefined::t)
    PrimApp _ _       -> eltType (undefined::t)
    Shape _           -> eltType (undefined::t)
    Size _            -> eltType (undefined::t)
    otherwise         -> error "Typing error"
