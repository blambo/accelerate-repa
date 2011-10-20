{-# LANGUAGE CPP, GADTs, BangPatterns, TypeOperators, PatternGuards #-}
{-# LANGUAGE TypeFamilies, ScopedTypeVariables, FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, RankNTypes #-}
-- |
-- Module     : Data.Array.Accelerate.Repa.Evaluations
--
-- Maintainer : Ben Lambert-Smith <blambo+github@gmail.com>
--
-- Defines the code generation for Acc, Exp and Fun nodes into Repa code

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

evalAcc :: forall a. Acc a -> Doc
evalAcc acc
 = parsedS
 where
   RepaAcc parsedS = evalOpenAcc acc 0

-- | Unpacks AST by removing 'OpenAcc' shell
evalOpenAcc :: forall aenv a. OpenAcc aenv a -> Int -> RepaAcc
evalOpenAcc (OpenAcc acc) = evalPreOpenAcc acc

-- | Traverses over AST
evalPreOpenAcc :: forall aenv a. PreOpenAcc OpenAcc aenv a
               -> Int
               -> RepaAcc

evalPreOpenAcc (Let acc1 acc2) letLevel
 = RepaAcc returnDoc
 where
   RepaAcc arr1 = evalOpenAcc acc1 letLevel
   RepaAcc arr2 = evalOpenAcc acc2 (letLevel+1)

   var          = char 'y' <> int letLevel
   returnDoc    = text "let" <+> var
              <+> equals <+> parens arr1
               $$ text "in"
               $$ nest 1 arr2


evalPreOpenAcc (Let2 acc1 acc2) letLevel
 = RepaAcc returnDoc
 where
   RepaAcc arr1 = evalOpenAcc acc1 letLevel
   RepaAcc arr2 = evalOpenAcc acc2 (letLevel+2)
   var1         = char 'y' <> int letLevel
   var2         = char 'y' <> int (letLevel + 1)
   returnDoc    = text "let" <+> parens (var1 <> comma <+> var2)
              <+> equals
              <+> (parens $ nest 1 arr1)
               $$ text "in"
               $$ nest 1 arr2


evalPreOpenAcc (PairArrays acc1 acc2) letLevel
 = RepaAcc returnDoc
 where
   RepaAcc arr1 = evalOpenAcc acc1 letLevel
   RepaAcc arr2 = evalOpenAcc acc2 letLevel

   returnDoc    = parens (parens arr1 <> comma <+> parens arr2)


evalPreOpenAcc (Avar idx) letLevel
 = RepaAcc var
 where
   var    = char 'y' <> int (letLevel - varNum - 1)
   varNum = getVarNum idx

-- TODO: Possibly not currently correct
evalPreOpenAcc (Apply (Alam (Abody funAcc)) acc) letLevel
 = RepaAcc $ returnDoc
 where
   RepaAcc fun = evalOpenAcc funAcc (1)
   RepaAcc arr = evalOpenAcc acc    letLevel

   var         = char 'y' <> int 0
   returnDoc   = text "let" <+> var
             <+> equals <+> parens arr
              $$ text "in"
              $$ nest 1 fun


evalPreOpenAcc (Apply _afun _acc) _letLevel
 = error "GHC pattern matching does not detect that this case is impossible"


evalPreOpenAcc (Acond cond acc1 acc2) letLevel
 = RepaAcc returnDoc
 where
   exp          = toDoc $ evalExp cond letLevel
   RepaAcc arr1 = evalOpenAcc acc1 letLevel
   RepaAcc arr2 = evalOpenAcc acc2 letLevel

   returnDoc    = text "if" <+> exp
               $$ text "then" $$ (nest 1 arr1)
               $$ text "else" $$ (nest 1 arr2)



evalPreOpenAcc (Use arr@(Array sh e)) letLevel
 = RepaAcc returnDoc
 where
   shS       = printShape sh
   arrL      = toList arr
   arrData   = text $ show $ arrL
   listType  = text $ (showsTypeRep $ typeOf $ arrL) ""

   returnDoc = text "fromList"
           <+> parens shS
           <+> parens (arrData <+> colon <> colon <+> listType)


evalPreOpenAcc (Unit e) letLevel
 = RepaAcc returnDoc
 where
   exp       = toDoc $ evalExp e letLevel
   returnDoc = text "fromList Z" <+> brackets exp


evalPreOpenAcc (Reshape e acc) letLevel
 = RepaAcc returnDoc
 where
   RepaAcc arr = evalOpenAcc acc letLevel
   exp         = toDoc $ evalExp e letLevel

   returnDoc   = text "reshape" <+> parens exp <+> parens arr


evalPreOpenAcc (Generate sh f) letLevel
 = RepaAcc returnDoc
 where
   exp         = toDoc $ evalExp sh letLevel
   RepaAcc fun = evalFun f letLevel

   returnDoc   = text "fromFunction"
             <+> parens exp
             <+> parens fun


--TODO
evalPreOpenAcc (Replicate _sliceIndex _slix _acc) _letLevel
 = RepaAcc $ text "<ERROR:Replicate>"


--TODO
evalPreOpenAcc (Index _sliceIndex _acc _slix) _letLevel
 = RepaAcc $ text "<ERROR:Index>"


evalPreOpenAcc (Map f acc) letLevel
 = RepaAcc returnDoc
 where
   RepaAcc fun = evalFun     f   letLevel
   RepaAcc arr = evalOpenAcc acc letLevel

   returnDoc   = text "Repa.map"
             <+> (parens fun
              $$ parens arr)


evalPreOpenAcc (ZipWith f acc1 acc2) letLevel
 = RepaAcc returnDoc
 where
   RepaAcc fun  = evalFun f letLevel
   RepaAcc arr1 = evalOpenAcc acc1 letLevel 
   RepaAcc arr2 = evalOpenAcc acc2 letLevel 

   returnDoc       = text "Repa.zipWith"
                 <+> (parens fun
                  $$ parens arr1
                  $$ parens arr2)


evalPreOpenAcc (Fold f e acc) letLevel
 = RepaAcc returnDoc
 where
   RepaAcc fun = evalFun     f   letLevel
   exp         = toDoc $ evalExp e letLevel
   RepaAcc arr = evalOpenAcc acc letLevel

   returnDoc      = text "fold"
                <+> (parens fun
                 $$ parens exp
                 $$ parens arr)


evalPreOpenAcc (Fold1 f acc) letLevel
 = RepaAcc $ returnDoc
 where
   RepaAcc combD   = evalFun f letLevel
   RepaAcc srcArrD = evalOpenAcc acc letLevel

   newShapeD = parens $ text "\\(sh:._) -> sh"
   genElemD
      = parens $ text "\\lookup pos ->"
             <+> (text "let (_:.end) = extent srcArr"
               $$ text "in foldr1 comb"
               $$ nest 1 (text "$ Prelude.map (lookup) [(pos:.i) | i <- [0..(end-1)]]"))

   returnDoc =
      text "let" <+> (text "srcArr =" <+> srcArrD
                   $$ text "comb   =" <+> combD)
      $$ text "in traverse"
                  <+> (parens srcArrD
                   $$ newShapeD
                   $$ genElemD)

--TODO
evalPreOpenAcc (FoldSeg _f _e _acc1 _acc2) _letLevel
 = RepaAcc $ text "<ERROR:FoldSeg>"


--TODO
evalPreOpenAcc (Fold1Seg _f _acc1 _acc2) _letLevel
 = RepaAcc $ text "<ERROR:Fold1Seg>"


-- Current generated code will be grossly inefficient, will need to generate more
-- efficient code later, but currently working
evalPreOpenAcc (Scanl f e acc) letLevel
 = RepaAcc returnDoc
 where
   RepaAcc fun = evalFun     f   letLevel
   RepaAcc arr = evalOpenAcc acc letLevel
   exp         = toDoc $ evalExp     e   letLevel


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
evalPreOpenAcc (Scanl' f e acc) letLevel
 = RepaAcc returnDoc
 where
   RepaAcc fun = evalFun     f   letLevel
   RepaAcc arr = evalOpenAcc acc letLevel
   exp         = toDoc $ evalExp     e   letLevel

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


evalPreOpenAcc (Scanl1 f acc) letLevel
 = RepaAcc returnDoc
 where
   RepaAcc fun = evalFun     f   letLevel
   RepaAcc arr = evalOpenAcc acc letLevel

   returnDoc   = text "traverse"
             <+> (parens arr $$ text "(id)" $$ parens newValDoc)
   newValDoc   = text "let newVal orig sh@(Z:.pos)"
             <+> ((text "| pos == 0" <+> equals <+> text "orig sh")
              $$ (text "| otherwise" <+> equals
             <+> (parens fun
              $$ parens (text "newVal orig (Z:.(pos-1))")
              $$ parens (text "orig sh"))))
              $$ text "in newVal"


evalPreOpenAcc (Scanr f e acc) letLevel
 = RepaAcc returnDoc
 where
   RepaAcc fun = evalFun     f   letLevel
   RepaAcc arr = evalOpenAcc acc letLevel
   exp         = toDoc $ evalExp     e   letLevel

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


evalPreOpenAcc (Scanr' f e acc) letLevel
 = RepaAcc returnDoc
 where
   RepaAcc fun = evalFun     f   letLevel
   RepaAcc arr = evalOpenAcc acc letLevel
   exp         = toDoc $ evalExp     e   letLevel

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


evalPreOpenAcc (Scanr1 f acc) letLevel
 = RepaAcc returnDoc
 where
   RepaAcc fun = evalFun     f   letLevel
   RepaAcc arr = evalOpenAcc acc letLevel

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

--TODO: Needs to handle ignore case
evalPreOpenAcc (Permute f dftAcc p acc) letLevel
 = RepaAcc $ returnDoc
 where
   RepaAcc dftArrD = evalOpenAcc dftAcc letLevel
   RepaAcc srcArrD = evalOpenAcc acc    letLevel
   RepaAcc combD   = evalFun     f      letLevel
   RepaAcc permD   = evalFun     p      letLevel

   returnDoc =
      text "let" <+> (text "srcArr =" <+> srcArrD
                   $$ text "dftArr =" <+> dftArrD
                   $$ text "perm   =" <+> (parens permD)
                   $$ text "comb   =" <+> (parens combD)
                   $$ permuteDoc)
      $$ text "in"
      $$ nest 1 (text "permute 0 comb dftArr perm srcArr")

   permuteDoc =
      text "permute idx comb dftArr perm srcArr" $$
      nest 1 (text "| idx >= (size $ extent srcArr) = dftArr"
           $$ text "| otherwise =" <+> permuteArgsDoc)

   permuteArgsDoc =
      text "let" <+> (text "srcIdx = fromIndex (extent srcArr) idx"
          $$ text "newArr = fromFunction"
          <+> (text "(extent dftArr)"
            $$ text "(\\sh -> case sh == (perm srcIdx) of"
            $$ nest 1 (text "True  -> (dftArr ! (perm srcIdx)) `comb`"
                            <+> text "(srcArr ! srcIdx)"
                    $$ text "False -> index dftArr sh)")))
      $$ text "in permute (idx+1) comb newArr perm srcArr"


evalPreOpenAcc (Backpermute e p acc) letLevel
 = RepaAcc $ returnDoc
 where
   returnDoc = text "backpermute"
           <+> parens expD
           <+> parens permD
           <+> parens srcArrD
   expD            = toDoc $ evalExp e letLevel
   RepaAcc permD   = evalFun     p   letLevel
   RepaAcc srcArrD = evalOpenAcc acc letLevel

--TODO
evalPreOpenAcc (Stencil sten bndy acc) letLevel
 = RepaAcc $ returnDoc
 where
   RepaAcc funD = evalFun      sten letLevel
   RepaAcc arrD = evalOpenAcc  acc  letLevel
   bndyD        = evalBoundary acc bndy

   returnDoc    = letD $$ traverseD

   letD = text "let" <+> (text "arr  =" <+> arrD
                       $$ text "bndy =" <+> bndyD
                       $$ text "sten =" <+> parens funD)
       $$ text "in"
   traverseD
    = text "traverse"
    <+> (text "arr"
      $$ text "id"
      -- $$ text "(sten . (stencilData (bound arr bndy (arrayExtent arr)) arr))")
      $$ text "(\\lookup curr -> sten $ stencilData (bound lookup bndy (arrayExtent arr)) arr curr)")


--TODO
evalPreOpenAcc (Stencil2 _sten _bndy1 _acc1 _bndy2 _acc2) _letLevel
 = RepaAcc $ text "<ERROR:Stencil2>"

evalPreOpenAcc _ _ = RepaAcc $ text "<UNDEFINED>"

--------------------
-- FUNCTION NODES --
--------------------

evalFun :: Fun aenv t -> Int -> RepaAcc
evalFun f letL = evalOpenFun f 0 letL

evalOpenFun :: OpenFun env aenv t -> Int -> Int -> RepaAcc
evalOpenFun (Body e) lamL letL
 = RepaAcc $ parens (toDoc $ evalOpenExp e lamL letL) <+> colon <> colon <+> (expToString e)
evalOpenFun (Lam f)  lamL letL
 = RepaAcc (text "\\" <> varName <+> text "->" <+> funS)
 where
   RepaAcc funS = evalOpenFun f (lamL+1) letL
   varName = text "x" <> int lamL

----------------------
-- EXPRESSION NODES --
----------------------

-- Evaluate an open expression
evalOpenExp :: forall a env aenv .
               OpenExp env aenv a -> Int -> Int -> RepaExp

evalOpenExp (Var idx) lamL letL
   = RepaExp $ char 'x' <> int varNum
   where
      varNum = lamL - (getVarNum idx) - 1

evalOpenExp (Const c) _ _
   = RepaExp $ val <+> colon <> colon <+> typeS
   where
      val   = text $ show ((Sugar.toElt c) :: a)
      typeS = text $ (showsTypeRep $ typeOf ((Sugar.toElt c) :: a)) ""

evalOpenExp (Tuple tup) lamL letL
   = evalTuple tup lamL letL

evalOpenExp (Prj idx e) lamL letL
   = RepaExp $ text "let" <+> parens prjS <+> equals <+> parens expS
           <+> text "in" <+> prjVarName
   where
      prjS = evalPrj (tupSize $ expType e) (tupIdx idx)
      expS = toDoc $ evalOpenExp e lamL letL

evalOpenExp IndexNil _ _
   = RepaExp $ char 'Z'

evalOpenExp (IndexCons sh i) lamL letL
   = RepaExp $ shS <+> text ":." <+> parens ix
   where
      shS = toDoc $ evalOpenExp sh lamL letL
      ix  = toDoc $ evalOpenExp i lamL letL

evalOpenExp (IndexHead ix) lamL letL
   = RepaExp $ text "case" <+> parens exp <+> text "of (_:.h) -> h"
   where
      exp = toDoc $ evalOpenExp ix lamL letL

evalOpenExp (IndexTail ix) lamL letL
   = RepaExp $ text "case" <+> parens exp <+> text "of (t:._) -> t"
   where
      exp = toDoc $ evalOpenExp ix lamL letL

evalOpenExp (IndexAny) _ _
   = RepaExp $ text "Any"

evalOpenExp (Cond c t e) lamL letL
   = RepaExp $ text "if" <+> cond
  $$ (nest 1 $ text "then" <+> exp1)
  $$ (nest 1 $ text "else" <+> exp2)
   where
      cond = toDoc $ evalOpenExp c lamL letL
      exp1 = toDoc $ evalOpenExp t lamL letL
      exp2 = toDoc $ evalOpenExp e lamL letL

evalOpenExp (PrimConst c) _ _
   = RepaExp $ evalPrimConst c

evalOpenExp (PrimApp p arg) lamL letL
   = RepaExp $ evalPrim p argS
   where
      argS = evalOpenExp arg lamL letL

evalOpenExp (IndexScalar acc ix) lamL letL
   = RepaExp $ parens arr <+> char '!' <+> parens idx
   where
      RepaAcc arr = evalOpenAcc acc letL
      RepaExp idx = evalOpenExp ix lamL letL

evalOpenExp (Shape acc) lamL letL
   = RepaExp $ text "extent" <+> parens arr
   where
      RepaAcc arr = evalOpenAcc acc letL

evalOpenExp (Size acc) _lamL letL
   = RepaExp $ text "Repa.size" <+> parens arr
   where
      RepaAcc arr = evalOpenAcc acc letL

evalOpenExp _ _ _ = RepaExp $ text "<UNDEFINED>"

-- Evaluate a closed expression
--
evalExp :: PreExp OpenAcc aenv t -> Int -> RepaExp
evalExp e letL = evalOpenExp e 0 letL

------------
-- TUPLES --
------------
evalTuple :: Tuple (OpenExp env aenv) t -> Int -> Int -> RepaExp
evalTuple tup lamL letL = RepaTuple $ evalTuple' tup lamL letL

evalTuple' :: Tuple (OpenExp env aenv) t -> Int -> Int -> [Doc]
evalTuple' NilTup _ _ = []
evalTuple' (e1 `SnocTup` e2) lamL letL
   = tup ++ [t]
   where
      t   = toDoc $ evalOpenExp e2 lamL letL
      tup = evalTuple' e1 lamL letL

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

-------------------------------
-- EVAL BOUNDARY EXPRESSIONS --
-------------------------------

evalBoundary :: forall aenv sh e. (Elt e)
             => OpenAcc aenv (Array sh e)
             -> Boundary (EltRepr e)
             -> Doc
evalBoundary _ bndy = case bndy of
                       Clamp      -> text "Clamp"
                       Mirror     -> text "Mirror"
                       Wrap       -> text "Wrap"
                       Constant a -> text "Constant" <+> text (show ((Sugar.toElt a) :: e))

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


-----------------------------
-- TYPING HELPER FUNCTIONS --
-----------------------------

-- Creates a Doc for the given expression
expToString :: OpenExp env aenv a -> Doc
expToString exp = tupleTypeToString $ expType exp

tupleTypeToString :: TupleType a -> Doc
tupleTypeToString UnitTuple = empty
tupleTypeToString (PairTuple a b) = let aDoc = tupleTypeToString a
                                        bDoc = tupleTypeToString b
                                    in
                                       if isEmpty aDoc
                                          then bDoc
                                          else if isEmpty bDoc
                                             then aDoc
                                             else aDoc <> comma <+> bDoc
tupleTypeToString (SingleTuple a) = text $ show a

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
