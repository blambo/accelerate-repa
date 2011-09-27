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
import qualified Data.Array.Accelerate.Array.Representation as Repr

import Data.Array.Accelerate.Repa.Evaluations.Prim
import Data.Array.Accelerate.Repa.RepaParsed


---------------
-- ACC NODES --
---------------

evalAcc :: Acc a -> Doc
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
 = RepaParsed returnDoc
 where
   RepaParsed arr1 = evalOpenAcc acc1 letLevel      aenv
   RepaParsed arr2 = evalOpenAcc acc2 (letLevel+1) (aenv `Push` error "from let")

   var              = text "y" <> int letLevel
   returnDoc        = text "let" <+> var
                  <+> equals <+> parens arr1
                   $$ text "in"
                   $$ nest 1 arr2


evalPreOpenAcc (Let2 acc1 acc2) letLevel aenv
 = RepaParsed returnDoc
 where
   RepaParsed arr1 = evalOpenAcc acc1 letLevel aenv
   RepaParsed arr2 = evalOpenAcc acc2 (letLevel+2) (aenv `Push` (error "let2,1") 
                                                         `Push` (error "let2,2"))
   var1            = text "y" <> int letLevel
   var2            = text "y" <> int (letLevel + 1)
   returnDoc       = text "let" <+> parens (var1 <> comma <+> var2)
                 <+> equals
                  $$ (nest 1 arr1)
                  $$ text "in"
                  $$ nest 1 arr2


evalPreOpenAcc (PairArrays acc1 acc2) letLevel aenv
 = RepaParsed returnDoc
 where
   RepaParsed arr1 = evalOpenAcc acc1 letLevel aenv
   RepaParsed arr2 = evalOpenAcc acc2 letLevel aenv

   returnDoc       = parens (parens arr1 <> comma <+> parens arr2)


evalPreOpenAcc (Avar idx) letLevel _aenv
 = RepaParsed var
 where
   var    = text "y" <> int (letLevel - varNum - 1)
   varNum = getVarNum idx


-- TODO
evalPreOpenAcc (Apply (Alam (Abody _funAcc)) _acc) _letLevel _aenv
 = RepaParsed $ text "<ERROR:Apply>"
evalPreOpenAcc (Apply _afun _acc) _letLevel _aenv
 = error "GHC pattern matching does not detect that this case is impossible"


evalPreOpenAcc (Acond cond acc1 acc2) letLevel aenv
 = RepaParsed returnDoc
 where
   exp             = evalExp cond letLevel aenv
   RepaParsed arr1 = evalOpenAcc acc1 letLevel aenv
   RepaParsed arr2 = evalOpenAcc acc2 letLevel aenv

   returnDoc       = text "if" <+> exp
                  $$ text "then" $$ (nest 1 arr1)
                  $$ text "else" $$ (nest 1 arr2)



evalPreOpenAcc (Use arr@(Array sh e)) letLevel aenv
 = RepaParsed returnDoc
 where
   shS       = printShape sh
   arrL      = toList arr
   arrData   = text $ show $ arrL
   listType  = text $ (showsTypeRep $ typeOf $ arrL) ""

   returnDoc = text "fromList"
           <+> parens shS
           <+> parens (arrData <+> colon <> colon <+> listType)


evalPreOpenAcc (Unit e) letLevel aenv
 = RepaParsed returnDoc
 where
   exp       = evalExp e letLevel aenv
   returnDoc = text "fromList Z" <+> brackets exp


evalPreOpenAcc (Reshape e acc) letLevel aenv
 = RepaParsed returnDoc
 where
   RepaParsed arr = evalOpenAcc acc letLevel aenv
   exp            = evalExp e letLevel aenv

   returnDoc      = text "reshape" <+> parens exp <+> parens arr


evalPreOpenAcc (Generate sh f) letLevel aenv
 = RepaParsed returnDoc
 where
   exp            = evalExp sh letLevel aenv
   RepaParsed fun = evalFun f letLevel aenv

   returnDoc      = text "fromFunction"
                <+> parens exp
                <+> parens fun


--TODO
evalPreOpenAcc (Replicate _sliceIndex _slix _acc) _letLevel _aenv
 = RepaParsed $ text "<ERROR:Replicate>"


--TODO
evalPreOpenAcc (Index _sliceIndex _acc _slix) _letLevel _aenv
 = RepaParsed $ text "<ERROR:Index>"


evalPreOpenAcc (Map f acc) letLevel aenv
 = RepaParsed returnDoc
 where
   RepaParsed fun = evalFun     f   letLevel aenv
   RepaParsed arr = evalOpenAcc acc letLevel aenv

   returnDoc      = text "Repa.map"
                <+> parens fun
                <+> parens arr


evalPreOpenAcc (ZipWith f acc1 acc2) letLevel aenv
 = RepaParsed returnDoc
 where
   RepaParsed fun  = evalFun f letLevel aenv
   RepaParsed arr1 = (evalOpenAcc acc1 letLevel aenv) 
   RepaParsed arr2 = (evalOpenAcc acc2 letLevel aenv) 

   returnDoc       = text "Repa.zipWith"
                 <+> parens fun
                 <+> parens arr1
                 <+> parens arr2


evalPreOpenAcc (Fold f e acc) letLevel aenv
 = RepaParsed returnDoc
 where
   RepaParsed fun = evalFun     f   letLevel aenv
   exp            = evalExp     e   letLevel aenv
   RepaParsed arr = evalOpenAcc acc letLevel aenv

   returnDoc      = text "fold"
                <+> parens fun
                <+> parens exp
                <+> parens arr


--TODO
evalPreOpenAcc (Fold1 _f _acc) _letLevel _aenv
 = RepaParsed $ text "<ERROR:Fold1>"


--TODO
evalPreOpenAcc (FoldSeg _f _e _acc1 _acc2) _letLevel _aenv
 = RepaParsed $ text "<ERROR:FoldSeg>"


--TODO
evalPreOpenAcc (Fold1Seg _f _acc1 _acc2) _letLevel _aenv
 = RepaParsed $ text "<ERROR:Fold1Seg>"


-- Current generated code will be grossly inefficient, will need to generate more
-- efficient code later, but currently working
evalPreOpenAcc (Scanl f e acc) letLevel aenv
 = RepaParsed returnDoc
 where
   RepaParsed fun = evalFun     f   letLevel aenv
   RepaParsed arr = evalOpenAcc acc letLevel aenv
   exp            = evalExp     e   letLevel aenv


   returnDoc      = text "traverse"
                <+> (parens arr $$ parens shapeDoc $$ parens newValDoc)
   shapeDoc       = text "\\(Z:.i) -> (Z:.(i+1))"
   newValDoc      = text "let newVal orig (Z:.pos) "
                 $$ nest 1 (text "| pos == 0" <+> equals <+> exp)
                 $$ nest 1 (text "| otherwise" <+> equals
                <+> parens fun
                <+> parens (text "newVal orig (Z:.(pos-1))")
                <+> parens (text "orig (Z:.(pos-1))"))
                 $$ text "in newVal"


-- Generated code is quite inefficient due to repeated computations
evalPreOpenAcc (Scanl' f e acc) letLevel aenv
 = RepaParsed returnDoc
 where
   RepaParsed fun = evalFun     f   letLevel aenv
   RepaParsed arr = evalOpenAcc acc letLevel aenv
   exp            = evalExp     e   letLevel aenv

   returnDoc      = parens (first $$ comma $$ second)
   first          = text "traverse"
                <+> (parens arr $$ text "(id)" $$ parens newValDoc)
   second         = text "fold" <+> parens fun <+> parens exp <+> parens arr
   newValDoc      = text "let newVal orig (Z:.pos) "
                 $$ nest 1 (text "| pos == 0" <+> equals <+> exp)
                 $$ nest 1 (text "| otherwise" <+> equals
                <+> parens fun
                <+> parens (text "newVal orig (Z:.(pos-1))")
                <+> parens (text "orig (Z:.(pos-1))"))
                 $$ text "in newVal"


evalPreOpenAcc (Scanl1 f acc) letLevel aenv
 = RepaParsed returnDoc
 where
   RepaParsed fun = evalFun     f   letLevel aenv
   RepaParsed arr = evalOpenAcc acc letLevel aenv

   returnDoc      = text "traverse"
                <+> (parens arr $$ text "(id)" $$ parens newValDoc)
   newValDoc      = text "let newVal orig sh@(Z:.pos) "
                 $$ nest 1 (text "| pos == 0" <+> equals <+> text "orig sh")
                 $$ nest 1 (text "| otherwise" <+> equals
                <+> parens fun
                <+> parens (text "newVal orig (Z:.(pos-1))")
                <+> parens (text "orig sh)"))
                 $$ text "in newVal"


evalPreOpenAcc (Scanr f e acc) letLevel aenv
 = RepaParsed returnDoc
 where
   RepaParsed fun = evalFun     f   letLevel aenv
   RepaParsed arr = evalOpenAcc acc letLevel aenv
   exp            = evalExp     e   letLevel aenv

   returnDoc      = text "traverse"
                <+> (parens arr $$ shapeDoc $$ newValDoc)
   shapeDoc       = parens $ text "\\(Z:.i) -> (Z:.(i+1))"
   newValDoc      = text "let newVal orig sh@(Z:.pos) "
                 $$ nest 1 (text "| pos ==" <+> last <+> equals <+> text "orig sh")
                 $$ nest 1 (text "| otherwise" <+> equals
                <+> parens fun
                <+> parens (text "newVal orig (Z:.(pos+1))")
                <+> parens (text "orig sh)"))
                 $$ text "in newVal"
   last = parens $ text "size $ extent $" <+> arr


{-
evalPreOpenAcc (Scanr' f e acc) letLevel aenv
 = RepaParsed returnString
 where
   returnString       = "(" ++ firstS ++ ", " ++ secondS ++ ")"

   firstS          = "traverse (" ++ arrS ++ ")" ++ " (id) "
                   ++ "(let last = (size $ extent $ " ++ arrS ++ ") - 1 in "
                   ++ "(let newVal (origVal) (Z:.pos) "
                   ++ "| pos == last = " ++ expS ++ " "
                   ++ "| otherwise = " ++ "(" ++ funS ++ ") " ++
                                             "(newVal origVal (Z:.(pos+1))) " ++
                                             "(origVal (Z:.pos)) "
                   ++ "in newVal))"
   secondS         = "fold (" ++ funS ++ ") (" ++ expS ++ ") (" ++ arrS ++ ")"

   RepaParsed funS = evalFun     f   letLevel aenv
   RepaParsed arrS = evalOpenAcc acc letLevel aenv
   expS            = evalExp     e   letLevel aenv

evalPreOpenAcc (Scanr1 f acc) letLevel aenv
 = RepaParsed returnString
 where
   RepaParsed funS = evalFun     f   letLevel aenv
   RepaParsed arrS = evalOpenAcc acc letLevel aenv

   returnString    = "traverse (" ++ arrS ++ ")" ++ " (id) "
                   ++ "(let last = (size $ extent $ " ++ arrS ++ ") - 1 in "
                   ++ "(let newVal (origVal) sh@(Z:.pos) "
                   ++ "| pos == last = origVal sh"
                   ++ "| otherwise = " ++ "(" ++ funS ++ ") " ++
                                             "(newVal origVal (Z:.(pos+1))) " ++
                                             "(origVal (Z:.pos)) "
                   ++ "in newVal))"

--TODO
evalPreOpenAcc (Permute f dftAcc p acc) letLevel aenv
 = error "Permute"
 where
   RepaParsed dftArrS = evalOpenAcc dftAcc letLevel aenv
   RepaParsed arrS    = evalOpenAcc acc    letLevel aenv
   RepaParsed funS    = evalFun     f      letLevel aenv
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

--TODO
evalPreOpenAcc (Backpermute _e _p _acc) _letLevel _aenv
 = error "Backpermute"

--TODO
evalPreOpenAcc (Stencil _sten _bndy _acc) _letLevel _aenv
 = error "Stencil"

--TODO
evalPreOpenAcc (Stencil2 _sten _bndy1 _acc1 _bndy2 _acc2) _letLevel _aenv
 = error "Stencil2"
-}

evalPreOpenAcc _ _ _ = RepaParsed $ text "ERROR"

--------------------
-- FUNCTION NODES --
--------------------

evalFun :: Fun aenv t -> Int-> Val aenv -> RepaParsed t
evalFun f letL aenv = evalOpenFun f 0 letL Empty aenv

evalOpenFun :: OpenFun env aenv t -> Int -> Int -> Val env -> Val aenv -> RepaParsed t
evalOpenFun _ _ _ _ _ = RepaParsed $ text "ERROR"
{-
evalOpenFun (Body e) lamL letL env aenv
 = RepaParsed $ evalOpenExp e lamL letL env aenv
evalOpenFun (Lam f)  lamL letL env aenv
 = RepaParsed ("\\" ++ varName ++ " -> " ++ funS)
 where
   RepaParsed funS = evalOpenFun f (lamL+1) letL (env `Push` (error "Lam")) aenv
   varName = "x" ++ (show lamL)
-}
----------------------
-- EXPRESSION NODES --
----------------------

-- Evaluate an open expression
evalOpenExp :: forall a env aenv .
               OpenExp env aenv a -> Int -> Int -> Val env -> Val aenv -> Doc
evalOpenExp _ _ _ _ _ = text "ERROR"

{-
evalOpenExp (Var idx) lamL letL env _
   = "x" ++ (show varNum)
   where
      varNum = lamL - (getVarNum idx) - 1

evalOpenExp (Const c) _ _ _ _
   = valS ++ " :: " ++ typeS
   where
      valS  = show ((Sugar.toElt c) :: a)
      typeS = (showsTypeRep $ typeOf ((Sugar.toElt c) :: a)) ""

evalOpenExp (Tuple tup) lamL letL env aenv 
   = evalTuple tup lamL letL env aenv

--TODO
evalOpenExp (Prj idx e) lamL letL env aenv 
   = "Prj"
--   = "let (" ++ tupS ++ ") = (" ++ expS ++ ") in tupVar"
--   where
--      expS = evalOpenExp e lamL letL env aenv
--      tupS = (showsTypeRep $ typeOf e) ""

evalOpenExp IndexNil _ _ _ _
   = "Z"

evalOpenExp (IndexCons sh i) lamL letL env aenv 
   = (evalOpenExp sh lamL letL env aenv) ++ " :. ("
     ++ (evalOpenExp i lamL letL env aenv) ++ ")"

evalOpenExp (IndexHead ix) lamL letL env aenv 
   = "case (" ++ expS ++ ") of (_:.h) -> h"
   where
      expS = evalOpenExp ix lamL letL env aenv

evalOpenExp (IndexTail ix) lamL letL env aenv 
   = "case (" ++ expS ++ ") of (t:._) -> t"
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
   = "Repa.size (" ++ arrS ++ ")"
   where
      RepaParsed arrS = evalOpenAcc acc letL aenv
-}

-- Evaluate a closed expression
--
evalExp :: PreExp OpenAcc aenv t -> Int -> Val aenv -> Doc
evalExp e letL aenv = evalOpenExp e 0 letL Empty aenv

------------
-- TUPLES --
------------
{-
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
-}
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
{-
evalPrj :: TupleIdx t e -> t -> String
evalPrj (SuccTupIdx idx) (tup, _) = "(" ++ (evalPrj' (Just idx) tup) ++ "_)"

evalPrj' :: Maybe (TupleIdx t e) -> t -> String
evalPrj' Nothing (tup, _)                 = (evalPrj' Nothing tup) ++ "_, "
evalPrj' Nothing ()                       = ""
evalPrj' (Just (ZeroTupIdx)) (tup, _)     = (evalPrj' Nothing tup) ++ "tupVar, "
evalPrj' (Just (SuccTupIdx idx)) (tup, _) = (evalPrj' (Just idx) tup) ++ "_, "
-}
