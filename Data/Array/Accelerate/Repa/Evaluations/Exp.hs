{-# LANGUAGE CPP, GADTs, BangPatterns, TypeOperators, PatternGuards #-}
{-# LANGUAGE TypeFamilies, ScopedTypeVariables, FlexibleContexts #-}

-- |
-- Module     : Data.Array.Accelerate.Repa.Evaluations.Exp
--
-- Maintainer : Ben Lambert-Smith <blambo+github@gmail.com>
--
-- Defines evaluation of Accelerate expressions,
--  borrowed in part from the Accelerate Interpreter module

module Data.Array.Accelerate.Repa.Evaluations.Exp
   ( evalOpenExp
   , evalExp
   )
   where

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Array.Sugar as Sugar
import Data.Array.Accelerate.Tuple

import Data.Array.Accelerate.Repa.Evaluations.Prim

-- Evaluate an open expression
evalOpenExp :: forall a env aenv .
               OpenExp env aenv a -> Int -> Val env -> Val aenv -> String

evalOpenExp (Var idx) lamLevel env _
   = "x" ++ (show varNum)
   where
      varNum = lamLevel - (getVarNum idx) - 1

evalOpenExp (Const c) _ _ _
   = show ((Sugar.toElt c) :: a)

evalOpenExp (Tuple tup) lamLevel env aenv 
   = evalTuple tup lamLevel env aenv

evalOpenExp (Prj idx e) _lamLevel env aenv 
   = "Prj"

evalOpenExp IndexNil _ _ _
   = "Z"

evalOpenExp (IndexCons sh i) lamLevel env aenv 
   = (evalOpenExp sh lamLevel env aenv) ++ " :. ("
     ++ (evalOpenExp i lamLevel env aenv) ++ suffix
   where
      suffix = case i of
                  Const _   -> " :: Int)"
                  otherwise -> ")"

evalOpenExp (IndexHead ix) lamLevel env aenv 
   = "IndexHead"

evalOpenExp (IndexTail ix) lamLevel env aenv 
   = "IndexTail"

evalOpenExp (IndexAny) _ _ _
   = "Any"

evalOpenExp (Cond c t e) lamLevel env aenv 
   = "Cond"

evalOpenExp (PrimConst c) _ _ _
   = "PrimConst"

evalOpenExp (PrimApp p arg) lamLevel env aenv 
   = evalPrim p argS
   where
      argS = evalOpenExp arg lamLevel env aenv

evalOpenExp (IndexScalar acc ix) lamLevel env aenv 
   = "IndexScalar"

evalOpenExp (Shape acc) _lamLevel _ aenv 
   = "Shape"

evalOpenExp (Size acc) _lamLevel _ aenv 
   = "Size"

-- Evaluate a closed expression
--
evalExp :: PreExp OpenAcc aenv t -> Val aenv -> String
evalExp e aenv = evalOpenExp e 0 Empty aenv

evalTuple :: Tuple (OpenExp env aenv) t -> Int -> Val env -> Val aenv -> String
evalTuple tup lamLevel env aenv = "(" ++ evalTuple' tup lamLevel env aenv ++ ")"

evalTuple' :: Tuple (OpenExp env aenv) t1 -> Int -> Val env -> Val aenv -> String
evalTuple' NilTup _ _env _aenv = ""
evalTuple' (e1 `SnocTup` e2) lamLevel env aenv
   = case tupS of
      ""        -> evalOpenExp e2 lamLevel env aenv
      otherwise -> tupS ++ ", " ++ evalOpenExp e2 lamLevel env aenv
   where
      tupS = evalTuple' e1 lamLevel env aenv

getVarNum :: Idx env t -> Int
getVarNum ZeroIdx = 0
getVarNum (SuccIdx idx) = 1 + (getVarNum idx)
