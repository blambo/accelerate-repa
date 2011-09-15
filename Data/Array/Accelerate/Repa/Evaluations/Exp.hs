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
evalOpenExp :: forall a env aenv .  OpenExp env aenv a
                                 -> Val env -> Val aenv -> String

evalOpenExp (Var idx) env _
   = "Var"

evalOpenExp (Const c) _ _
   = show ((Sugar.toElt c) :: a)

evalOpenExp (Tuple tup) env aenv 
   = evalTuple tup env aenv

evalOpenExp (Prj idx e) env aenv 
   = "Prj"

evalOpenExp IndexNil _env _aenv 
   = "Z"

evalOpenExp (IndexCons sh i) env aenv 
   = (evalOpenExp sh env aenv) ++ " :. ("
     ++ (evalOpenExp i env aenv) ++ suffix
   where
      suffix = case i of
                  Const _   -> " :: Int)"
                  otherwise -> ")"

evalOpenExp (IndexHead ix) env aenv 
   = "IndexHead"

evalOpenExp (IndexTail ix) env aenv 
   = "IndexTail"

evalOpenExp (IndexAny) _ _
   = "Any"

evalOpenExp (Cond c t e) env aenv 
   = "Cond"

evalOpenExp (PrimConst c) _ _
   = "PrimConst"

evalOpenExp (PrimApp p arg) env aenv 
   = evalPrim p argS
   where
      argS = evalOpenExp arg env aenv

evalOpenExp (IndexScalar acc ix) env aenv 
   = "IndexScalar"

evalOpenExp (Shape acc) _ aenv 
   = "Shape"

evalOpenExp (Size acc) _ aenv 
   = "Size"

-- Evaluate a closed expression
--
evalExp :: PreExp OpenAcc aenv t -> Val aenv -> String
evalExp e aenv = evalOpenExp e Empty aenv

evalTuple :: Tuple (OpenExp env aenv) t -> Val env -> Val aenv -> String
evalTuple tup env aenv = "(" ++ evalTuple' tup env aenv ++ ")"

evalTuple' :: Tuple (OpenExp env aenv) t1 -> Val env -> Val aenv -> String
evalTuple' NilTup _env _aenv = ""
evalTuple' (e1 `SnocTup` e2) env aenv
   = case tupS of
      ""        -> evalOpenExp e2 env aenv
      otherwise -> tupS ++ ", " ++ evalOpenExp e2 env aenv
   where
      tupS = evalTuple' e1 env aenv
