{-# LANGUAGE GADTs #-}
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

-- Evaluate an open expression
evalOpenExp :: OpenExp env aenv a -> Val env -> Val aenv -> a

evalOpenExp (Var idx) env _
   = error "Var"

evalOpenExp (Const c) _ _
   = error "Const"

evalOpenExp (Tuple tup) env aenv 
   = error "Tuple"

evalOpenExp (Prj idx e) env aenv 
   = error "Prj"

evalOpenExp IndexNil _env _aenv 
   = error "IndexNil"

evalOpenExp (IndexCons sh i) env aenv 
   = error "IndexCons"

evalOpenExp (IndexHead ix) env aenv 
   = error "IndexHead"

evalOpenExp (IndexTail ix) env aenv 
   = error "IndexTail"

evalOpenExp (IndexAny) _ _
   = error "IndexAny"

evalOpenExp (Cond c t e) env aenv 
   = error "Cond"

evalOpenExp (PrimConst c) _ _
   = error "PrimConst"

evalOpenExp (PrimApp p arg) env aenv 
   = error "PrimApp"

evalOpenExp (IndexScalar acc ix) env aenv 
   = error "IndexScalar"

evalOpenExp (Shape acc) _ aenv 
   = error "Shape"

evalOpenExp (Size acc) _ aenv 
   = error "Size"

-- Evaluate a closed expression
--
evalExp :: Exp aenv t -> Val aenv -> t
evalExp e aenv = evalOpenExp e Empty aenv
