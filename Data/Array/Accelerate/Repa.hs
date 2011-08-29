module Data.Array.Accelerate.Repa
   ( Arrays
   , run
   )
   where

import Data.Array.Accelerate.AST
--import Data.Array.Accelerate.Smart  (Acc)

-- | Used to compile and run an embedded array program using the Repa backend
run :: Arrays a => Acc a -> a
run _ = error "Undefined run program"
