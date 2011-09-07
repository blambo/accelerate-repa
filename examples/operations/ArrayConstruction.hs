{-# LANGUAGE TypeOperators #-}

import Data.Array.Accelerate
import Data.Array.Accelerate.Repa as Repa

import Prelude hiding (replicate)

main :: IO ()
main = do
   print $ Repa.run $ unit $ constant (3 :: Int)
   -- print $ Repa.run $ use  $ constant (3 :: Int)
   print "TODO: 'use'"
   print $ Repa.run $ generate (index2 (index1 3) 3) (\x -> constant (1 :: Int))
--   print $ Repa.run $ replicate (Z :. All :. 3)
--                    $ generate  (index1 3)      (\x -> constant 1)

index2 :: Exp (Z:.Int) -> Int -> Exp (Z:.Int:.Int)
index2 sh x = lift ( sh :. x )
