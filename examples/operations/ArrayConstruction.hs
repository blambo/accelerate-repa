{-# LANGUAGE TypeOperators #-}

import Data.Array.Accelerate
import Data.Array.Accelerate.Repa as Repa

import Prelude hiding (replicate)

main :: IO ()
main = do
   -- unit
   putStrLn $ Repa.run $ unit $ constant (3 :: Int)

   -- use
   -- print $ Repa.run $ use  $ constant (3 :: Int)
   putStrLn "TODO: 'use'"

   -- generate
   putStrLn $ Repa.run $ generate (index2 (index1 3) 3) (\x -> constant (1 :: Int))

   -- replicate
   putStrLn "TODO: 'replicate'"
--   print $ Repa.run $ replicate (Z :. All :. 3)
--                    $ generate  (index1 3)      (\x -> constant 1)

   -- pairA
   putStrLn $ Repa.run $ pairA scalarArr scalarArr

   -- fstA
   putStrLn $ Repa.run $ fstA $ pairA scalarArr scalarArr

   -- sndA
   putStrLn $ Repa.run $ sndA $ pairA scalarArr scalarArr


scalarArr :: Acc (Array Z Int)
scalarArr = generate index0 (\x -> constant 1)

index2 :: Exp (Z:.Int) -> Int -> Exp (Z:.Int:.Int)
index2 sh x = lift ( sh :. x )
