{-# LANGUAGE TypeOperators #-}

import Data.Array.Accelerate
import Data.Array.Accelerate.Repa as Repa

import Prelude hiding (replicate)

main :: IO ()
main = do
   -- unit
   putStrLn "---UNIT---"
   putStrLn $ Repa.run $ unit $ constant (3 :: Int)

   -- use
   putStrLn "---USE---"
   -- print $ Repa.run $ use  $ constant (3 :: Int)
   putStrLn "TODO: 'use'"

   -- generate
   putStrLn "---GENERATE---"
   putStrLn $ Repa.run $ generate (index2 (index1 3) 3) (\x -> constant (1 :: Int))

   -- replicate
   putStrLn "---REPLICATE---"
   putStrLn "TODO: 'replicate'"
--   print $ Repa.run $ replicate (Z :. All :. 3)
--                    $ generate  (index1 3)      (\x -> constant 1)

   -- pairA
   putStrLn "---PAIRA---"
   putStrLn $ Repa.run $ pairA testArr testArr
   putStrLn $ Repa.run $ pairA (scalarArr 1) (scalarArr 1)
   putStrLn $ Repa.run $ pairA (scalarArr 2) (scalarArr 3)

   -- fstA
   putStrLn "---FSTA---"
   putStrLn $ Repa.run $ fstA $ pairA testArr testArr
   putStrLn $ Repa.run $ fstA $ pairA (scalarArr 4) (scalarArr 5)

   -- sndA
   putStrLn "---SNDA---"
   putStrLn $ Repa.run $ sndA $ pairA testArr testArr
   putStrLn $ Repa.run $ sndA $ pairA (scalarArr 6) (scalarArr 7)

testArr :: Acc (Array Z Int)
testArr = generate index0 (\x -> constant 0)

scalarArr :: Int -> Acc (Array Z Int)
scalarArr c = generate index0 (\x -> constant c)

index2 :: Exp (Z:.Int) -> Int -> Exp (Z:.Int:.Int)
index2 sh x = lift ( sh :. x )
