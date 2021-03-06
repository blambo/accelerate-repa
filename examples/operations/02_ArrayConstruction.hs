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
   putStrLn $ Repa.run $ use $ fromList (Z :. (3 :: Int)) ([1,2,3] :: [Int])

   -- generate
   putStrLn "---GENERATE---"
   putStrLn $ Repa.run $ generate (index2 (index1 3) 3) (\x -> constant (1 :: Int))

   -- replicate
   putStrLn "---REPLICATE---"
   putStrLn $ Repa.run $ replicate replicateShape
                       $ generate  (index0)      (\x -> constant (1 :: Int))
   putStrLn $ Repa.run $ replicate replicateShape'
                       $ generate  (index1 4)    (\x -> constant (2 :: Int))

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

   -- fstA and sndA
   putStrLn "---FSTA and SNDA---"
   putStrLn $ Repa.run
            $ fstA $ pairA (sndA $ pairA testArr testArr) (fstA $ pairA testArr testArr)

testArr :: Acc (Array Z Int)
testArr = generate index0 (\x -> constant 0)

scalarArr :: Int -> Acc (Array Z Int)
scalarArr c = generate index0 (\x -> constant c)

index2 :: Exp (Z:.Int) -> Int -> Exp (Z:.Int:.Int)
index2 sh x = lift ( sh :. x )

replicateShape :: Exp (Z :. Int)
replicateShape = lift $ Z :. (3 :: Int)

replicateShape' :: Exp (Z :. All :. Int)
replicateShape' = lift $ Z :. All :. (3 :: Int)
