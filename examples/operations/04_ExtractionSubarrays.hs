{-# LANGUAGE TypeOperators #-}

import Data.Array.Accelerate
import Data.Array.Accelerate.Repa as Repa

main :: IO ()
main = do
   -- slice
   putStrLn "---SLICE---"
   putStrLn $ Repa.run $ slice (generate (index2 4 4) (\x -> constant (1 :: Int))) (Z:.All:.(3 :: Int))

index2 :: Exp Int -> Exp Int -> Exp (Z:.Int:.Int)
index2 a b = lift $ (index1 a) :. b
