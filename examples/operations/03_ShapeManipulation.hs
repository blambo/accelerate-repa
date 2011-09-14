{-# LANGUAGE TypeOperators #-}

import Data.Array.Accelerate
import Data.Array.Accelerate.Repa as Repa

main :: IO ()
main = do
   putStrLn "---RESHAPE---"
   putStrLn $ Repa.run $ reshape (index2 2 10) $ generate (index2 4 5) $ (\x -> constant (3::Int))
   putStrLn $ Repa.run $ reshape (index2 4 5) $ generate (index2 4 5) $ (\x -> constant (3::Int))

index2 :: Int -> Int -> Exp (Z:.Int:.Int)
index2 x y = lift $ (lift (lift Z):.y):.x
