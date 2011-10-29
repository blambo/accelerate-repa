{-# LANGUAGE TypeOperators #-}
import Data.Array.Accelerate as Acc
import Data.Array.Accelerate.Repa as Repa
import Data.Array.Accelerate.Interpreter as Interpreter
import Prelude hiding (replicate)

main = do
--   putStrLn $ Repa.run $ slice arr1 sl1
--   putStrLn $ Repa.run $ slice arr2 sl2
   putStrLn $ show $ Interpreter.run $ slice arr2 sl2
--   putStrLn $ Repa.run $ slice arr2 sl1
   

arr1 = generate (index1 10) (\_ -> constant (2 :: Int))
arr2 = generate (index2 (index1 10) 10) (\_ -> constant (3 :: Int))

sl1 = lift $ Z :. (0 :: Int)
sl2 = lift $ Z :. (5 :: Int) :. (0 :: Int)

index2 :: Exp (Z:.Int) -> Int -> Exp (Z:.Int:.Int)
index2 sh x = lift (sh :.x)

