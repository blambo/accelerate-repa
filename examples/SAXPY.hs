import Data.Array.Accelerate as Acc
import Data.Array.Accelerate.Repa as Repa

saxpyAcc :: Float -> Vector Float -> Vector Float -> Acc (Vector Float)
saxpyAcc alpha xs ys
  = let
      xs' = use xs
      ys' = use ys
    in
    Acc.zipWith (\x y -> constant alpha * x + y) xs' ys'

main = do
   putStrLn $ Repa.run $ saxpyAcc (1.0) arr1 arr2

arr1 = fromList (Z:.5) [1.0,1.0,1.0,1.0,1.0]
arr2 = fromList (Z:.5) [2.0,2.0,2.0,2.0,2.0]
