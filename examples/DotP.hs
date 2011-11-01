import Data.Array.Accelerate as Acc
import Data.Array.Accelerate.Repa as Repa

dotpAcc :: Vector Float -> Vector Float -> Acc (Scalar Float)
dotpAcc xs ys
  = let
      xs' = use xs
      ys' = use ys
    in
    Acc.fold (+) 0 (Acc.zipWith (*) xs' ys')

main = do
   putStrLn $ Repa.run $ dotpAcc (fromList (Z:.3) [1.0,2.0,3.0]) (fromList (Z:.3) [3.4,2.3,1.9])
