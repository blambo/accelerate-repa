import Data.Array.Accelerate as Acc
import Data.Array.Accelerate.Repa as Repa

sasumAcc :: Vector Float -> Acc (Scalar Float)
sasumAcc xs
  = Acc.fold (+) 0 . Acc.map abs $ Acc.use xs

main = do
   putStrLn $ Repa.run $ sasumAcc $ (fromList (Z:.10) [1,-1,1,-1,1,-1,1,-1,1,-1])
