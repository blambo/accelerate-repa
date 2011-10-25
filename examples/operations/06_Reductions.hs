import Data.Array.Accelerate      as Acc
import Data.Array.Accelerate.Repa as Repa
  
main :: IO ()
main = do
   --fold
   putStrLn "---FOLD---"
   putStrLn $ Repa.run $ Acc.fold (+) (constant (0 :: Int)) $ generate (index1 3) (\x -> constant (1 :: Int))
   --fold1
   putStrLn "---FOLD1---"
   putStrLn $ Repa.run $ Acc.fold1 (+) $ generate (index1 3) (\x -> constant (1 :: Int))
   --foldSeg
   putStrLn "---FOLDSEG---"
   putStrLn $ Repa.run $ Acc.foldSeg (+) (constant 0) arr1 arr1seg
   --fold1Seg
   putStrLn "---FOLD1SEG---"
   putStrLn $ Repa.run $ Acc.fold1Seg (+) arr1 arr1seg


arr1 = use $ fromList (Z:.10) ([1,2,3,4,5,6,7,8,9,10] :: [Int])

arr1seg = use $ fromList (Z:.3) ([2,3,5])
