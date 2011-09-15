import Data.Array.Accelerate      as Acc
import Data.Array.Accelerate.Repa as Repa
  
main :: IO ()
main = do
   --fold
   putStrLn "---FOLD---"
   putStrLn $ Repa.run $ Acc.fold (-) (constant (3 :: Int)) $ generate (index1 3) (\x -> constant (1 :: Int))
   --fold1
   putStrLn "---FOLD1---"
   putStrLn $ Repa.run $ Acc.fold1 (-) $ generate (index1 3) (\x -> constant (1 :: Int))
   --foldSeg
   putStrLn "---FOLDSEG---"
   --fold1Seg
   putStrLn "---FOLD1SEG---"
