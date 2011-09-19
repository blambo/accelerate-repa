import Data.Array.Accelerate      as Acc
import Data.Array.Accelerate.Repa as Repa
   
main :: IO ()
main = do
   --scanl
   putStrLn "---SCANL---"
   putStrLn $ Repa.run $ Acc.scanl (+) (constant (0 :: Int)) $ generate (index1 10) (\_ -> 1)
   --scanl'
   --scanl1
   --scanr
   --scanr'
   --scanr1

