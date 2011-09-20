import Data.Array.Accelerate      as Acc hiding (fst, snd)
import Data.Array.Accelerate.Repa as Repa
   
main :: IO ()
main = do
   --scanl
   putStrLn "---SCANL---"
   putStrLn $ Repa.run $ Acc.scanl (+) (constant (0 :: Int)) $ generate (index1 10) (\_ -> 1)
   --scanl'
   putStrLn "---SCANL'---"
   putStrLn $ Repa.run $ fst $ Acc.scanl' (+) (constant (0 :: Int)) $ generate (index1 10) (\_ -> 1)
   putStrLn $ Repa.run $ snd $ Acc.scanl' (+) (constant (0 :: Int)) $ generate (index1 10) (\_ -> 1)
   --scanl1
   putStrLn "---SCANL1---"
   putStrLn $ Repa.run $ Acc.scanl1 (+) $ generate (index1 10) (\_ -> constant (1 :: Int))
   --scanr
   putStrLn "---SCANR---"
   putStrLn $ Repa.run $ Acc.scanr (+) (constant (0 :: Int)) $ generate (index1 10) (\_ -> 1)
   --scanr
   putStrLn "---SCANR'---"
   putStrLn $ Repa.run $ fst $ Acc.scanr' (+) (constant (0 :: Int)) $ generate (index1 10) (\_ -> 1)
   putStrLn $ Repa.run $ snd $ Acc.scanr' (+) (constant (0 :: Int)) $ generate (index1 10) (\_ -> 1)
   --scanr1
   putStrLn "---SCANR1---"
   putStrLn $ Repa.run $ Acc.scanr1 (+) $ generate (index1 10) (\_ -> constant (1 :: Int))

