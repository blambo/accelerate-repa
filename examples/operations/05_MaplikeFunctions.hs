import Data.Array.Accelerate      as Acc
import Data.Array.Accelerate.Repa as Repa
 
main :: IO ()
main = do
   putStrLn "---MAP---"
   putStrLn $ Repa.run $ Acc.map (\x -> x) $ use $ fromList Z [3::Int]
   putStrLn $ Repa.run $ Acc.map (\x -> x + 1) $ generate (index1 5) (\_ -> constant (3 :: Int))
   putStrLn "---ZIPWITH---"
   putStrLn $ Repa.run $ Acc.zipWith (\x y -> x + y) (generate (index1 4) (\_ -> constant (4 :: Int))) (generate (index1 5) (\_ -> constant (5 :: Int)))
