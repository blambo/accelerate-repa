import Data.Array.Accelerate      as Acc hiding (fst, snd)
import Data.Array.Accelerate.Repa as Repa
    
main :: IO ()
main = do
   --cond
   putStrLn $ Repa.run $ cond (two ==* (one + one)) (generate (index1 10) (\_ -> constant (1 :: Int))) (generate (index1 10) (\_ -> constant (2 :: Int)))

one :: Exp Int
one = 1
two :: Exp Int
two = 2
