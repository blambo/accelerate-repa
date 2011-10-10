{-# LANGUAGE TypeOperators, ScopedTypeVariables #-}
import Data.Array.Accelerate as Acc
import Data.Array.Accelerate.Repa as Repa
import Data.Array.Accelerate.Interpreter as Interpreter

main :: IO ()
main = do
   putStrLn $ show $ Interpreter.run $
      permute comb dftArr perm srcArr

-- comb :: Int -> Int -> Int
comb = (+)

perm :: Exp (Z:. Int:. Int) -> Exp (Z:. Int)
perm ix = let (Z:.y:.(x :: Exp Int)) = unlift ix in (index1 y)

dftArr :: Acc (Array DIM1 Int)
dftArr = generate (index1 5) (\_ -> 0)

srcArr :: Acc (Array DIM2 Int)
srcArr = generate (index2 (index1 5) 5) (\_ -> 1)

index2 :: Exp (Z:.Int) -> Int -> Exp (Z:.Int:.Int)
index2 sh x = lift (sh :.x)
