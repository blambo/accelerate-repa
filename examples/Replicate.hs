{-# LANGUAGE TypeOperators #-}
import Data.Array.Accelerate as Acc
import Data.Array.Accelerate.Repa as Repa
import Prelude hiding (replicate)


main = do
--   putStrLn $ Repa.run $ replicate replicateShape
--                       $ generate  (index0)      (\x -> constant (1 :: Int))
   putStrLn $ Repa.run $ replicate replicateShape'
                       $ generate  (index1 4)    (\x -> constant (2 :: Int))

replicateShape :: Exp (Z :. Int)
replicateShape = lift $ Z :. (3 :: Int)

replicateShape' :: Exp (Z :. All :. Int)
replicateShape' = lift $ Z :. All :. (3 :: Int)

