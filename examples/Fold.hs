import Data.Array.Accelerate      as Acc
import Data.Array.Accelerate.Repa as Repa

main :: IO ()
main = do
   print $ Repa.run $ Acc.fold (\x y -> x) 1 $ use $ fromList (Z:.(1::Int)) [3::Int]
