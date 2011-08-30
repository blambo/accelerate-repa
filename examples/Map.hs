import Data.Array.Accelerate      as Acc
import Data.Array.Accelerate.Repa as Repa

main :: IO ()
main = do
   Repa.run $ Acc.map (\x -> x) $ use $ fromList Z [3::Int]
