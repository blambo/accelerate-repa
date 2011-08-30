import Data.Array.Accelerate
import Data.Array.Accelerate.Repa as Repa

main :: IO ()
main = do
   Repa.run $ use $ fromList Z [3::Int]
