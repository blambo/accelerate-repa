import Data.Array.Accelerate
import Data.Array.Accelerate.Repa as Repa

main :: IO ()
main = putStrLn $ Repa.run $ unit $ constant (3::Int)
