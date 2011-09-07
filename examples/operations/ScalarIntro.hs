import Data.Array.Accelerate
import Data.Array.Accelerate.Repa as Repa

main :: IO ()
main = print $ Repa.run $ unit $ constant (3::Int)
