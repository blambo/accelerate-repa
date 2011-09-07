import Data.Array.Accelerate
import Data.Array.Accelerate.Repa as Repa

main :: IO ()
main = print $ Repa.run $ generate index0 (\x -> constant (1 :: Int))
