import Data.Array.Accelerate
import Data.Array.Accelerate.Repa as Repa

main = do
   print $ Repa.run $ unit $ index0
   print $ Repa.run $ unit $ index1 5
   print $ Repa.run $ unit $ lift Any
