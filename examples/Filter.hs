import Data.Array.Accelerate as Acc
import Data.Array.Accelerate.Repa as Repa

filterAcc :: Elt a
          => (Exp a -> Exp Bool)
          -> Vector a
          -> Acc (Vector a)
filterAcc p vec
  = let arr              = Acc.use vec
        flags            = Acc.map (boolToInt . p) arr
        (targetIdx, len) = Acc.scanl' (+) 0 flags
        arr'             = Acc.backpermute (index1 $ the len) id arr
    in
    Acc.permute const arr' (\ix -> flags!ix ==* 0 ? (ignore, index1 $ targetIdx!ix)) arr

main = do
   putStrLn $ Repa.run $ filterAcc (>* 0) (fromList (Z:.10) ([1,2,-3,4,-5,-6,7,8,-9,-10] :: [Int]))
