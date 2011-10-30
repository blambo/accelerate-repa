import Random

import qualified Prelude
import Prelude               hiding (zip, map, scanl, scanr, zipWith, fst)
import Data.Bits             hiding (shiftL, shiftR, bit, testBit)
import Data.Array.Accelerate as Acc
import Data.Array.Accelerate.Repa as Repa

import Data.List             (sort)
import Data.Array.Unboxed    (IArray, UArray, listArray, bounds, elems)
import System.Random.MWC
import Unsafe.Coerce


-- Radix sort
-- ----------

class Elt e => Radix e where
    passes :: Exp e   -> Int                -- Haskell-side control needs to know this
    radix  :: Exp Int -> Exp e -> Exp Int

instance Radix Int where                    -- may be 32- or 64-bit
    passes    = bitSize . (undefined :: Exp t -> t)
    radix i e = i ==* (passes' e - 1) ? (radix' (e `xor` minBound), radix' e)
      where
        radix' x = (x `shiftR` i) .&. 1
        passes'  = constant . passes

--
-- A simple (parallel) radix sort implementation [1].
--
-- [1] G. E. Blelloch. "Prefix sums and their applications." Technical Report
--     CMU-CS-90-190. Carnegie Mellon University. 1990.
--
sortAcc :: Radix a => Vector a -> Acc (Vector a)
sortAcc = sortAccBy id

sortAccBy :: (Elt a, Radix r) => (Exp a -> Exp r) -> Vector a -> Acc (Vector a)
sortAccBy rdx arr = foldr1 (>->) (Prelude.map radixPass [0..p-1]) (use arr)
  where
    n = constant $ (arraySize $ arrayShape arr) - 1
    p = passes . rdx . (undefined :: Vector e -> Exp e) $ arr

    deal f x      = let (a,b) = unlift x in (f ==* 0) ? (a,b)
    radixPass k v = let flags = map (radix (constant k) . rdx) v
                        idown = prescanl (+) 0 . map (xor 1) $ flags
                        iup   = map (n-) . prescanr (+) 0    $ flags
                        index = zipWith deal flags (zip idown iup)
                    in
                    permute const v (\ix -> index1 (index!ix)) v

-- Main
-- ----

main = do
   putStrLn $ Repa.run $ sortAcc $ arr

arr = fromList (Z:.10) ([3,4,5,1,6,9,8,7,2,10] :: [Int])
