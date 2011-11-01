{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
import Data.Array.Repa as Repa
import Data.Bits -- required for Prim ops
import Data.Char -- required for Prim ops
import Data.Int  -- required for Prim ops
import Data.List (sortBy)  -- required for permute
import Data.Ord  (compare) -- required for permute
 
main = putStrLn $ show $
 let y0 = (let y0 = (fromList (Z :. (10 :: Int)) ([3,4,5,1,6,9,8,7,2,10] :: [Int])) -- binding of Apply block
           in
            let y1 = (y0) -- Let block
            in
             let y2 = (let y2 = (Repa.map (\x0 -> (if ((0 :: Int) == ((32 :: Int) - (1 :: Int)))
                                                    then (((((x0)) `xor` (minBound)) `shiftR` (0 :: Int)) .&. (1 :: Int))
                                                    else ((((x0)) `shiftR` (0 :: Int)) .&. (1 :: Int))))
                                          (y1)) -- Let block
                       in
                        Repa.zipWith (\x0 -> \x1 -> (if (((x0)) == (0 :: Int))
                                                      then (let ((tVar, _)) = ((x1)) in tVar)
                                                      else (let ((_, tVar)) = ((x1)) in tVar)))
                                     (y2)
                                     (Repa.zipWith (\x0 -> \x1 -> (((x0), (x1))))
                                                   (let (y3, y4) = (let res = traverse (Repa.map (\x0 -> ((1 :: Int) `xor` ((x0))))
                                                                                                 (y2))
                                                                                       (\(Z:.i) -> (Z:.(i+1)))
                                                                                       (let newVal orig (Z:.pos) | pos == 0 = 0 :: Int
                                                                                                                  | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                (newVal orig (Z:.(pos-1)))
                                                                                                                                (orig (Z:.(pos-1)))
                                                                                        in newVal)
                                                                    in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig sh -> orig sh), fromList Z [(res!(Z:.((size $ extent res)-1)))]))
                                                    in
                                                     y3)
                                                   (Repa.map (\x0 -> ((9 :: Int) - ((x0))))
                                                             (let (y3, y4) = (let res = traverse (y2)
                                                                                                 (\(Z:.i) -> (Z:.(i+1)))
                                                                                                 (let newVal orig sh@(Z:.pos) | pos == (size $ extent $ y2) = 0 :: Int
                                                                                                                              | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                            (newVal orig (Z:.(pos+1)))
                                                                                                                                            (orig sh)
                                                                                                  in newVal)
                                                                              in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig (Z:.pos) -> orig (Z:.(pos+1)))
                                                                                  ,
                                                                                  fromList Z [(res!(Z:.0))]))
                                                              in
                                                               y3)))) -- Let block
             in
              let srcArr = y1
                  dftArr = y1
                  perm   = (\x0 -> (Z :. ((y2) ! (Z:.(x0)))))
                  comb   = (\x0 -> \x1 -> ((x0)))
                  permute idx comb dftArr perm srcArr
                   | idx >= (size $ extent srcArr) = dftArr
                   | otherwise = let (Z:.srcIdx) = fromIndex (extent srcArr) idx
                                     newArr = fromFunction (extent dftArr)
                                                           (\sh -> case sh == (perm srcIdx) of
                                                            True  -> (dftArr ! (perm srcIdx)) `comb` (srcArr ! (Z:.srcIdx))
                                                            False -> index dftArr sh)
                                 in permute (idx+1) comb newArr perm srcArr
              in
               permute 0 comb dftArr perm srcArr) -- Let block
 in
  let y0 = (y0) -- binding of Apply block
  in
   let y1 = (let y0 = (y0) -- binding of Apply block
             in
              let y1 = (y0) -- Let block
              in
               let y2 = (let y2 = (Repa.map (\x0 -> (if ((1 :: Int) == ((32 :: Int) - (1 :: Int)))
                                                      then (((((x0)) `xor` (minBound)) `shiftR` (1 :: Int)) .&. (1 :: Int))
                                                      else ((((x0)) `shiftR` (1 :: Int)) .&. (1 :: Int))))
                                            (y1)) -- Let block
                         in
                          Repa.zipWith (\x0 -> \x1 -> (if (((x0)) == (0 :: Int))
                                                        then (let ((tVar, _)) = ((x1)) in tVar)
                                                        else (let ((_, tVar)) = ((x1)) in tVar)))
                                       (y2)
                                       (Repa.zipWith (\x0 -> \x1 -> (((x0), (x1))))
                                                     (let (y3, y4) = (let res = traverse (Repa.map (\x0 -> ((1 :: Int) `xor` ((x0))))
                                                                                                   (y2))
                                                                                         (\(Z:.i) -> (Z:.(i+1)))
                                                                                         (let newVal orig (Z:.pos) | pos == 0 = 0 :: Int
                                                                                                                    | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                  (newVal orig (Z:.(pos-1)))
                                                                                                                                  (orig (Z:.(pos-1)))
                                                                                          in newVal)
                                                                      in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig sh -> orig sh), fromList Z [(res!(Z:.((size $ extent res)-1)))]))
                                                      in
                                                       y3)
                                                     (Repa.map (\x0 -> ((9 :: Int) - ((x0))))
                                                               (let (y3, y4) = (let res = traverse (y2)
                                                                                                   (\(Z:.i) -> (Z:.(i+1)))
                                                                                                   (let newVal orig sh@(Z:.pos) | pos == (size $ extent $ y2) = 0 :: Int
                                                                                                                                | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                              (newVal orig (Z:.(pos+1)))
                                                                                                                                              (orig sh)
                                                                                                    in newVal)
                                                                                in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig (Z:.pos) -> orig (Z:.(pos+1)))
                                                                                    ,
                                                                                    fromList Z [(res!(Z:.0))]))
                                                                in
                                                                 y3)))) -- Let block
               in
                let srcArr = y1
                    dftArr = y1
                    perm   = (\x0 -> (Z :. ((y2) ! (Z:.(x0)))))
                    comb   = (\x0 -> \x1 -> ((x0)))
                    permute idx comb dftArr perm srcArr
                     | idx >= (size $ extent srcArr) = dftArr
                     | otherwise = let (Z:.srcIdx) = fromIndex (extent srcArr) idx
                                       newArr = fromFunction (extent dftArr)
                                                             (\sh -> case sh == (perm srcIdx) of
                                                              True  -> (dftArr ! (perm srcIdx)) `comb` (srcArr ! (Z:.srcIdx))
                                                              False -> index dftArr sh)
                                   in permute (idx+1) comb newArr perm srcArr
                in
                 permute 0 comb dftArr perm srcArr) -- Let block
   in
    let y0 = (y1) -- binding of Apply block
    in
     let y1 = (let y0 = (y0) -- binding of Apply block
               in
                let y1 = (y0) -- Let block
                in
                 let y2 = (let y2 = (Repa.map (\x0 -> (if ((2 :: Int) == ((32 :: Int) - (1 :: Int)))
                                                        then (((((x0)) `xor` (minBound)) `shiftR` (2 :: Int)) .&. (1 :: Int))
                                                        else ((((x0)) `shiftR` (2 :: Int)) .&. (1 :: Int))))
                                              (y1)) -- Let block
                           in
                            Repa.zipWith (\x0 -> \x1 -> (if (((x0)) == (0 :: Int))
                                                          then (let ((tVar, _)) = ((x1)) in tVar)
                                                          else (let ((_, tVar)) = ((x1)) in tVar)))
                                         (y2)
                                         (Repa.zipWith (\x0 -> \x1 -> (((x0), (x1))))
                                                       (let (y3, y4) = (let res = traverse (Repa.map (\x0 -> ((1 :: Int) `xor` ((x0))))
                                                                                                     (y2))
                                                                                           (\(Z:.i) -> (Z:.(i+1)))
                                                                                           (let newVal orig (Z:.pos) | pos == 0 = 0 :: Int
                                                                                                                      | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                    (newVal orig (Z:.(pos-1)))
                                                                                                                                    (orig (Z:.(pos-1)))
                                                                                            in newVal)
                                                                        in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig sh -> orig sh), fromList Z [(res!(Z:.((size $ extent res)-1)))]))
                                                        in
                                                         y3)
                                                       (Repa.map (\x0 -> ((9 :: Int) - ((x0))))
                                                                 (let (y3, y4) = (let res = traverse (y2)
                                                                                                     (\(Z:.i) -> (Z:.(i+1)))
                                                                                                     (let newVal orig sh@(Z:.pos) | pos == (size $ extent $ y2) = 0 :: Int
                                                                                                                                  | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                (newVal orig (Z:.(pos+1)))
                                                                                                                                                (orig sh)
                                                                                                      in newVal)
                                                                                  in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig (Z:.pos) -> orig (Z:.(pos+1)))
                                                                                      ,
                                                                                      fromList Z [(res!(Z:.0))]))
                                                                  in
                                                                   y3)))) -- Let block
                 in
                  let srcArr = y1
                      dftArr = y1
                      perm   = (\x0 -> (Z :. ((y2) ! (Z:.(x0)))))
                      comb   = (\x0 -> \x1 -> ((x0)))
                      permute idx comb dftArr perm srcArr
                       | idx >= (size $ extent srcArr) = dftArr
                       | otherwise = let (Z:.srcIdx) = fromIndex (extent srcArr) idx
                                         newArr = fromFunction (extent dftArr)
                                                               (\sh -> case sh == (perm srcIdx) of
                                                                True  -> (dftArr ! (perm srcIdx)) `comb` (srcArr ! (Z:.srcIdx))
                                                                False -> index dftArr sh)
                                     in permute (idx+1) comb newArr perm srcArr
                  in
                   permute 0 comb dftArr perm srcArr) -- Let block
     in
      let y0 = (y1) -- binding of Apply block
      in
       let y1 = (let y0 = (y0) -- binding of Apply block
                 in
                  let y1 = (y0) -- Let block
                  in
                   let y2 = (let y2 = (Repa.map (\x0 -> (if ((3 :: Int) == ((32 :: Int) - (1 :: Int)))
                                                          then (((((x0)) `xor` (minBound)) `shiftR` (3 :: Int)) .&. (1 :: Int))
                                                          else ((((x0)) `shiftR` (3 :: Int)) .&. (1 :: Int))))
                                                (y1)) -- Let block
                             in
                              Repa.zipWith (\x0 -> \x1 -> (if (((x0)) == (0 :: Int))
                                                            then (let ((tVar, _)) = ((x1)) in tVar)
                                                            else (let ((_, tVar)) = ((x1)) in tVar)))
                                           (y2)
                                           (Repa.zipWith (\x0 -> \x1 -> (((x0), (x1))))
                                                         (let (y3, y4) = (let res = traverse (Repa.map (\x0 -> ((1 :: Int) `xor` ((x0))))
                                                                                                       (y2))
                                                                                             (\(Z:.i) -> (Z:.(i+1)))
                                                                                             (let newVal orig (Z:.pos) | pos == 0 = 0 :: Int
                                                                                                                        | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                      (newVal orig (Z:.(pos-1)))
                                                                                                                                      (orig (Z:.(pos-1)))
                                                                                              in newVal)
                                                                          in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig sh -> orig sh), fromList Z [(res!(Z:.((size $ extent res)-1)))]))
                                                          in
                                                           y3)
                                                         (Repa.map (\x0 -> ((9 :: Int) - ((x0))))
                                                                   (let (y3, y4) = (let res = traverse (y2)
                                                                                                       (\(Z:.i) -> (Z:.(i+1)))
                                                                                                       (let newVal orig sh@(Z:.pos) | pos == (size $ extent $ y2) = 0 :: Int
                                                                                                                                    | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                  (newVal orig (Z:.(pos+1)))
                                                                                                                                                  (orig sh)
                                                                                                        in newVal)
                                                                                    in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig (Z:.pos) -> orig (Z:.(pos+1)))
                                                                                        ,
                                                                                        fromList Z [(res!(Z:.0))]))
                                                                    in
                                                                     y3)))) -- Let block
                   in
                    let srcArr = y1
                        dftArr = y1
                        perm   = (\x0 -> (Z :. ((y2) ! (Z:.(x0)))))
                        comb   = (\x0 -> \x1 -> ((x0)))
                        permute idx comb dftArr perm srcArr
                         | idx >= (size $ extent srcArr) = dftArr
                         | otherwise = let (Z:.srcIdx) = fromIndex (extent srcArr) idx
                                           newArr = fromFunction (extent dftArr)
                                                                 (\sh -> case sh == (perm srcIdx) of
                                                                  True  -> (dftArr ! (perm srcIdx)) `comb` (srcArr ! (Z:.srcIdx))
                                                                  False -> index dftArr sh)
                                       in permute (idx+1) comb newArr perm srcArr
                    in
                     permute 0 comb dftArr perm srcArr) -- Let block
       in
        let y0 = (y1) -- binding of Apply block
        in
         let y1 = (let y0 = (y0) -- binding of Apply block
                   in
                    let y1 = (y0) -- Let block
                    in
                     let y2 = (let y2 = (Repa.map (\x0 -> (if ((4 :: Int) == ((32 :: Int) - (1 :: Int)))
                                                            then (((((x0)) `xor` (minBound)) `shiftR` (4 :: Int)) .&. (1 :: Int))
                                                            else ((((x0)) `shiftR` (4 :: Int)) .&. (1 :: Int))))
                                                  (y1)) -- Let block
                               in
                                Repa.zipWith (\x0 -> \x1 -> (if (((x0)) == (0 :: Int))
                                                              then (let ((tVar, _)) = ((x1)) in tVar)
                                                              else (let ((_, tVar)) = ((x1)) in tVar)))
                                             (y2)
                                             (Repa.zipWith (\x0 -> \x1 -> (((x0), (x1))))
                                                           (let (y3, y4) = (let res = traverse (Repa.map (\x0 -> ((1 :: Int) `xor` ((x0))))
                                                                                                         (y2))
                                                                                               (\(Z:.i) -> (Z:.(i+1)))
                                                                                               (let newVal orig (Z:.pos) | pos == 0 = 0 :: Int
                                                                                                                          | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                        (newVal orig (Z:.(pos-1)))
                                                                                                                                        (orig (Z:.(pos-1)))
                                                                                                in newVal)
                                                                            in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig sh -> orig sh), fromList Z [(res!(Z:.((size $ extent res)-1)))]))
                                                            in
                                                             y3)
                                                           (Repa.map (\x0 -> ((9 :: Int) - ((x0))))
                                                                     (let (y3, y4) = (let res = traverse (y2)
                                                                                                         (\(Z:.i) -> (Z:.(i+1)))
                                                                                                         (let newVal orig sh@(Z:.pos) | pos == (size $ extent $ y2) = 0 :: Int
                                                                                                                                      | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                    (newVal orig (Z:.(pos+1)))
                                                                                                                                                    (orig sh)
                                                                                                          in newVal)
                                                                                      in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig (Z:.pos) -> orig (Z:.(pos+1)))
                                                                                          ,
                                                                                          fromList Z [(res!(Z:.0))]))
                                                                      in
                                                                       y3)))) -- Let block
                     in
                      let srcArr = y1
                          dftArr = y1
                          perm   = (\x0 -> (Z :. ((y2) ! (Z:.(x0)))))
                          comb   = (\x0 -> \x1 -> ((x0)))
                          permute idx comb dftArr perm srcArr
                           | idx >= (size $ extent srcArr) = dftArr
                           | otherwise = let (Z:.srcIdx) = fromIndex (extent srcArr) idx
                                             newArr = fromFunction (extent dftArr)
                                                                   (\sh -> case sh == (perm srcIdx) of
                                                                    True  -> (dftArr ! (perm srcIdx)) `comb` (srcArr ! (Z:.srcIdx))
                                                                    False -> index dftArr sh)
                                         in permute (idx+1) comb newArr perm srcArr
                      in
                       permute 0 comb dftArr perm srcArr) -- Let block
         in
          let y0 = (y1) -- binding of Apply block
          in
           let y1 = (let y0 = (y0) -- binding of Apply block
                     in
                      let y1 = (y0) -- Let block
                      in
                       let y2 = (let y2 = (Repa.map (\x0 -> (if ((5 :: Int) == ((32 :: Int) - (1 :: Int)))
                                                              then (((((x0)) `xor` (minBound)) `shiftR` (5 :: Int)) .&. (1 :: Int))
                                                              else ((((x0)) `shiftR` (5 :: Int)) .&. (1 :: Int))))
                                                    (y1)) -- Let block
                                 in
                                  Repa.zipWith (\x0 -> \x1 -> (if (((x0)) == (0 :: Int))
                                                                then (let ((tVar, _)) = ((x1)) in tVar)
                                                                else (let ((_, tVar)) = ((x1)) in tVar)))
                                               (y2)
                                               (Repa.zipWith (\x0 -> \x1 -> (((x0), (x1))))
                                                             (let (y3, y4) = (let res = traverse (Repa.map (\x0 -> ((1 :: Int) `xor` ((x0))))
                                                                                                           (y2))
                                                                                                 (\(Z:.i) -> (Z:.(i+1)))
                                                                                                 (let newVal orig (Z:.pos) | pos == 0 = 0 :: Int
                                                                                                                            | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                          (newVal orig (Z:.(pos-1)))
                                                                                                                                          (orig (Z:.(pos-1)))
                                                                                                  in newVal)
                                                                              in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig sh -> orig sh), fromList Z [(res!(Z:.((size $ extent res)-1)))]))
                                                              in
                                                               y3)
                                                             (Repa.map (\x0 -> ((9 :: Int) - ((x0))))
                                                                       (let (y3, y4) = (let res = traverse (y2)
                                                                                                           (\(Z:.i) -> (Z:.(i+1)))
                                                                                                           (let newVal orig sh@(Z:.pos) | pos == (size $ extent $ y2) = 0 :: Int
                                                                                                                                        | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                      (newVal orig (Z:.(pos+1)))
                                                                                                                                                      (orig sh)
                                                                                                            in newVal)
                                                                                        in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig (Z:.pos) -> orig (Z:.(pos+1)))
                                                                                            ,
                                                                                            fromList Z [(res!(Z:.0))]))
                                                                        in
                                                                         y3)))) -- Let block
                       in
                        let srcArr = y1
                            dftArr = y1
                            perm   = (\x0 -> (Z :. ((y2) ! (Z:.(x0)))))
                            comb   = (\x0 -> \x1 -> ((x0)))
                            permute idx comb dftArr perm srcArr
                             | idx >= (size $ extent srcArr) = dftArr
                             | otherwise = let (Z:.srcIdx) = fromIndex (extent srcArr) idx
                                               newArr = fromFunction (extent dftArr)
                                                                     (\sh -> case sh == (perm srcIdx) of
                                                                      True  -> (dftArr ! (perm srcIdx)) `comb` (srcArr ! (Z:.srcIdx))
                                                                      False -> index dftArr sh)
                                           in permute (idx+1) comb newArr perm srcArr
                        in
                         permute 0 comb dftArr perm srcArr) -- Let block
           in
            let y0 = (y1) -- binding of Apply block
            in
             let y1 = (let y0 = (y0) -- binding of Apply block
                       in
                        let y1 = (y0) -- Let block
                        in
                         let y2 = (let y2 = (Repa.map (\x0 -> (if ((6 :: Int) == ((32 :: Int) - (1 :: Int)))
                                                                then (((((x0)) `xor` (minBound)) `shiftR` (6 :: Int)) .&. (1 :: Int))
                                                                else ((((x0)) `shiftR` (6 :: Int)) .&. (1 :: Int))))
                                                      (y1)) -- Let block
                                   in
                                    Repa.zipWith (\x0 -> \x1 -> (if (((x0)) == (0 :: Int))
                                                                  then (let ((tVar, _)) = ((x1)) in tVar)
                                                                  else (let ((_, tVar)) = ((x1)) in tVar)))
                                                 (y2)
                                                 (Repa.zipWith (\x0 -> \x1 -> (((x0), (x1))))
                                                               (let (y3, y4) = (let res = traverse (Repa.map (\x0 -> ((1 :: Int) `xor` ((x0))))
                                                                                                             (y2))
                                                                                                   (\(Z:.i) -> (Z:.(i+1)))
                                                                                                   (let newVal orig (Z:.pos) | pos == 0 = 0 :: Int
                                                                                                                              | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                            (newVal orig (Z:.(pos-1)))
                                                                                                                                            (orig (Z:.(pos-1)))
                                                                                                    in newVal)
                                                                                in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig sh -> orig sh), fromList Z [(res!(Z:.((size $ extent res)-1)))]))
                                                                in
                                                                 y3)
                                                               (Repa.map (\x0 -> ((9 :: Int) - ((x0))))
                                                                         (let (y3, y4) = (let res = traverse (y2)
                                                                                                             (\(Z:.i) -> (Z:.(i+1)))
                                                                                                             (let newVal orig sh@(Z:.pos) | pos == (size $ extent $ y2) = 0 :: Int
                                                                                                                                          | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                        (newVal orig (Z:.(pos+1)))
                                                                                                                                                        (orig sh)
                                                                                                              in newVal)
                                                                                          in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig (Z:.pos) -> orig (Z:.(pos+1)))
                                                                                              ,
                                                                                              fromList Z [(res!(Z:.0))]))
                                                                          in
                                                                           y3)))) -- Let block
                         in
                          let srcArr = y1
                              dftArr = y1
                              perm   = (\x0 -> (Z :. ((y2) ! (Z:.(x0)))))
                              comb   = (\x0 -> \x1 -> ((x0)))
                              permute idx comb dftArr perm srcArr
                               | idx >= (size $ extent srcArr) = dftArr
                               | otherwise = let (Z:.srcIdx) = fromIndex (extent srcArr) idx
                                                 newArr = fromFunction (extent dftArr)
                                                                       (\sh -> case sh == (perm srcIdx) of
                                                                        True  -> (dftArr ! (perm srcIdx)) `comb` (srcArr ! (Z:.srcIdx))
                                                                        False -> index dftArr sh)
                                             in permute (idx+1) comb newArr perm srcArr
                          in
                           permute 0 comb dftArr perm srcArr) -- Let block
             in
              let y0 = (y1) -- binding of Apply block
              in
               let y1 = (let y0 = (y0) -- binding of Apply block
                         in
                          let y1 = (y0) -- Let block
                          in
                           let y2 = (let y2 = (Repa.map (\x0 -> (if ((7 :: Int) == ((32 :: Int) - (1 :: Int)))
                                                                  then (((((x0)) `xor` (minBound)) `shiftR` (7 :: Int)) .&. (1 :: Int))
                                                                  else ((((x0)) `shiftR` (7 :: Int)) .&. (1 :: Int))))
                                                        (y1)) -- Let block
                                     in
                                      Repa.zipWith (\x0 -> \x1 -> (if (((x0)) == (0 :: Int))
                                                                    then (let ((tVar, _)) = ((x1)) in tVar)
                                                                    else (let ((_, tVar)) = ((x1)) in tVar)))
                                                   (y2)
                                                   (Repa.zipWith (\x0 -> \x1 -> (((x0), (x1))))
                                                                 (let (y3, y4) = (let res = traverse (Repa.map (\x0 -> ((1 :: Int) `xor` ((x0))))
                                                                                                               (y2))
                                                                                                     (\(Z:.i) -> (Z:.(i+1)))
                                                                                                     (let newVal orig (Z:.pos) | pos == 0 = 0 :: Int
                                                                                                                                | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                              (newVal orig (Z:.(pos-1)))
                                                                                                                                              (orig (Z:.(pos-1)))
                                                                                                      in newVal)
                                                                                  in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig sh -> orig sh), fromList Z [(res!(Z:.((size $ extent res)-1)))]))
                                                                  in
                                                                   y3)
                                                                 (Repa.map (\x0 -> ((9 :: Int) - ((x0))))
                                                                           (let (y3, y4) = (let res = traverse (y2)
                                                                                                               (\(Z:.i) -> (Z:.(i+1)))
                                                                                                               (let newVal orig sh@(Z:.pos) | pos == (size $ extent $ y2) = 0 :: Int
                                                                                                                                            | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                          (newVal orig (Z:.(pos+1)))
                                                                                                                                                          (orig sh)
                                                                                                                in newVal)
                                                                                            in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig (Z:.pos) -> orig (Z:.(pos+1)))
                                                                                                ,
                                                                                                fromList Z [(res!(Z:.0))]))
                                                                            in
                                                                             y3)))) -- Let block
                           in
                            let srcArr = y1
                                dftArr = y1
                                perm   = (\x0 -> (Z :. ((y2) ! (Z:.(x0)))))
                                comb   = (\x0 -> \x1 -> ((x0)))
                                permute idx comb dftArr perm srcArr
                                 | idx >= (size $ extent srcArr) = dftArr
                                 | otherwise = let (Z:.srcIdx) = fromIndex (extent srcArr) idx
                                                   newArr = fromFunction (extent dftArr)
                                                                         (\sh -> case sh == (perm srcIdx) of
                                                                          True  -> (dftArr ! (perm srcIdx)) `comb` (srcArr ! (Z:.srcIdx))
                                                                          False -> index dftArr sh)
                                               in permute (idx+1) comb newArr perm srcArr
                            in
                             permute 0 comb dftArr perm srcArr) -- Let block
               in
                let y0 = (y1) -- binding of Apply block
                in
                 let y1 = (let y0 = (y0) -- binding of Apply block
                           in
                            let y1 = (y0) -- Let block
                            in
                             let y2 = (let y2 = (Repa.map (\x0 -> (if ((8 :: Int) == ((32 :: Int) - (1 :: Int)))
                                                                    then (((((x0)) `xor` (minBound)) `shiftR` (8 :: Int)) .&. (1 :: Int))
                                                                    else ((((x0)) `shiftR` (8 :: Int)) .&. (1 :: Int))))
                                                          (y1)) -- Let block
                                       in
                                        Repa.zipWith (\x0 -> \x1 -> (if (((x0)) == (0 :: Int))
                                                                      then (let ((tVar, _)) = ((x1)) in tVar)
                                                                      else (let ((_, tVar)) = ((x1)) in tVar)))
                                                     (y2)
                                                     (Repa.zipWith (\x0 -> \x1 -> (((x0), (x1))))
                                                                   (let (y3, y4) = (let res = traverse (Repa.map (\x0 -> ((1 :: Int) `xor` ((x0))))
                                                                                                                 (y2))
                                                                                                       (\(Z:.i) -> (Z:.(i+1)))
                                                                                                       (let newVal orig (Z:.pos) | pos == 0 = 0 :: Int
                                                                                                                                  | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                (newVal orig (Z:.(pos-1)))
                                                                                                                                                (orig (Z:.(pos-1)))
                                                                                                        in newVal)
                                                                                    in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig sh -> orig sh), fromList Z [(res!(Z:.((size $ extent res)-1)))]))
                                                                    in
                                                                     y3)
                                                                   (Repa.map (\x0 -> ((9 :: Int) - ((x0))))
                                                                             (let (y3, y4) = (let res = traverse (y2)
                                                                                                                 (\(Z:.i) -> (Z:.(i+1)))
                                                                                                                 (let newVal orig sh@(Z:.pos) | pos == (size $ extent $ y2) = 0 :: Int
                                                                                                                                              | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                            (newVal orig (Z:.(pos+1)))
                                                                                                                                                            (orig sh)
                                                                                                                  in newVal)
                                                                                              in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig (Z:.pos) -> orig (Z:.(pos+1)))
                                                                                                  ,
                                                                                                  fromList Z [(res!(Z:.0))]))
                                                                              in
                                                                               y3)))) -- Let block
                             in
                              let srcArr = y1
                                  dftArr = y1
                                  perm   = (\x0 -> (Z :. ((y2) ! (Z:.(x0)))))
                                  comb   = (\x0 -> \x1 -> ((x0)))
                                  permute idx comb dftArr perm srcArr
                                   | idx >= (size $ extent srcArr) = dftArr
                                   | otherwise = let (Z:.srcIdx) = fromIndex (extent srcArr) idx
                                                     newArr = fromFunction (extent dftArr)
                                                                           (\sh -> case sh == (perm srcIdx) of
                                                                            True  -> (dftArr ! (perm srcIdx)) `comb` (srcArr ! (Z:.srcIdx))
                                                                            False -> index dftArr sh)
                                                 in permute (idx+1) comb newArr perm srcArr
                              in
                               permute 0 comb dftArr perm srcArr) -- Let block
                 in
                  let y0 = (y1) -- binding of Apply block
                  in
                   let y1 = (let y0 = (y0) -- binding of Apply block
                             in
                              let y1 = (y0) -- Let block
                              in
                               let y2 = (let y2 = (Repa.map (\x0 -> (if ((9 :: Int) == ((32 :: Int) - (1 :: Int)))
                                                                      then (((((x0)) `xor` (minBound)) `shiftR` (9 :: Int)) .&. (1 :: Int))
                                                                      else ((((x0)) `shiftR` (9 :: Int)) .&. (1 :: Int))))
                                                            (y1)) -- Let block
                                         in
                                          Repa.zipWith (\x0 -> \x1 -> (if (((x0)) == (0 :: Int))
                                                                        then (let ((tVar, _)) = ((x1)) in tVar)
                                                                        else (let ((_, tVar)) = ((x1)) in tVar)))
                                                       (y2)
                                                       (Repa.zipWith (\x0 -> \x1 -> (((x0), (x1))))
                                                                     (let (y3, y4) = (let res = traverse (Repa.map (\x0 -> ((1 :: Int) `xor` ((x0))))
                                                                                                                   (y2))
                                                                                                         (\(Z:.i) -> (Z:.(i+1)))
                                                                                                         (let newVal orig (Z:.pos) | pos == 0 = 0 :: Int
                                                                                                                                    | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                  (newVal orig (Z:.(pos-1)))
                                                                                                                                                  (orig (Z:.(pos-1)))
                                                                                                          in newVal)
                                                                                      in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig sh -> orig sh), fromList Z [(res!(Z:.((size $ extent res)-1)))]))
                                                                      in
                                                                       y3)
                                                                     (Repa.map (\x0 -> ((9 :: Int) - ((x0))))
                                                                               (let (y3, y4) = (let res = traverse (y2)
                                                                                                                   (\(Z:.i) -> (Z:.(i+1)))
                                                                                                                   (let newVal orig sh@(Z:.pos) | pos == (size $ extent $ y2) = 0 :: Int
                                                                                                                                                | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                              (newVal orig (Z:.(pos+1)))
                                                                                                                                                              (orig sh)
                                                                                                                    in newVal)
                                                                                                in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig (Z:.pos) -> orig (Z:.(pos+1)))
                                                                                                    ,
                                                                                                    fromList Z [(res!(Z:.0))]))
                                                                                in
                                                                                 y3)))) -- Let block
                               in
                                let srcArr = y1
                                    dftArr = y1
                                    perm   = (\x0 -> (Z :. ((y2) ! (Z:.(x0)))))
                                    comb   = (\x0 -> \x1 -> ((x0)))
                                    permute idx comb dftArr perm srcArr
                                     | idx >= (size $ extent srcArr) = dftArr
                                     | otherwise = let (Z:.srcIdx) = fromIndex (extent srcArr) idx
                                                       newArr = fromFunction (extent dftArr)
                                                                             (\sh -> case sh == (perm srcIdx) of
                                                                              True  -> (dftArr ! (perm srcIdx)) `comb` (srcArr ! (Z:.srcIdx))
                                                                              False -> index dftArr sh)
                                                   in permute (idx+1) comb newArr perm srcArr
                                in
                                 permute 0 comb dftArr perm srcArr) -- Let block
                   in
                    let y0 = (y1) -- binding of Apply block
                    in
                     let y1 = (let y0 = (y0) -- binding of Apply block
                               in
                                let y1 = (y0) -- Let block
                                in
                                 let y2 = (let y2 = (Repa.map (\x0 -> (if ((10 :: Int) == ((32 :: Int) - (1 :: Int)))
                                                                        then (((((x0)) `xor` (minBound)) `shiftR` (10 :: Int)) .&. (1 :: Int))
                                                                        else ((((x0)) `shiftR` (10 :: Int)) .&. (1 :: Int))))
                                                              (y1)) -- Let block
                                           in
                                            Repa.zipWith (\x0 -> \x1 -> (if (((x0)) == (0 :: Int))
                                                                          then (let ((tVar, _)) = ((x1)) in tVar)
                                                                          else (let ((_, tVar)) = ((x1)) in tVar)))
                                                         (y2)
                                                         (Repa.zipWith (\x0 -> \x1 -> (((x0), (x1))))
                                                                       (let (y3, y4) = (let res = traverse (Repa.map (\x0 -> ((1 :: Int) `xor` ((x0))))
                                                                                                                     (y2))
                                                                                                           (\(Z:.i) -> (Z:.(i+1)))
                                                                                                           (let newVal orig (Z:.pos) | pos == 0 = 0 :: Int
                                                                                                                                      | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                    (newVal orig (Z:.(pos-1)))
                                                                                                                                                    (orig (Z:.(pos-1)))
                                                                                                            in newVal)
                                                                                        in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig sh -> orig sh), fromList Z [(res!(Z:.((size $ extent res)-1)))]))
                                                                        in
                                                                         y3)
                                                                       (Repa.map (\x0 -> ((9 :: Int) - ((x0))))
                                                                                 (let (y3, y4) = (let res = traverse (y2)
                                                                                                                     (\(Z:.i) -> (Z:.(i+1)))
                                                                                                                     (let newVal orig sh@(Z:.pos) | pos == (size $ extent $ y2) = 0 :: Int
                                                                                                                                                  | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                                (newVal orig (Z:.(pos+1)))
                                                                                                                                                                (orig sh)
                                                                                                                      in newVal)
                                                                                                  in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig (Z:.pos) -> orig (Z:.(pos+1)))
                                                                                                      ,
                                                                                                      fromList Z [(res!(Z:.0))]))
                                                                                  in
                                                                                   y3)))) -- Let block
                                 in
                                  let srcArr = y1
                                      dftArr = y1
                                      perm   = (\x0 -> (Z :. ((y2) ! (Z:.(x0)))))
                                      comb   = (\x0 -> \x1 -> ((x0)))
                                      permute idx comb dftArr perm srcArr
                                       | idx >= (size $ extent srcArr) = dftArr
                                       | otherwise = let (Z:.srcIdx) = fromIndex (extent srcArr) idx
                                                         newArr = fromFunction (extent dftArr)
                                                                               (\sh -> case sh == (perm srcIdx) of
                                                                                True  -> (dftArr ! (perm srcIdx)) `comb` (srcArr ! (Z:.srcIdx))
                                                                                False -> index dftArr sh)
                                                     in permute (idx+1) comb newArr perm srcArr
                                  in
                                   permute 0 comb dftArr perm srcArr) -- Let block
                     in
                      let y0 = (y1) -- binding of Apply block
                      in
                       let y1 = (let y0 = (y0) -- binding of Apply block
                                 in
                                  let y1 = (y0) -- Let block
                                  in
                                   let y2 = (let y2 = (Repa.map (\x0 -> (if ((11 :: Int) == ((32 :: Int) - (1 :: Int)))
                                                                          then (((((x0)) `xor` (minBound)) `shiftR` (11 :: Int)) .&. (1 :: Int))
                                                                          else ((((x0)) `shiftR` (11 :: Int)) .&. (1 :: Int))))
                                                                (y1)) -- Let block
                                             in
                                              Repa.zipWith (\x0 -> \x1 -> (if (((x0)) == (0 :: Int))
                                                                            then (let ((tVar, _)) = ((x1)) in tVar)
                                                                            else (let ((_, tVar)) = ((x1)) in tVar)))
                                                           (y2)
                                                           (Repa.zipWith (\x0 -> \x1 -> (((x0), (x1))))
                                                                         (let (y3, y4) = (let res = traverse (Repa.map (\x0 -> ((1 :: Int) `xor` ((x0))))
                                                                                                                       (y2))
                                                                                                             (\(Z:.i) -> (Z:.(i+1)))
                                                                                                             (let newVal orig (Z:.pos) | pos == 0 = 0 :: Int
                                                                                                                                        | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                      (newVal orig (Z:.(pos-1)))
                                                                                                                                                      (orig (Z:.(pos-1)))
                                                                                                              in newVal)
                                                                                          in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig sh -> orig sh), fromList Z [(res!(Z:.((size $ extent res)-1)))]))
                                                                          in
                                                                           y3)
                                                                         (Repa.map (\x0 -> ((9 :: Int) - ((x0))))
                                                                                   (let (y3, y4) = (let res = traverse (y2)
                                                                                                                       (\(Z:.i) -> (Z:.(i+1)))
                                                                                                                       (let newVal orig sh@(Z:.pos) | pos == (size $ extent $ y2) = 0 :: Int
                                                                                                                                                    | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                                  (newVal orig (Z:.(pos+1)))
                                                                                                                                                                  (orig sh)
                                                                                                                        in newVal)
                                                                                                    in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig (Z:.pos) -> orig (Z:.(pos+1)))
                                                                                                        ,
                                                                                                        fromList Z [(res!(Z:.0))]))
                                                                                    in
                                                                                     y3)))) -- Let block
                                   in
                                    let srcArr = y1
                                        dftArr = y1
                                        perm   = (\x0 -> (Z :. ((y2) ! (Z:.(x0)))))
                                        comb   = (\x0 -> \x1 -> ((x0)))
                                        permute idx comb dftArr perm srcArr
                                         | idx >= (size $ extent srcArr) = dftArr
                                         | otherwise = let (Z:.srcIdx) = fromIndex (extent srcArr) idx
                                                           newArr = fromFunction (extent dftArr)
                                                                                 (\sh -> case sh == (perm srcIdx) of
                                                                                  True  -> (dftArr ! (perm srcIdx)) `comb` (srcArr ! (Z:.srcIdx))
                                                                                  False -> index dftArr sh)
                                                       in permute (idx+1) comb newArr perm srcArr
                                    in
                                     permute 0 comb dftArr perm srcArr) -- Let block
                       in
                        let y0 = (y1) -- binding of Apply block
                        in
                         let y1 = (let y0 = (y0) -- binding of Apply block
                                   in
                                    let y1 = (y0) -- Let block
                                    in
                                     let y2 = (let y2 = (Repa.map (\x0 -> (if ((12 :: Int) == ((32 :: Int) - (1 :: Int)))
                                                                            then (((((x0)) `xor` (minBound)) `shiftR` (12 :: Int)) .&. (1 :: Int))
                                                                            else ((((x0)) `shiftR` (12 :: Int)) .&. (1 :: Int))))
                                                                  (y1)) -- Let block
                                               in
                                                Repa.zipWith (\x0 -> \x1 -> (if (((x0)) == (0 :: Int))
                                                                              then (let ((tVar, _)) = ((x1)) in tVar)
                                                                              else (let ((_, tVar)) = ((x1)) in tVar)))
                                                             (y2)
                                                             (Repa.zipWith (\x0 -> \x1 -> (((x0), (x1))))
                                                                           (let (y3, y4) = (let res = traverse (Repa.map (\x0 -> ((1 :: Int) `xor` ((x0))))
                                                                                                                         (y2))
                                                                                                               (\(Z:.i) -> (Z:.(i+1)))
                                                                                                               (let newVal orig (Z:.pos) | pos == 0 = 0 :: Int
                                                                                                                                          | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                        (newVal orig (Z:.(pos-1)))
                                                                                                                                                        (orig (Z:.(pos-1)))
                                                                                                                in newVal)
                                                                                            in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig sh -> orig sh), fromList Z [(res!(Z:.((size $ extent res)-1)))]))
                                                                            in
                                                                             y3)
                                                                           (Repa.map (\x0 -> ((9 :: Int) - ((x0))))
                                                                                     (let (y3, y4) = (let res = traverse (y2)
                                                                                                                         (\(Z:.i) -> (Z:.(i+1)))
                                                                                                                         (let newVal orig sh@(Z:.pos) | pos == (size $ extent $ y2) = 0 :: Int
                                                                                                                                                      | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                                    (newVal orig (Z:.(pos+1)))
                                                                                                                                                                    (orig sh)
                                                                                                                          in newVal)
                                                                                                      in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig (Z:.pos) -> orig (Z:.(pos+1)))
                                                                                                          ,
                                                                                                          fromList Z [(res!(Z:.0))]))
                                                                                      in
                                                                                       y3)))) -- Let block
                                     in
                                      let srcArr = y1
                                          dftArr = y1
                                          perm   = (\x0 -> (Z :. ((y2) ! (Z:.(x0)))))
                                          comb   = (\x0 -> \x1 -> ((x0)))
                                          permute idx comb dftArr perm srcArr
                                           | idx >= (size $ extent srcArr) = dftArr
                                           | otherwise = let (Z:.srcIdx) = fromIndex (extent srcArr) idx
                                                             newArr = fromFunction (extent dftArr)
                                                                                   (\sh -> case sh == (perm srcIdx) of
                                                                                    True  -> (dftArr ! (perm srcIdx)) `comb` (srcArr ! (Z:.srcIdx))
                                                                                    False -> index dftArr sh)
                                                         in permute (idx+1) comb newArr perm srcArr
                                      in
                                       permute 0 comb dftArr perm srcArr) -- Let block
                         in
                          let y0 = (y1) -- binding of Apply block
                          in
                           let y1 = (let y0 = (y0) -- binding of Apply block
                                     in
                                      let y1 = (y0) -- Let block
                                      in
                                       let y2 = (let y2 = (Repa.map (\x0 -> (if ((13 :: Int) == ((32 :: Int) - (1 :: Int)))
                                                                              then (((((x0)) `xor` (minBound)) `shiftR` (13 :: Int)) .&. (1 :: Int))
                                                                              else ((((x0)) `shiftR` (13 :: Int)) .&. (1 :: Int))))
                                                                    (y1)) -- Let block
                                                 in
                                                  Repa.zipWith (\x0 -> \x1 -> (if (((x0)) == (0 :: Int))
                                                                                then (let ((tVar, _)) = ((x1)) in tVar)
                                                                                else (let ((_, tVar)) = ((x1)) in tVar)))
                                                               (y2)
                                                               (Repa.zipWith (\x0 -> \x1 -> (((x0), (x1))))
                                                                             (let (y3, y4) = (let res = traverse (Repa.map (\x0 -> ((1 :: Int) `xor` ((x0))))
                                                                                                                           (y2))
                                                                                                                 (\(Z:.i) -> (Z:.(i+1)))
                                                                                                                 (let newVal orig (Z:.pos) | pos == 0 = 0 :: Int
                                                                                                                                            | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                          (newVal orig (Z:.(pos-1)))
                                                                                                                                                          (orig (Z:.(pos-1)))
                                                                                                                  in newVal)
                                                                                              in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig sh -> orig sh), fromList Z [(res!(Z:.((size $ extent res)-1)))]))
                                                                              in
                                                                               y3)
                                                                             (Repa.map (\x0 -> ((9 :: Int) - ((x0))))
                                                                                       (let (y3, y4) = (let res = traverse (y2)
                                                                                                                           (\(Z:.i) -> (Z:.(i+1)))
                                                                                                                           (let newVal orig sh@(Z:.pos) | pos == (size $ extent $ y2) = 0 :: Int
                                                                                                                                                        | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                                      (newVal orig (Z:.(pos+1)))
                                                                                                                                                                      (orig sh)
                                                                                                                            in newVal)
                                                                                                        in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig (Z:.pos) -> orig (Z:.(pos+1)))
                                                                                                            ,
                                                                                                            fromList Z [(res!(Z:.0))]))
                                                                                        in
                                                                                         y3)))) -- Let block
                                       in
                                        let srcArr = y1
                                            dftArr = y1
                                            perm   = (\x0 -> (Z :. ((y2) ! (Z:.(x0)))))
                                            comb   = (\x0 -> \x1 -> ((x0)))
                                            permute idx comb dftArr perm srcArr
                                             | idx >= (size $ extent srcArr) = dftArr
                                             | otherwise = let (Z:.srcIdx) = fromIndex (extent srcArr) idx
                                                               newArr = fromFunction (extent dftArr)
                                                                                     (\sh -> case sh == (perm srcIdx) of
                                                                                      True  -> (dftArr ! (perm srcIdx)) `comb` (srcArr ! (Z:.srcIdx))
                                                                                      False -> index dftArr sh)
                                                           in permute (idx+1) comb newArr perm srcArr
                                        in
                                         permute 0 comb dftArr perm srcArr) -- Let block
                           in
                            let y0 = (y1) -- binding of Apply block
                            in
                             let y1 = (let y0 = (y0) -- binding of Apply block
                                       in
                                        let y1 = (y0) -- Let block
                                        in
                                         let y2 = (let y2 = (Repa.map (\x0 -> (if ((14 :: Int) == ((32 :: Int) - (1 :: Int)))
                                                                                then (((((x0)) `xor` (minBound)) `shiftR` (14 :: Int)) .&. (1 :: Int))
                                                                                else ((((x0)) `shiftR` (14 :: Int)) .&. (1 :: Int))))
                                                                      (y1)) -- Let block
                                                   in
                                                    Repa.zipWith (\x0 -> \x1 -> (if (((x0)) == (0 :: Int))
                                                                                  then (let ((tVar, _)) = ((x1)) in tVar)
                                                                                  else (let ((_, tVar)) = ((x1)) in tVar)))
                                                                 (y2)
                                                                 (Repa.zipWith (\x0 -> \x1 -> (((x0), (x1))))
                                                                               (let (y3, y4) = (let res = traverse (Repa.map (\x0 -> ((1 :: Int) `xor` ((x0))))
                                                                                                                             (y2))
                                                                                                                   (\(Z:.i) -> (Z:.(i+1)))
                                                                                                                   (let newVal orig (Z:.pos) | pos == 0 = 0 :: Int
                                                                                                                                              | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                            (newVal orig (Z:.(pos-1)))
                                                                                                                                                            (orig (Z:.(pos-1)))
                                                                                                                    in newVal)
                                                                                                in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig sh -> orig sh), fromList Z [(res!(Z:.((size $ extent res)-1)))]))
                                                                                in
                                                                                 y3)
                                                                               (Repa.map (\x0 -> ((9 :: Int) - ((x0))))
                                                                                         (let (y3, y4) = (let res = traverse (y2)
                                                                                                                             (\(Z:.i) -> (Z:.(i+1)))
                                                                                                                             (let newVal orig sh@(Z:.pos) | pos == (size $ extent $ y2) = 0 :: Int
                                                                                                                                                          | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                                        (newVal orig (Z:.(pos+1)))
                                                                                                                                                                        (orig sh)
                                                                                                                              in newVal)
                                                                                                          in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig (Z:.pos) -> orig (Z:.(pos+1)))
                                                                                                              ,
                                                                                                              fromList Z [(res!(Z:.0))]))
                                                                                          in
                                                                                           y3)))) -- Let block
                                         in
                                          let srcArr = y1
                                              dftArr = y1
                                              perm   = (\x0 -> (Z :. ((y2) ! (Z:.(x0)))))
                                              comb   = (\x0 -> \x1 -> ((x0)))
                                              permute idx comb dftArr perm srcArr
                                               | idx >= (size $ extent srcArr) = dftArr
                                               | otherwise = let (Z:.srcIdx) = fromIndex (extent srcArr) idx
                                                                 newArr = fromFunction (extent dftArr)
                                                                                       (\sh -> case sh == (perm srcIdx) of
                                                                                        True  -> (dftArr ! (perm srcIdx)) `comb` (srcArr ! (Z:.srcIdx))
                                                                                        False -> index dftArr sh)
                                                             in permute (idx+1) comb newArr perm srcArr
                                          in
                                           permute 0 comb dftArr perm srcArr) -- Let block
                             in
                              let y0 = (y1) -- binding of Apply block
                              in
                               let y1 = (let y0 = (y0) -- binding of Apply block
                                         in
                                          let y1 = (y0) -- Let block
                                          in
                                           let y2 = (let y2 = (Repa.map (\x0 -> (if ((15 :: Int) == ((32 :: Int) - (1 :: Int)))
                                                                                  then (((((x0)) `xor` (minBound)) `shiftR` (15 :: Int)) .&. (1 :: Int))
                                                                                  else ((((x0)) `shiftR` (15 :: Int)) .&. (1 :: Int))))
                                                                        (y1)) -- Let block
                                                     in
                                                      Repa.zipWith (\x0 -> \x1 -> (if (((x0)) == (0 :: Int))
                                                                                    then (let ((tVar, _)) = ((x1)) in tVar)
                                                                                    else (let ((_, tVar)) = ((x1)) in tVar)))
                                                                   (y2)
                                                                   (Repa.zipWith (\x0 -> \x1 -> (((x0), (x1))))
                                                                                 (let (y3, y4) = (let res = traverse (Repa.map (\x0 -> ((1 :: Int) `xor` ((x0))))
                                                                                                                               (y2))
                                                                                                                     (\(Z:.i) -> (Z:.(i+1)))
                                                                                                                     (let newVal orig (Z:.pos) | pos == 0 = 0 :: Int
                                                                                                                                                | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                              (newVal orig (Z:.(pos-1)))
                                                                                                                                                              (orig (Z:.(pos-1)))
                                                                                                                      in newVal)
                                                                                                  in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig sh -> orig sh), fromList Z [(res!(Z:.((size $ extent res)-1)))]))
                                                                                  in
                                                                                   y3)
                                                                                 (Repa.map (\x0 -> ((9 :: Int) - ((x0))))
                                                                                           (let (y3, y4) = (let res = traverse (y2)
                                                                                                                               (\(Z:.i) -> (Z:.(i+1)))
                                                                                                                               (let newVal orig sh@(Z:.pos) | pos == (size $ extent $ y2) = 0 :: Int
                                                                                                                                                            | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                                          (newVal orig (Z:.(pos+1)))
                                                                                                                                                                          (orig sh)
                                                                                                                                in newVal)
                                                                                                            in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig (Z:.pos) -> orig (Z:.(pos+1)))
                                                                                                                ,
                                                                                                                fromList Z [(res!(Z:.0))]))
                                                                                            in
                                                                                             y3)))) -- Let block
                                           in
                                            let srcArr = y1
                                                dftArr = y1
                                                perm   = (\x0 -> (Z :. ((y2) ! (Z:.(x0)))))
                                                comb   = (\x0 -> \x1 -> ((x0)))
                                                permute idx comb dftArr perm srcArr
                                                 | idx >= (size $ extent srcArr) = dftArr
                                                 | otherwise = let (Z:.srcIdx) = fromIndex (extent srcArr) idx
                                                                   newArr = fromFunction (extent dftArr)
                                                                                         (\sh -> case sh == (perm srcIdx) of
                                                                                          True  -> (dftArr ! (perm srcIdx)) `comb` (srcArr ! (Z:.srcIdx))
                                                                                          False -> index dftArr sh)
                                                               in permute (idx+1) comb newArr perm srcArr
                                            in
                                             permute 0 comb dftArr perm srcArr) -- Let block
                               in
                                let y0 = (y1) -- binding of Apply block
                                in
                                 let y1 = (let y0 = (y0) -- binding of Apply block
                                           in
                                            let y1 = (y0) -- Let block
                                            in
                                             let y2 = (let y2 = (Repa.map (\x0 -> (if ((16 :: Int) == ((32 :: Int) - (1 :: Int)))
                                                                                    then (((((x0)) `xor` (minBound)) `shiftR` (16 :: Int)) .&. (1 :: Int))
                                                                                    else ((((x0)) `shiftR` (16 :: Int)) .&. (1 :: Int))))
                                                                          (y1)) -- Let block
                                                       in
                                                        Repa.zipWith (\x0 -> \x1 -> (if (((x0)) == (0 :: Int))
                                                                                      then (let ((tVar, _)) = ((x1)) in tVar)
                                                                                      else (let ((_, tVar)) = ((x1)) in tVar)))
                                                                     (y2)
                                                                     (Repa.zipWith (\x0 -> \x1 -> (((x0), (x1))))
                                                                                   (let (y3, y4) = (let res = traverse (Repa.map (\x0 -> ((1 :: Int) `xor` ((x0))))
                                                                                                                                 (y2))
                                                                                                                       (\(Z:.i) -> (Z:.(i+1)))
                                                                                                                       (let newVal orig (Z:.pos) | pos == 0 = 0 :: Int
                                                                                                                                                  | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                                (newVal orig (Z:.(pos-1)))
                                                                                                                                                                (orig (Z:.(pos-1)))
                                                                                                                        in newVal)
                                                                                                    in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig sh -> orig sh), fromList Z [(res!(Z:.((size $ extent res)-1)))]))
                                                                                    in
                                                                                     y3)
                                                                                   (Repa.map (\x0 -> ((9 :: Int) - ((x0))))
                                                                                             (let (y3, y4) = (let res = traverse (y2)
                                                                                                                                 (\(Z:.i) -> (Z:.(i+1)))
                                                                                                                                 (let newVal orig sh@(Z:.pos) | pos == (size $ extent $ y2) = 0 :: Int
                                                                                                                                                              | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                                            (newVal orig (Z:.(pos+1)))
                                                                                                                                                                            (orig sh)
                                                                                                                                  in newVal)
                                                                                                              in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig (Z:.pos) -> orig (Z:.(pos+1)))
                                                                                                                  ,
                                                                                                                  fromList Z [(res!(Z:.0))]))
                                                                                              in
                                                                                               y3)))) -- Let block
                                             in
                                              let srcArr = y1
                                                  dftArr = y1
                                                  perm   = (\x0 -> (Z :. ((y2) ! (Z:.(x0)))))
                                                  comb   = (\x0 -> \x1 -> ((x0)))
                                                  permute idx comb dftArr perm srcArr
                                                   | idx >= (size $ extent srcArr) = dftArr
                                                   | otherwise = let (Z:.srcIdx) = fromIndex (extent srcArr) idx
                                                                     newArr = fromFunction (extent dftArr)
                                                                                           (\sh -> case sh == (perm srcIdx) of
                                                                                            True  -> (dftArr ! (perm srcIdx)) `comb` (srcArr ! (Z:.srcIdx))
                                                                                            False -> index dftArr sh)
                                                                 in permute (idx+1) comb newArr perm srcArr
                                              in
                                               permute 0 comb dftArr perm srcArr) -- Let block
                                 in
                                  let y0 = (y1) -- binding of Apply block
                                  in
                                   let y1 = (let y0 = (y0) -- binding of Apply block
                                             in
                                              let y1 = (y0) -- Let block
                                              in
                                               let y2 = (let y2 = (Repa.map (\x0 -> (if ((17 :: Int) == ((32 :: Int) - (1 :: Int)))
                                                                                      then (((((x0)) `xor` (minBound)) `shiftR` (17 :: Int)) .&. (1 :: Int))
                                                                                      else ((((x0)) `shiftR` (17 :: Int)) .&. (1 :: Int))))
                                                                            (y1)) -- Let block
                                                         in
                                                          Repa.zipWith (\x0 -> \x1 -> (if (((x0)) == (0 :: Int))
                                                                                        then (let ((tVar, _)) = ((x1)) in tVar)
                                                                                        else (let ((_, tVar)) = ((x1)) in tVar)))
                                                                       (y2)
                                                                       (Repa.zipWith (\x0 -> \x1 -> (((x0), (x1))))
                                                                                     (let (y3, y4) = (let res = traverse (Repa.map (\x0 -> ((1 :: Int) `xor` ((x0))))
                                                                                                                                   (y2))
                                                                                                                         (\(Z:.i) -> (Z:.(i+1)))
                                                                                                                         (let newVal orig (Z:.pos) | pos == 0 = 0 :: Int
                                                                                                                                                    | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                                  (newVal orig (Z:.(pos-1)))
                                                                                                                                                                  (orig (Z:.(pos-1)))
                                                                                                                          in newVal)
                                                                                                      in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig sh -> orig sh), fromList Z [(res!(Z:.((size $ extent res)-1)))]))
                                                                                      in
                                                                                       y3)
                                                                                     (Repa.map (\x0 -> ((9 :: Int) - ((x0))))
                                                                                               (let (y3, y4) = (let res = traverse (y2)
                                                                                                                                   (\(Z:.i) -> (Z:.(i+1)))
                                                                                                                                   (let newVal orig sh@(Z:.pos) | pos == (size $ extent $ y2) = 0 :: Int
                                                                                                                                                                | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                                              (newVal orig (Z:.(pos+1)))
                                                                                                                                                                              (orig sh)
                                                                                                                                    in newVal)
                                                                                                                in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig (Z:.pos) -> orig (Z:.(pos+1)))
                                                                                                                    ,
                                                                                                                    fromList Z [(res!(Z:.0))]))
                                                                                                in
                                                                                                 y3)))) -- Let block
                                               in
                                                let srcArr = y1
                                                    dftArr = y1
                                                    perm   = (\x0 -> (Z :. ((y2) ! (Z:.(x0)))))
                                                    comb   = (\x0 -> \x1 -> ((x0)))
                                                    permute idx comb dftArr perm srcArr
                                                     | idx >= (size $ extent srcArr) = dftArr
                                                     | otherwise = let (Z:.srcIdx) = fromIndex (extent srcArr) idx
                                                                       newArr = fromFunction (extent dftArr)
                                                                                             (\sh -> case sh == (perm srcIdx) of
                                                                                              True  -> (dftArr ! (perm srcIdx)) `comb` (srcArr ! (Z:.srcIdx))
                                                                                              False -> index dftArr sh)
                                                                   in permute (idx+1) comb newArr perm srcArr
                                                in
                                                 permute 0 comb dftArr perm srcArr) -- Let block
                                   in
                                    let y0 = (y1) -- binding of Apply block
                                    in
                                     let y1 = (let y0 = (y0) -- binding of Apply block
                                               in
                                                let y1 = (y0) -- Let block
                                                in
                                                 let y2 = (let y2 = (Repa.map (\x0 -> (if ((18 :: Int) == ((32 :: Int) - (1 :: Int)))
                                                                                        then (((((x0)) `xor` (minBound)) `shiftR` (18 :: Int)) .&. (1 :: Int))
                                                                                        else ((((x0)) `shiftR` (18 :: Int)) .&. (1 :: Int))))
                                                                              (y1)) -- Let block
                                                           in
                                                            Repa.zipWith (\x0 -> \x1 -> (if (((x0)) == (0 :: Int))
                                                                                          then (let ((tVar, _)) = ((x1)) in tVar)
                                                                                          else (let ((_, tVar)) = ((x1)) in tVar)))
                                                                         (y2)
                                                                         (Repa.zipWith (\x0 -> \x1 -> (((x0), (x1))))
                                                                                       (let (y3, y4) = (let res = traverse (Repa.map (\x0 -> ((1 :: Int) `xor` ((x0))))
                                                                                                                                     (y2))
                                                                                                                           (\(Z:.i) -> (Z:.(i+1)))
                                                                                                                           (let newVal orig (Z:.pos) | pos == 0 = 0 :: Int
                                                                                                                                                      | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                                    (newVal orig (Z:.(pos-1)))
                                                                                                                                                                    (orig (Z:.(pos-1)))
                                                                                                                            in newVal)
                                                                                                        in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig sh -> orig sh), fromList Z [(res!(Z:.((size $ extent res)-1)))]))
                                                                                        in
                                                                                         y3)
                                                                                       (Repa.map (\x0 -> ((9 :: Int) - ((x0))))
                                                                                                 (let (y3, y4) = (let res = traverse (y2)
                                                                                                                                     (\(Z:.i) -> (Z:.(i+1)))
                                                                                                                                     (let newVal orig sh@(Z:.pos) | pos == (size $ extent $ y2) = 0 :: Int
                                                                                                                                                                  | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                                                (newVal orig (Z:.(pos+1)))
                                                                                                                                                                                (orig sh)
                                                                                                                                      in newVal)
                                                                                                                  in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig (Z:.pos) -> orig (Z:.(pos+1)))
                                                                                                                      ,
                                                                                                                      fromList Z [(res!(Z:.0))]))
                                                                                                  in
                                                                                                   y3)))) -- Let block
                                                 in
                                                  let srcArr = y1
                                                      dftArr = y1
                                                      perm   = (\x0 -> (Z :. ((y2) ! (Z:.(x0)))))
                                                      comb   = (\x0 -> \x1 -> ((x0)))
                                                      permute idx comb dftArr perm srcArr
                                                       | idx >= (size $ extent srcArr) = dftArr
                                                       | otherwise = let (Z:.srcIdx) = fromIndex (extent srcArr) idx
                                                                         newArr = fromFunction (extent dftArr)
                                                                                               (\sh -> case sh == (perm srcIdx) of
                                                                                                True  -> (dftArr ! (perm srcIdx)) `comb` (srcArr ! (Z:.srcIdx))
                                                                                                False -> index dftArr sh)
                                                                     in permute (idx+1) comb newArr perm srcArr
                                                  in
                                                   permute 0 comb dftArr perm srcArr) -- Let block
                                     in
                                      let y0 = (y1) -- binding of Apply block
                                      in
                                       let y1 = (let y0 = (y0) -- binding of Apply block
                                                 in
                                                  let y1 = (y0) -- Let block
                                                  in
                                                   let y2 = (let y2 = (Repa.map (\x0 -> (if ((19 :: Int) == ((32 :: Int) - (1 :: Int)))
                                                                                          then (((((x0)) `xor` (minBound)) `shiftR` (19 :: Int)) .&. (1 :: Int))
                                                                                          else ((((x0)) `shiftR` (19 :: Int)) .&. (1 :: Int))))
                                                                                (y1)) -- Let block
                                                             in
                                                              Repa.zipWith (\x0 -> \x1 -> (if (((x0)) == (0 :: Int))
                                                                                            then (let ((tVar, _)) = ((x1)) in tVar)
                                                                                            else (let ((_, tVar)) = ((x1)) in tVar)))
                                                                           (y2)
                                                                           (Repa.zipWith (\x0 -> \x1 -> (((x0), (x1))))
                                                                                         (let (y3, y4) = (let res = traverse (Repa.map (\x0 -> ((1 :: Int) `xor` ((x0))))
                                                                                                                                       (y2))
                                                                                                                             (\(Z:.i) -> (Z:.(i+1)))
                                                                                                                             (let newVal orig (Z:.pos) | pos == 0 = 0 :: Int
                                                                                                                                                        | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                                      (newVal orig (Z:.(pos-1)))
                                                                                                                                                                      (orig (Z:.(pos-1)))
                                                                                                                              in newVal)
                                                                                                          in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig sh -> orig sh), fromList Z [(res!(Z:.((size $ extent res)-1)))]))
                                                                                          in
                                                                                           y3)
                                                                                         (Repa.map (\x0 -> ((9 :: Int) - ((x0))))
                                                                                                   (let (y3, y4) = (let res = traverse (y2)
                                                                                                                                       (\(Z:.i) -> (Z:.(i+1)))
                                                                                                                                       (let newVal orig sh@(Z:.pos) | pos == (size $ extent $ y2) = 0 :: Int
                                                                                                                                                                    | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                                                  (newVal orig (Z:.(pos+1)))
                                                                                                                                                                                  (orig sh)
                                                                                                                                        in newVal)
                                                                                                                    in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig (Z:.pos) -> orig (Z:.(pos+1)))
                                                                                                                        ,
                                                                                                                        fromList Z [(res!(Z:.0))]))
                                                                                                    in
                                                                                                     y3)))) -- Let block
                                                   in
                                                    let srcArr = y1
                                                        dftArr = y1
                                                        perm   = (\x0 -> (Z :. ((y2) ! (Z:.(x0)))))
                                                        comb   = (\x0 -> \x1 -> ((x0)))
                                                        permute idx comb dftArr perm srcArr
                                                         | idx >= (size $ extent srcArr) = dftArr
                                                         | otherwise = let (Z:.srcIdx) = fromIndex (extent srcArr) idx
                                                                           newArr = fromFunction (extent dftArr)
                                                                                                 (\sh -> case sh == (perm srcIdx) of
                                                                                                  True  -> (dftArr ! (perm srcIdx)) `comb` (srcArr ! (Z:.srcIdx))
                                                                                                  False -> index dftArr sh)
                                                                       in permute (idx+1) comb newArr perm srcArr
                                                    in
                                                     permute 0 comb dftArr perm srcArr) -- Let block
                                       in
                                        let y0 = (y1) -- binding of Apply block
                                        in
                                         let y1 = (let y0 = (y0) -- binding of Apply block
                                                   in
                                                    let y1 = (y0) -- Let block
                                                    in
                                                     let y2 = (let y2 = (Repa.map (\x0 -> (if ((20 :: Int) == ((32 :: Int) - (1 :: Int)))
                                                                                            then (((((x0)) `xor` (minBound)) `shiftR` (20 :: Int)) .&. (1 :: Int))
                                                                                            else ((((x0)) `shiftR` (20 :: Int)) .&. (1 :: Int))))
                                                                                  (y1)) -- Let block
                                                               in
                                                                Repa.zipWith (\x0 -> \x1 -> (if (((x0)) == (0 :: Int))
                                                                                              then (let ((tVar, _)) = ((x1)) in tVar)
                                                                                              else (let ((_, tVar)) = ((x1)) in tVar)))
                                                                             (y2)
                                                                             (Repa.zipWith (\x0 -> \x1 -> (((x0), (x1))))
                                                                                           (let (y3, y4) = (let res = traverse (Repa.map (\x0 -> ((1 :: Int) `xor` ((x0))))
                                                                                                                                         (y2))
                                                                                                                               (\(Z:.i) -> (Z:.(i+1)))
                                                                                                                               (let newVal orig (Z:.pos) | pos == 0 = 0 :: Int
                                                                                                                                                          | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                                        (newVal orig (Z:.(pos-1)))
                                                                                                                                                                        (orig (Z:.(pos-1)))
                                                                                                                                in newVal)
                                                                                                            in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig sh -> orig sh), fromList Z [(res!(Z:.((size $ extent res)-1)))]))
                                                                                            in
                                                                                             y3)
                                                                                           (Repa.map (\x0 -> ((9 :: Int) - ((x0))))
                                                                                                     (let (y3, y4) = (let res = traverse (y2)
                                                                                                                                         (\(Z:.i) -> (Z:.(i+1)))
                                                                                                                                         (let newVal orig sh@(Z:.pos) | pos == (size $ extent $ y2) = 0 :: Int
                                                                                                                                                                      | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                                                    (newVal orig (Z:.(pos+1)))
                                                                                                                                                                                    (orig sh)
                                                                                                                                          in newVal)
                                                                                                                      in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig (Z:.pos) -> orig (Z:.(pos+1)))
                                                                                                                          ,
                                                                                                                          fromList Z [(res!(Z:.0))]))
                                                                                                      in
                                                                                                       y3)))) -- Let block
                                                     in
                                                      let srcArr = y1
                                                          dftArr = y1
                                                          perm   = (\x0 -> (Z :. ((y2) ! (Z:.(x0)))))
                                                          comb   = (\x0 -> \x1 -> ((x0)))
                                                          permute idx comb dftArr perm srcArr
                                                           | idx >= (size $ extent srcArr) = dftArr
                                                           | otherwise = let (Z:.srcIdx) = fromIndex (extent srcArr) idx
                                                                             newArr = fromFunction (extent dftArr)
                                                                                                   (\sh -> case sh == (perm srcIdx) of
                                                                                                    True  -> (dftArr ! (perm srcIdx)) `comb` (srcArr ! (Z:.srcIdx))
                                                                                                    False -> index dftArr sh)
                                                                         in permute (idx+1) comb newArr perm srcArr
                                                      in
                                                       permute 0 comb dftArr perm srcArr) -- Let block
                                         in
                                          let y0 = (y1) -- binding of Apply block
                                          in
                                           let y1 = (let y0 = (y0) -- binding of Apply block
                                                     in
                                                      let y1 = (y0) -- Let block
                                                      in
                                                       let y2 = (let y2 = (Repa.map (\x0 -> (if ((21 :: Int) == ((32 :: Int) - (1 :: Int)))
                                                                                              then (((((x0)) `xor` (minBound)) `shiftR` (21 :: Int)) .&. (1 :: Int))
                                                                                              else ((((x0)) `shiftR` (21 :: Int)) .&. (1 :: Int))))
                                                                                    (y1)) -- Let block
                                                                 in
                                                                  Repa.zipWith (\x0 -> \x1 -> (if (((x0)) == (0 :: Int))
                                                                                                then (let ((tVar, _)) = ((x1)) in tVar)
                                                                                                else (let ((_, tVar)) = ((x1)) in tVar)))
                                                                               (y2)
                                                                               (Repa.zipWith (\x0 -> \x1 -> (((x0), (x1))))
                                                                                             (let (y3, y4) = (let res = traverse (Repa.map (\x0 -> ((1 :: Int) `xor` ((x0))))
                                                                                                                                           (y2))
                                                                                                                                 (\(Z:.i) -> (Z:.(i+1)))
                                                                                                                                 (let newVal orig (Z:.pos) | pos == 0 = 0 :: Int
                                                                                                                                                            | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                                          (newVal orig (Z:.(pos-1)))
                                                                                                                                                                          (orig (Z:.(pos-1)))
                                                                                                                                  in newVal)
                                                                                                              in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig sh -> orig sh), fromList Z [(res!(Z:.((size $ extent res)-1)))]))
                                                                                              in
                                                                                               y3)
                                                                                             (Repa.map (\x0 -> ((9 :: Int) - ((x0))))
                                                                                                       (let (y3, y4) = (let res = traverse (y2)
                                                                                                                                           (\(Z:.i) -> (Z:.(i+1)))
                                                                                                                                           (let newVal orig sh@(Z:.pos) | pos == (size $ extent $ y2) = 0 :: Int
                                                                                                                                                                        | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                                                      (newVal orig (Z:.(pos+1)))
                                                                                                                                                                                      (orig sh)
                                                                                                                                            in newVal)
                                                                                                                        in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig (Z:.pos) -> orig (Z:.(pos+1)))
                                                                                                                            ,
                                                                                                                            fromList Z [(res!(Z:.0))]))
                                                                                                        in
                                                                                                         y3)))) -- Let block
                                                       in
                                                        let srcArr = y1
                                                            dftArr = y1
                                                            perm   = (\x0 -> (Z :. ((y2) ! (Z:.(x0)))))
                                                            comb   = (\x0 -> \x1 -> ((x0)))
                                                            permute idx comb dftArr perm srcArr
                                                             | idx >= (size $ extent srcArr) = dftArr
                                                             | otherwise = let (Z:.srcIdx) = fromIndex (extent srcArr) idx
                                                                               newArr = fromFunction (extent dftArr)
                                                                                                     (\sh -> case sh == (perm srcIdx) of
                                                                                                      True  -> (dftArr ! (perm srcIdx)) `comb` (srcArr ! (Z:.srcIdx))
                                                                                                      False -> index dftArr sh)
                                                                           in permute (idx+1) comb newArr perm srcArr
                                                        in
                                                         permute 0 comb dftArr perm srcArr) -- Let block
                                           in
                                            let y0 = (y1) -- binding of Apply block
                                            in
                                             let y1 = (let y0 = (y0) -- binding of Apply block
                                                       in
                                                        let y1 = (y0) -- Let block
                                                        in
                                                         let y2 = (let y2 = (Repa.map (\x0 -> (if ((22 :: Int) == ((32 :: Int) - (1 :: Int)))
                                                                                                then (((((x0)) `xor` (minBound)) `shiftR` (22 :: Int)) .&. (1 :: Int))
                                                                                                else ((((x0)) `shiftR` (22 :: Int)) .&. (1 :: Int))))
                                                                                      (y1)) -- Let block
                                                                   in
                                                                    Repa.zipWith (\x0 -> \x1 -> (if (((x0)) == (0 :: Int))
                                                                                                  then (let ((tVar, _)) = ((x1)) in tVar)
                                                                                                  else (let ((_, tVar)) = ((x1)) in tVar)))
                                                                                 (y2)
                                                                                 (Repa.zipWith (\x0 -> \x1 -> (((x0), (x1))))
                                                                                               (let (y3, y4) = (let res = traverse (Repa.map (\x0 -> ((1 :: Int) `xor` ((x0))))
                                                                                                                                             (y2))
                                                                                                                                   (\(Z:.i) -> (Z:.(i+1)))
                                                                                                                                   (let newVal orig (Z:.pos) | pos == 0 = 0 :: Int
                                                                                                                                                              | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                                            (newVal orig (Z:.(pos-1)))
                                                                                                                                                                            (orig (Z:.(pos-1)))
                                                                                                                                    in newVal)
                                                                                                                in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig sh -> orig sh), fromList Z [(res!(Z:.((size $ extent res)-1)))]))
                                                                                                in
                                                                                                 y3)
                                                                                               (Repa.map (\x0 -> ((9 :: Int) - ((x0))))
                                                                                                         (let (y3, y4) = (let res = traverse (y2)
                                                                                                                                             (\(Z:.i) -> (Z:.(i+1)))
                                                                                                                                             (let newVal orig sh@(Z:.pos) | pos == (size $ extent $ y2) = 0 :: Int
                                                                                                                                                                          | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                                                        (newVal orig (Z:.(pos+1)))
                                                                                                                                                                                        (orig sh)
                                                                                                                                              in newVal)
                                                                                                                          in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig (Z:.pos) -> orig (Z:.(pos+1)))
                                                                                                                              ,
                                                                                                                              fromList Z [(res!(Z:.0))]))
                                                                                                          in
                                                                                                           y3)))) -- Let block
                                                         in
                                                          let srcArr = y1
                                                              dftArr = y1
                                                              perm   = (\x0 -> (Z :. ((y2) ! (Z:.(x0)))))
                                                              comb   = (\x0 -> \x1 -> ((x0)))
                                                              permute idx comb dftArr perm srcArr
                                                               | idx >= (size $ extent srcArr) = dftArr
                                                               | otherwise = let (Z:.srcIdx) = fromIndex (extent srcArr) idx
                                                                                 newArr = fromFunction (extent dftArr)
                                                                                                       (\sh -> case sh == (perm srcIdx) of
                                                                                                        True  -> (dftArr ! (perm srcIdx)) `comb` (srcArr ! (Z:.srcIdx))
                                                                                                        False -> index dftArr sh)
                                                                             in permute (idx+1) comb newArr perm srcArr
                                                          in
                                                           permute 0 comb dftArr perm srcArr) -- Let block
                                             in
                                              let y0 = (y1) -- binding of Apply block
                                              in
                                               let y1 = (let y0 = (y0) -- binding of Apply block
                                                         in
                                                          let y1 = (y0) -- Let block
                                                          in
                                                           let y2 = (let y2 = (Repa.map (\x0 -> (if ((23 :: Int) == ((32 :: Int) - (1 :: Int)))
                                                                                                  then (((((x0)) `xor` (minBound)) `shiftR` (23 :: Int)) .&. (1 :: Int))
                                                                                                  else ((((x0)) `shiftR` (23 :: Int)) .&. (1 :: Int))))
                                                                                        (y1)) -- Let block
                                                                     in
                                                                      Repa.zipWith (\x0 -> \x1 -> (if (((x0)) == (0 :: Int))
                                                                                                    then (let ((tVar, _)) = ((x1)) in tVar)
                                                                                                    else (let ((_, tVar)) = ((x1)) in tVar)))
                                                                                   (y2)
                                                                                   (Repa.zipWith (\x0 -> \x1 -> (((x0), (x1))))
                                                                                                 (let (y3, y4) = (let res = traverse (Repa.map (\x0 -> ((1 :: Int) `xor` ((x0))))
                                                                                                                                               (y2))
                                                                                                                                     (\(Z:.i) -> (Z:.(i+1)))
                                                                                                                                     (let newVal orig (Z:.pos) | pos == 0 = 0 :: Int
                                                                                                                                                                | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                                              (newVal orig (Z:.(pos-1)))
                                                                                                                                                                              (orig (Z:.(pos-1)))
                                                                                                                                      in newVal)
                                                                                                                  in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig sh -> orig sh), fromList Z [(res!(Z:.((size $ extent res)-1)))]))
                                                                                                  in
                                                                                                   y3)
                                                                                                 (Repa.map (\x0 -> ((9 :: Int) - ((x0))))
                                                                                                           (let (y3, y4) = (let res = traverse (y2)
                                                                                                                                               (\(Z:.i) -> (Z:.(i+1)))
                                                                                                                                               (let newVal orig sh@(Z:.pos) | pos == (size $ extent $ y2) = 0 :: Int
                                                                                                                                                                            | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                                                          (newVal orig (Z:.(pos+1)))
                                                                                                                                                                                          (orig sh)
                                                                                                                                                in newVal)
                                                                                                                            in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig (Z:.pos) -> orig (Z:.(pos+1)))
                                                                                                                                ,
                                                                                                                                fromList Z [(res!(Z:.0))]))
                                                                                                            in
                                                                                                             y3)))) -- Let block
                                                           in
                                                            let srcArr = y1
                                                                dftArr = y1
                                                                perm   = (\x0 -> (Z :. ((y2) ! (Z:.(x0)))))
                                                                comb   = (\x0 -> \x1 -> ((x0)))
                                                                permute idx comb dftArr perm srcArr
                                                                 | idx >= (size $ extent srcArr) = dftArr
                                                                 | otherwise = let (Z:.srcIdx) = fromIndex (extent srcArr) idx
                                                                                   newArr = fromFunction (extent dftArr)
                                                                                                         (\sh -> case sh == (perm srcIdx) of
                                                                                                          True  -> (dftArr ! (perm srcIdx)) `comb` (srcArr ! (Z:.srcIdx))
                                                                                                          False -> index dftArr sh)
                                                                               in permute (idx+1) comb newArr perm srcArr
                                                            in
                                                             permute 0 comb dftArr perm srcArr) -- Let block
                                               in
                                                let y0 = (y1) -- binding of Apply block
                                                in
                                                 let y1 = (let y0 = (y0) -- binding of Apply block
                                                           in
                                                            let y1 = (y0) -- Let block
                                                            in
                                                             let y2 = (let y2 = (Repa.map (\x0 -> (if ((24 :: Int) == ((32 :: Int) - (1 :: Int)))
                                                                                                    then (((((x0)) `xor` (minBound)) `shiftR` (24 :: Int)) .&. (1 :: Int))
                                                                                                    else ((((x0)) `shiftR` (24 :: Int)) .&. (1 :: Int))))
                                                                                          (y1)) -- Let block
                                                                       in
                                                                        Repa.zipWith (\x0 -> \x1 -> (if (((x0)) == (0 :: Int))
                                                                                                      then (let ((tVar, _)) = ((x1)) in tVar)
                                                                                                      else (let ((_, tVar)) = ((x1)) in tVar)))
                                                                                     (y2)
                                                                                     (Repa.zipWith (\x0 -> \x1 -> (((x0), (x1))))
                                                                                                   (let (y3, y4) = (let res = traverse (Repa.map (\x0 -> ((1 :: Int) `xor` ((x0))))
                                                                                                                                                 (y2))
                                                                                                                                       (\(Z:.i) -> (Z:.(i+1)))
                                                                                                                                       (let newVal orig (Z:.pos) | pos == 0 = 0 :: Int
                                                                                                                                                                  | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                                                (newVal orig (Z:.(pos-1)))
                                                                                                                                                                                (orig (Z:.(pos-1)))
                                                                                                                                        in newVal)
                                                                                                                    in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig sh -> orig sh), fromList Z [(res!(Z:.((size $ extent res)-1)))]))
                                                                                                    in
                                                                                                     y3)
                                                                                                   (Repa.map (\x0 -> ((9 :: Int) - ((x0))))
                                                                                                             (let (y3, y4) = (let res = traverse (y2)
                                                                                                                                                 (\(Z:.i) -> (Z:.(i+1)))
                                                                                                                                                 (let newVal orig sh@(Z:.pos) | pos == (size $ extent $ y2) = 0 :: Int
                                                                                                                                                                              | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                                                            (newVal orig (Z:.(pos+1)))
                                                                                                                                                                                            (orig sh)
                                                                                                                                                  in newVal)
                                                                                                                              in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig (Z:.pos) -> orig (Z:.(pos+1)))
                                                                                                                                  ,
                                                                                                                                  fromList Z [(res!(Z:.0))]))
                                                                                                              in
                                                                                                               y3)))) -- Let block
                                                             in
                                                              let srcArr = y1
                                                                  dftArr = y1
                                                                  perm   = (\x0 -> (Z :. ((y2) ! (Z:.(x0)))))
                                                                  comb   = (\x0 -> \x1 -> ((x0)))
                                                                  permute idx comb dftArr perm srcArr
                                                                   | idx >= (size $ extent srcArr) = dftArr
                                                                   | otherwise = let (Z:.srcIdx) = fromIndex (extent srcArr) idx
                                                                                     newArr = fromFunction (extent dftArr)
                                                                                                           (\sh -> case sh == (perm srcIdx) of
                                                                                                            True  -> (dftArr ! (perm srcIdx)) `comb` (srcArr ! (Z:.srcIdx))
                                                                                                            False -> index dftArr sh)
                                                                                 in permute (idx+1) comb newArr perm srcArr
                                                              in
                                                               permute 0 comb dftArr perm srcArr) -- Let block
                                                 in
                                                  let y0 = (y1) -- binding of Apply block
                                                  in
                                                   let y1 = (let y0 = (y0) -- binding of Apply block
                                                             in
                                                              let y1 = (y0) -- Let block
                                                              in
                                                               let y2 = (let y2 = (Repa.map (\x0 -> (if ((25 :: Int) == ((32 :: Int) - (1 :: Int)))
                                                                                                      then (((((x0)) `xor` (minBound)) `shiftR` (25 :: Int)) .&. (1 :: Int))
                                                                                                      else ((((x0)) `shiftR` (25 :: Int)) .&. (1 :: Int))))
                                                                                            (y1)) -- Let block
                                                                         in
                                                                          Repa.zipWith (\x0 -> \x1 -> (if (((x0)) == (0 :: Int))
                                                                                                        then (let ((tVar, _)) = ((x1)) in tVar)
                                                                                                        else (let ((_, tVar)) = ((x1)) in tVar)))
                                                                                       (y2)
                                                                                       (Repa.zipWith (\x0 -> \x1 -> (((x0), (x1))))
                                                                                                     (let (y3, y4) = (let res = traverse (Repa.map (\x0 -> ((1 :: Int) `xor` ((x0))))
                                                                                                                                                   (y2))
                                                                                                                                         (\(Z:.i) -> (Z:.(i+1)))
                                                                                                                                         (let newVal orig (Z:.pos) | pos == 0 = 0 :: Int
                                                                                                                                                                    | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                                                  (newVal orig (Z:.(pos-1)))
                                                                                                                                                                                  (orig (Z:.(pos-1)))
                                                                                                                                          in newVal)
                                                                                                                      in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig sh -> orig sh), fromList Z [(res!(Z:.((size $ extent res)-1)))]))
                                                                                                      in
                                                                                                       y3)
                                                                                                     (Repa.map (\x0 -> ((9 :: Int) - ((x0))))
                                                                                                               (let (y3, y4) = (let res = traverse (y2)
                                                                                                                                                   (\(Z:.i) -> (Z:.(i+1)))
                                                                                                                                                   (let newVal orig sh@(Z:.pos) | pos == (size $ extent $ y2) = 0 :: Int
                                                                                                                                                                                | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                                                              (newVal orig (Z:.(pos+1)))
                                                                                                                                                                                              (orig sh)
                                                                                                                                                    in newVal)
                                                                                                                                in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig (Z:.pos) -> orig (Z:.(pos+1)))
                                                                                                                                    ,
                                                                                                                                    fromList Z [(res!(Z:.0))]))
                                                                                                                in
                                                                                                                 y3)))) -- Let block
                                                               in
                                                                let srcArr = y1
                                                                    dftArr = y1
                                                                    perm   = (\x0 -> (Z :. ((y2) ! (Z:.(x0)))))
                                                                    comb   = (\x0 -> \x1 -> ((x0)))
                                                                    permute idx comb dftArr perm srcArr
                                                                     | idx >= (size $ extent srcArr) = dftArr
                                                                     | otherwise = let (Z:.srcIdx) = fromIndex (extent srcArr) idx
                                                                                       newArr = fromFunction (extent dftArr)
                                                                                                             (\sh -> case sh == (perm srcIdx) of
                                                                                                              True  -> (dftArr ! (perm srcIdx)) `comb` (srcArr ! (Z:.srcIdx))
                                                                                                              False -> index dftArr sh)
                                                                                   in permute (idx+1) comb newArr perm srcArr
                                                                in
                                                                 permute 0 comb dftArr perm srcArr) -- Let block
                                                   in
                                                    let y0 = (y1) -- binding of Apply block
                                                    in
                                                     let y1 = (let y0 = (y0) -- binding of Apply block
                                                               in
                                                                let y1 = (y0) -- Let block
                                                                in
                                                                 let y2 = (let y2 = (Repa.map (\x0 -> (if ((26 :: Int) == ((32 :: Int) - (1 :: Int)))
                                                                                                        then (((((x0)) `xor` (minBound)) `shiftR` (26 :: Int)) .&. (1 :: Int))
                                                                                                        else ((((x0)) `shiftR` (26 :: Int)) .&. (1 :: Int))))
                                                                                              (y1)) -- Let block
                                                                           in
                                                                            Repa.zipWith (\x0 -> \x1 -> (if (((x0)) == (0 :: Int))
                                                                                                          then (let ((tVar, _)) = ((x1)) in tVar)
                                                                                                          else (let ((_, tVar)) = ((x1)) in tVar)))
                                                                                         (y2)
                                                                                         (Repa.zipWith (\x0 -> \x1 -> (((x0), (x1))))
                                                                                                       (let (y3, y4) = (let res = traverse (Repa.map (\x0 -> ((1 :: Int) `xor` ((x0))))
                                                                                                                                                     (y2))
                                                                                                                                           (\(Z:.i) -> (Z:.(i+1)))
                                                                                                                                           (let newVal orig (Z:.pos) | pos == 0 = 0 :: Int
                                                                                                                                                                      | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                                                    (newVal orig (Z:.(pos-1)))
                                                                                                                                                                                    (orig (Z:.(pos-1)))
                                                                                                                                            in newVal)
                                                                                                                        in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig sh -> orig sh), fromList Z [(res!(Z:.((size $ extent res)-1)))]))
                                                                                                        in
                                                                                                         y3)
                                                                                                       (Repa.map (\x0 -> ((9 :: Int) - ((x0))))
                                                                                                                 (let (y3, y4) = (let res = traverse (y2)
                                                                                                                                                     (\(Z:.i) -> (Z:.(i+1)))
                                                                                                                                                     (let newVal orig sh@(Z:.pos) | pos == (size $ extent $ y2) = 0 :: Int
                                                                                                                                                                                  | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                                                                (newVal orig (Z:.(pos+1)))
                                                                                                                                                                                                (orig sh)
                                                                                                                                                      in newVal)
                                                                                                                                  in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig (Z:.pos) -> orig (Z:.(pos+1)))
                                                                                                                                      ,
                                                                                                                                      fromList Z [(res!(Z:.0))]))
                                                                                                                  in
                                                                                                                   y3)))) -- Let block
                                                                 in
                                                                  let srcArr = y1
                                                                      dftArr = y1
                                                                      perm   = (\x0 -> (Z :. ((y2) ! (Z:.(x0)))))
                                                                      comb   = (\x0 -> \x1 -> ((x0)))
                                                                      permute idx comb dftArr perm srcArr
                                                                       | idx >= (size $ extent srcArr) = dftArr
                                                                       | otherwise = let (Z:.srcIdx) = fromIndex (extent srcArr) idx
                                                                                         newArr = fromFunction (extent dftArr)
                                                                                                               (\sh -> case sh == (perm srcIdx) of
                                                                                                                True  -> (dftArr ! (perm srcIdx)) `comb` (srcArr ! (Z:.srcIdx))
                                                                                                                False -> index dftArr sh)
                                                                                     in permute (idx+1) comb newArr perm srcArr
                                                                  in
                                                                   permute 0 comb dftArr perm srcArr) -- Let block
                                                     in
                                                      let y0 = (y1) -- binding of Apply block
                                                      in
                                                       let y1 = (let y0 = (y0) -- binding of Apply block
                                                                 in
                                                                  let y1 = (y0) -- Let block
                                                                  in
                                                                   let y2 = (let y2 = (Repa.map (\x0 -> (if ((27 :: Int) == ((32 :: Int) - (1 :: Int)))
                                                                                                          then (((((x0)) `xor` (minBound)) `shiftR` (27 :: Int)) .&. (1 :: Int))
                                                                                                          else ((((x0)) `shiftR` (27 :: Int)) .&. (1 :: Int))))
                                                                                                (y1)) -- Let block
                                                                             in
                                                                              Repa.zipWith (\x0 -> \x1 -> (if (((x0)) == (0 :: Int))
                                                                                                            then (let ((tVar, _)) = ((x1)) in tVar)
                                                                                                            else (let ((_, tVar)) = ((x1)) in tVar)))
                                                                                           (y2)
                                                                                           (Repa.zipWith (\x0 -> \x1 -> (((x0), (x1))))
                                                                                                         (let (y3, y4) = (let res = traverse (Repa.map (\x0 -> ((1 :: Int) `xor` ((x0))))
                                                                                                                                                       (y2))
                                                                                                                                             (\(Z:.i) -> (Z:.(i+1)))
                                                                                                                                             (let newVal orig (Z:.pos) | pos == 0 = 0 :: Int
                                                                                                                                                                        | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                                                      (newVal orig (Z:.(pos-1)))
                                                                                                                                                                                      (orig (Z:.(pos-1)))
                                                                                                                                              in newVal)
                                                                                                                          in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig sh -> orig sh), fromList Z [(res!(Z:.((size $ extent res)-1)))]))
                                                                                                          in
                                                                                                           y3)
                                                                                                         (Repa.map (\x0 -> ((9 :: Int) - ((x0))))
                                                                                                                   (let (y3, y4) = (let res = traverse (y2)
                                                                                                                                                       (\(Z:.i) -> (Z:.(i+1)))
                                                                                                                                                       (let newVal orig sh@(Z:.pos) | pos == (size $ extent $ y2) = 0 :: Int
                                                                                                                                                                                    | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                                                                  (newVal orig (Z:.(pos+1)))
                                                                                                                                                                                                  (orig sh)
                                                                                                                                                        in newVal)
                                                                                                                                    in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig (Z:.pos) -> orig (Z:.(pos+1)))
                                                                                                                                        ,
                                                                                                                                        fromList Z [(res!(Z:.0))]))
                                                                                                                    in
                                                                                                                     y3)))) -- Let block
                                                                   in
                                                                    let srcArr = y1
                                                                        dftArr = y1
                                                                        perm   = (\x0 -> (Z :. ((y2) ! (Z:.(x0)))))
                                                                        comb   = (\x0 -> \x1 -> ((x0)))
                                                                        permute idx comb dftArr perm srcArr
                                                                         | idx >= (size $ extent srcArr) = dftArr
                                                                         | otherwise = let (Z:.srcIdx) = fromIndex (extent srcArr) idx
                                                                                           newArr = fromFunction (extent dftArr)
                                                                                                                 (\sh -> case sh == (perm srcIdx) of
                                                                                                                  True  -> (dftArr ! (perm srcIdx)) `comb` (srcArr ! (Z:.srcIdx))
                                                                                                                  False -> index dftArr sh)
                                                                                       in permute (idx+1) comb newArr perm srcArr
                                                                    in
                                                                     permute 0 comb dftArr perm srcArr) -- Let block
                                                       in
                                                        let y0 = (y1) -- binding of Apply block
                                                        in
                                                         let y1 = (let y0 = (y0) -- binding of Apply block
                                                                   in
                                                                    let y1 = (y0) -- Let block
                                                                    in
                                                                     let y2 = (let y2 = (Repa.map (\x0 -> (if ((28 :: Int) == ((32 :: Int) - (1 :: Int)))
                                                                                                            then (((((x0)) `xor` (minBound)) `shiftR` (28 :: Int)) .&. (1 :: Int))
                                                                                                            else ((((x0)) `shiftR` (28 :: Int)) .&. (1 :: Int))))
                                                                                                  (y1)) -- Let block
                                                                               in
                                                                                Repa.zipWith (\x0 -> \x1 -> (if (((x0)) == (0 :: Int))
                                                                                                              then (let ((tVar, _)) = ((x1)) in tVar)
                                                                                                              else (let ((_, tVar)) = ((x1)) in tVar)))
                                                                                             (y2)
                                                                                             (Repa.zipWith (\x0 -> \x1 -> (((x0), (x1))))
                                                                                                           (let (y3, y4) = (let res = traverse (Repa.map (\x0 -> ((1 :: Int) `xor` ((x0))))
                                                                                                                                                         (y2))
                                                                                                                                               (\(Z:.i) -> (Z:.(i+1)))
                                                                                                                                               (let newVal orig (Z:.pos) | pos == 0 = 0 :: Int
                                                                                                                                                                          | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                                                        (newVal orig (Z:.(pos-1)))
                                                                                                                                                                                        (orig (Z:.(pos-1)))
                                                                                                                                                in newVal)
                                                                                                                            in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig sh -> orig sh), fromList Z [(res!(Z:.((size $ extent res)-1)))]))
                                                                                                            in
                                                                                                             y3)
                                                                                                           (Repa.map (\x0 -> ((9 :: Int) - ((x0))))
                                                                                                                     (let (y3, y4) = (let res = traverse (y2)
                                                                                                                                                         (\(Z:.i) -> (Z:.(i+1)))
                                                                                                                                                         (let newVal orig sh@(Z:.pos) | pos == (size $ extent $ y2) = 0 :: Int
                                                                                                                                                                                      | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                                                                    (newVal orig (Z:.(pos+1)))
                                                                                                                                                                                                    (orig sh)
                                                                                                                                                          in newVal)
                                                                                                                                      in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig (Z:.pos) -> orig (Z:.(pos+1)))
                                                                                                                                          ,
                                                                                                                                          fromList Z [(res!(Z:.0))]))
                                                                                                                      in
                                                                                                                       y3)))) -- Let block
                                                                     in
                                                                      let srcArr = y1
                                                                          dftArr = y1
                                                                          perm   = (\x0 -> (Z :. ((y2) ! (Z:.(x0)))))
                                                                          comb   = (\x0 -> \x1 -> ((x0)))
                                                                          permute idx comb dftArr perm srcArr
                                                                           | idx >= (size $ extent srcArr) = dftArr
                                                                           | otherwise = let (Z:.srcIdx) = fromIndex (extent srcArr) idx
                                                                                             newArr = fromFunction (extent dftArr)
                                                                                                                   (\sh -> case sh == (perm srcIdx) of
                                                                                                                    True  -> (dftArr ! (perm srcIdx)) `comb` (srcArr ! (Z:.srcIdx))
                                                                                                                    False -> index dftArr sh)
                                                                                         in permute (idx+1) comb newArr perm srcArr
                                                                      in
                                                                       permute 0 comb dftArr perm srcArr) -- Let block
                                                         in
                                                          let y0 = (y1) -- binding of Apply block
                                                          in
                                                           let y1 = (let y0 = (y0) -- binding of Apply block
                                                                     in
                                                                      let y1 = (y0) -- Let block
                                                                      in
                                                                       let y2 = (let y2 = (Repa.map (\x0 -> (if ((29 :: Int) == ((32 :: Int) - (1 :: Int)))
                                                                                                              then (((((x0)) `xor` (minBound)) `shiftR` (29 :: Int)) .&. (1 :: Int))
                                                                                                              else ((((x0)) `shiftR` (29 :: Int)) .&. (1 :: Int))))
                                                                                                    (y1)) -- Let block
                                                                                 in
                                                                                  Repa.zipWith (\x0 -> \x1 -> (if (((x0)) == (0 :: Int))
                                                                                                                then (let ((tVar, _)) = ((x1)) in tVar)
                                                                                                                else (let ((_, tVar)) = ((x1)) in tVar)))
                                                                                               (y2)
                                                                                               (Repa.zipWith (\x0 -> \x1 -> (((x0), (x1))))
                                                                                                             (let (y3, y4) = (let res = traverse (Repa.map (\x0 -> ((1 :: Int) `xor` ((x0))))
                                                                                                                                                           (y2))
                                                                                                                                                 (\(Z:.i) -> (Z:.(i+1)))
                                                                                                                                                 (let newVal orig (Z:.pos) | pos == 0 = 0 :: Int
                                                                                                                                                                            | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                                                          (newVal orig (Z:.(pos-1)))
                                                                                                                                                                                          (orig (Z:.(pos-1)))
                                                                                                                                                  in newVal)
                                                                                                                              in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig sh -> orig sh), fromList Z [(res!(Z:.((size $ extent res)-1)))]))
                                                                                                              in
                                                                                                               y3)
                                                                                                             (Repa.map (\x0 -> ((9 :: Int) - ((x0))))
                                                                                                                       (let (y3, y4) = (let res = traverse (y2)
                                                                                                                                                           (\(Z:.i) -> (Z:.(i+1)))
                                                                                                                                                           (let newVal orig sh@(Z:.pos) | pos == (size $ extent $ y2) = 0 :: Int
                                                                                                                                                                                        | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                                                                      (newVal orig (Z:.(pos+1)))
                                                                                                                                                                                                      (orig sh)
                                                                                                                                                            in newVal)
                                                                                                                                        in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig (Z:.pos) -> orig (Z:.(pos+1)))
                                                                                                                                            ,
                                                                                                                                            fromList Z [(res!(Z:.0))]))
                                                                                                                        in
                                                                                                                         y3)))) -- Let block
                                                                       in
                                                                        let srcArr = y1
                                                                            dftArr = y1
                                                                            perm   = (\x0 -> (Z :. ((y2) ! (Z:.(x0)))))
                                                                            comb   = (\x0 -> \x1 -> ((x0)))
                                                                            permute idx comb dftArr perm srcArr
                                                                             | idx >= (size $ extent srcArr) = dftArr
                                                                             | otherwise = let (Z:.srcIdx) = fromIndex (extent srcArr) idx
                                                                                               newArr = fromFunction (extent dftArr)
                                                                                                                     (\sh -> case sh == (perm srcIdx) of
                                                                                                                      True  -> (dftArr ! (perm srcIdx)) `comb` (srcArr ! (Z:.srcIdx))
                                                                                                                      False -> index dftArr sh)
                                                                                           in permute (idx+1) comb newArr perm srcArr
                                                                        in
                                                                         permute 0 comb dftArr perm srcArr) -- Let block
                                                           in
                                                            let y0 = (y1) -- binding of Apply block
                                                            in
                                                             let y1 = (let y0 = (y0) -- binding of Apply block
                                                                       in
                                                                        let y1 = (y0) -- Let block
                                                                        in
                                                                         let y2 = (let y2 = (Repa.map (\x0 -> (if ((30 :: Int) == ((32 :: Int) - (1 :: Int)))
                                                                                                                then (((((x0)) `xor` (minBound)) `shiftR` (30 :: Int)) .&. (1 :: Int))
                                                                                                                else ((((x0)) `shiftR` (30 :: Int)) .&. (1 :: Int))))
                                                                                                      (y1)) -- Let block
                                                                                   in
                                                                                    Repa.zipWith (\x0 -> \x1 -> (if (((x0)) == (0 :: Int))
                                                                                                                  then (let ((tVar, _)) = ((x1)) in tVar)
                                                                                                                  else (let ((_, tVar)) = ((x1)) in tVar)))
                                                                                                 (y2)
                                                                                                 (Repa.zipWith (\x0 -> \x1 -> (((x0), (x1))))
                                                                                                               (let (y3, y4) = (let res = traverse (Repa.map (\x0 -> ((1 :: Int) `xor` ((x0))))
                                                                                                                                                             (y2))
                                                                                                                                                   (\(Z:.i) -> (Z:.(i+1)))
                                                                                                                                                   (let newVal orig (Z:.pos) | pos == 0 = 0 :: Int
                                                                                                                                                                              | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                                                            (newVal orig (Z:.(pos-1)))
                                                                                                                                                                                            (orig (Z:.(pos-1)))
                                                                                                                                                    in newVal)
                                                                                                                                in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig sh -> orig sh), fromList Z [(res!(Z:.((size $ extent res)-1)))]))
                                                                                                                in
                                                                                                                 y3)
                                                                                                               (Repa.map (\x0 -> ((9 :: Int) - ((x0))))
                                                                                                                         (let (y3, y4) = (let res = traverse (y2)
                                                                                                                                                             (\(Z:.i) -> (Z:.(i+1)))
                                                                                                                                                             (let newVal orig sh@(Z:.pos) | pos == (size $ extent $ y2) = 0 :: Int
                                                                                                                                                                                          | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                                                                        (newVal orig (Z:.(pos+1)))
                                                                                                                                                                                                        (orig sh)
                                                                                                                                                              in newVal)
                                                                                                                                          in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig (Z:.pos) -> orig (Z:.(pos+1)))
                                                                                                                                              ,
                                                                                                                                              fromList Z [(res!(Z:.0))]))
                                                                                                                          in
                                                                                                                           y3)))) -- Let block
                                                                         in
                                                                          let srcArr = y1
                                                                              dftArr = y1
                                                                              perm   = (\x0 -> (Z :. ((y2) ! (Z:.(x0)))))
                                                                              comb   = (\x0 -> \x1 -> ((x0)))
                                                                              permute idx comb dftArr perm srcArr
                                                                               | idx >= (size $ extent srcArr) = dftArr
                                                                               | otherwise = let (Z:.srcIdx) = fromIndex (extent srcArr) idx
                                                                                                 newArr = fromFunction (extent dftArr)
                                                                                                                       (\sh -> case sh == (perm srcIdx) of
                                                                                                                        True  -> (dftArr ! (perm srcIdx)) `comb` (srcArr ! (Z:.srcIdx))
                                                                                                                        False -> index dftArr sh)
                                                                                             in permute (idx+1) comb newArr perm srcArr
                                                                          in
                                                                           permute 0 comb dftArr perm srcArr) -- Let block
                                                             in
                                                              let y0 = (y1) -- binding of Apply block
                                                              in
                                                               let y1 = (y0) -- Let block
                                                               in
                                                                let y2 = (let y2 = (Repa.map (\x0 -> (if ((31 :: Int) == ((32 :: Int) - (1 :: Int)))
                                                                                                       then (((((x0)) `xor` (minBound)) `shiftR` (31 :: Int)) .&. (1 :: Int))
                                                                                                       else ((((x0)) `shiftR` (31 :: Int)) .&. (1 :: Int))))
                                                                                             (y1)) -- Let block
                                                                          in
                                                                           Repa.zipWith (\x0 -> \x1 -> (if (((x0)) == (0 :: Int))
                                                                                                         then (let ((tVar, _)) = ((x1)) in tVar)
                                                                                                         else (let ((_, tVar)) = ((x1)) in tVar)))
                                                                                        (y2)
                                                                                        (Repa.zipWith (\x0 -> \x1 -> (((x0), (x1))))
                                                                                                      (let (y3, y4) = (let res = traverse (Repa.map (\x0 -> ((1 :: Int) `xor` ((x0))))
                                                                                                                                                    (y2))
                                                                                                                                          (\(Z:.i) -> (Z:.(i+1)))
                                                                                                                                          (let newVal orig (Z:.pos) | pos == 0 = 0 :: Int
                                                                                                                                                                     | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                                                   (newVal orig (Z:.(pos-1)))
                                                                                                                                                                                   (orig (Z:.(pos-1)))
                                                                                                                                           in newVal)
                                                                                                                       in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig sh -> orig sh), fromList Z [(res!(Z:.((size $ extent res)-1)))]))
                                                                                                       in
                                                                                                        y3)
                                                                                                      (Repa.map (\x0 -> ((9 :: Int) - ((x0))))
                                                                                                                (let (y3, y4) = (let res = traverse (y2)
                                                                                                                                                    (\(Z:.i) -> (Z:.(i+1)))
                                                                                                                                                    (let newVal orig sh@(Z:.pos) | pos == (size $ extent $ y2) = 0 :: Int
                                                                                                                                                                                 | otherwise = (\x0 -> \x1 -> (((x0)) + ((x1))))
                                                                                                                                                                                               (newVal orig (Z:.(pos+1)))
                                                                                                                                                                                               (orig sh)
                                                                                                                                                     in newVal)
                                                                                                                                 in (traverse res (\(Z:.i) -> (Z:.(i-1))) (\orig (Z:.pos) -> orig (Z:.(pos+1)))
                                                                                                                                     ,
                                                                                                                                     fromList Z [(res!(Z:.0))]))
                                                                                                                 in
                                                                                                                  y3)))) -- Let block
                                                                in
                                                                 let srcArr = y1
                                                                     dftArr = y1
                                                                     perm   = (\x0 -> (Z :. ((y2) ! (Z:.(x0)))))
                                                                     comb   = (\x0 -> \x1 -> ((x0)))
                                                                     permute idx comb dftArr perm srcArr
                                                                      | idx >= (size $ extent srcArr) = dftArr
                                                                      | otherwise = let (Z:.srcIdx) = fromIndex (extent srcArr) idx
                                                                                        newArr = fromFunction (extent dftArr)
                                                                                                              (\sh -> case sh == (perm srcIdx) of
                                                                                                               True  -> (dftArr ! (perm srcIdx)) `comb` (srcArr ! (Z:.srcIdx))
                                                                                                               False -> index dftArr sh)
                                                                                    in permute (idx+1) comb newArr perm srcArr
                                                                 in
                                                                  permute 0 comb dftArr perm srcArr
 
class (Elt e, Shape sh) => MyStencil sh e tup where
 stencilData :: (sh -> e) -> sh -> tup
instance (Elt e) => MyStencil (Z:.Int) e (e, e, e) where
 stencilData rf (Z:.idx) = ( (rf' (idx-1))
                           , (rf' (idx))
                           , (rf' (idx+1))
                           )
                           where
                            rf' id = rf (Z:.id)
instance (Elt e) => MyStencil (Z:.Int) e (e, e, e, e, e) where
 stencilData rf (Z:.idx) = ( (rf' (idx-2))
                           , (rf' (idx-1))
                           , (rf' (idx))
                           , (rf' (idx+1))
                           , (rf' (idx+2))
                           )
                           where
                            rf' id = rf (Z:.id)
instance (Elt e) => MyStencil (Z:.Int) e (e, e, e, e, e, e, e) where
 stencilData rf (Z:.idx) = ( (rf' (idx-3))
                           , (rf' (idx-2))
                           , (rf' (idx-1))
                           , (rf' (idx))
                           , (rf' (idx+1))
                           , (rf' (idx+2))
                           , (rf' (idx+3))
                           )
                           where
                            rf' id = rf (Z:.id)
instance (Elt e) => MyStencil (Z:.Int) e (e, e, e, e, e, e, e, e, e) where
 stencilData rf (Z:.idx) = ( (rf' (idx-4))
                           , (rf' (idx-3))
                           , (rf' (idx-2))
                           , (rf' (idx-1))
                           , (rf' (idx))
                           , (rf' (idx+1))
                           , (rf' (idx+2))
                           , (rf' (idx+3))
                           , (rf' (idx+4))
                           )
                           where
                            rf' id = rf (Z:.id)
instance (MyStencil (sh:.Int) a row1,
          MyStencil (sh:.Int) a row2,
          MyStencil (sh:.Int) a row3) => MyStencil (sh:.Int:.Int) a (row1, row2, row3) where
 stencilData rf (ix:.i) = ( stencilData (rf' (i-1)) ix
                          , stencilData (rf' (i)) ix
                          , stencilData (rf' (i+1)) ix
                          )
                          where
                           rf' d ds = rf (ds :. d)
instance (MyStencil (sh:.Int) a row1,
          MyStencil (sh:.Int) a row2,
          MyStencil (sh:.Int) a row3,
          MyStencil (sh:.Int) a row4,
          MyStencil (sh:.Int) a row5) => MyStencil (sh:.Int:.Int) a (row1, row2, row3, row4, row5) where
 stencilData rf (ix:.i) = ( stencilData (rf' (i-2)) ix
                          , stencilData (rf' (i-1)) ix
                          , stencilData (rf' (i)) ix
                          , stencilData (rf' (i+1)) ix
                          , stencilData (rf' (i+2)) ix
                          )
                          where
                           rf' d ds = rf (ds :. d)
instance (MyStencil (sh:.Int) a row1,
          MyStencil (sh:.Int) a row2,
          MyStencil (sh:.Int) a row3,
          MyStencil (sh:.Int) a row4,
          MyStencil (sh:.Int) a row5,
          MyStencil (sh:.Int) a row6,
          MyStencil (sh:.Int) a row7) => MyStencil (sh:.Int:.Int) a (row1, row2, row3, row4, row5, row6, row7) where
 stencilData rf (ix:.i) = ( stencilData (rf' (i-3)) ix
                          , stencilData (rf' (i-2)) ix
                          , stencilData (rf' (i-1)) ix
                          , stencilData (rf' (i)) ix
                          , stencilData (rf' (i+1)) ix
                          , stencilData (rf' (i+2)) ix
                          , stencilData (rf' (i+3)) ix
                          )
                          where
                           rf' d ds = rf (ds :. d)
instance (MyStencil (sh:.Int) a row1,
          MyStencil (sh:.Int) a row2,
          MyStencil (sh:.Int) a row3,
          MyStencil (sh:.Int) a row4,
          MyStencil (sh:.Int) a row5,
          MyStencil (sh:.Int) a row6,
          MyStencil (sh:.Int) a row7,
          MyStencil (sh:.Int) a row8,
          MyStencil (sh:.Int) a row9) => MyStencil (sh:.Int:.Int) a (row1, row2, row3, row4, row5, row6, row7, row8, row9) where
 stencilData rf (ix:.i) = ( stencilData (rf' (i-4)) ix
                          , stencilData (rf' (i-3)) ix
                          , stencilData (rf' (i-2)) ix
                          , stencilData (rf' (i-1)) ix
                          , stencilData (rf' (i)) ix
                          , stencilData (rf' (i+1)) ix
                          , stencilData (rf' (i+2)) ix
                          , stencilData (rf' (i+3)) ix
                          , stencilData (rf' (i+4)) ix
                          )
                          where
                           rf' d ds = rf (ds :. d)
data Boundary a = Clamp
                | Mirror
                | Wrap
                | Constant a
bound :: (Shape sh, Elt a)
      => (sh -> a)
      -> Boundary a
      -> sh
      -> sh
      -> a
bound lookup bndy sh ix =
 case bound' (listOfShape sh) (listOfShape ix) bndy of
  Left  val -> val
  Right sh  -> lookup (shapeOfList sh)
bound' :: [Int] -> [Int] -> Boundary a -> Either a [Int]
bound' (sh:shs) (ix:ixs) bndy
 | ix < 0    = case bndy of
                Clamp     -> bound' shs ixs bndy `addDim` 0
                Mirror    -> bound' shs ixs bndy `addDim` (-ix)
                Wrap      -> bound' shs ixs bndy `addDim` (sh+ix)
                Constant e -> Left e
 | ix >= sh  = case bndy of
                Clamp     -> bound' shs ixs bndy `addDim` (sh-1)
                Mirror    -> bound' shs ixs bndy `addDim` (sh-(ix-sh+2))
                Wrap      -> bound' shs ixs bndy `addDim` (ix-sh)
                Constant e -> Left e
 | otherwise = bound' shs ixs bndy `addDim` ix
 where
  addDim (Right ds) d = Right (d:ds)
  addDim (Left  e)  _ = Left  e
bound' [] [] _ = Right []
