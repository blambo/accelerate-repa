module BlackScholes where

import Data.Array.Accelerate as Acc
import Data.Array.Accelerate.Repa as Repa

riskfree, volatility :: Float
riskfree   = 0.02
volatility = 0.30

horner :: Num a => [a] -> a -> a
horner coeff x = foldr1 madd coeff
  where
    madd a b = b*x + a

cnd' :: Floating a => a -> a
cnd' d =
  let poly     = horner coeff
      coeff    = [0.0,0.31938153,-0.356563782,1.781477937,-1.821255978,1.330274429]
      rsqrt2pi = 0.39894228040143267793994605993438
      k        = 1.0 / (1.0 + 0.2316419 * abs d)
  in
  rsqrt2pi * exp (-0.5*d*d) * poly k

blackscholesAcc :: Vector (Float, Float, Float) -> Acc (Vector (Float, Float))
blackscholesAcc xs = Acc.map go (Acc.use xs)
  where
  go x =
    let (price, strike, years) = Acc.unlift x
        r     = Acc.constant riskfree
        v     = Acc.constant volatility
        sqrtT = sqrt years
        d1    = (log (price / strike) + (r + 0.5 * v * v) * years) / (v * sqrtT)
        d2    = d1 - v * sqrtT
        cnd d = d >* 0 ? (1.0 - cnd' d, cnd' d)
        cndD1 = cnd d1
        cndD2 = cnd d2
        expRT = exp (-r * years)
    in
    Acc.lift ( price * cndD1 - strike * expRT * cndD2
             , strike * expRT * (1.0 - cndD2) - price * (1.0 - cndD1))

main = do
   putStrLn $ Repa.run $ blackscholesAcc
      $ fromList (Z:.3) [ (1.0,2.0,3.0)
                        , (1.5,2.5,3.5)
                        , (1.3,0.3,3.3)
                        ]
