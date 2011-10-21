{-# LANGUAGE TypeOperators #-}
import Data.Array.Accelerate as Acc
import Data.Array.Accelerate.Repa as Repa
import Data.Array.Accelerate.Interpreter as Interpreter

main = do
   -- stencil
   putStrLn "---STENCIL---"
   putStrLn $ Repa.run $ my1DStencil
   --putStrLn $ show $ Interpreter.run $ myStencil
   putStrLn $ Repa.run $ my2DStencil
   -- stencil2

my1DStencil :: Acc (Vector Int)
my1DStencil = stencil myFun (Constant 0) $ the1DArray

the1DArray :: Acc (Vector Int)
the1DArray = generate (index1 10) (\_ -> constant 1)

the2DArray :: Acc (Array (Z:.Int:.Int) Int)
the2DArray = generate (index2 (index1 10) 10) (\_ -> constant 1)

my2DStencil = stencil my2DFun (Constant 0) $ the2DArray


index2 :: Exp (Z:.Int) -> Int -> Exp (Z:.Int:.Int)
index2 sh x = lift ( sh :. x )

myFun (a,b,c) = a + b + c

my2DFun :: Stencil5x3 Int -> Exp Int
my2DFun  ( (a1, a2, a3, b1, b2)
         , (a4, a5, a6, b3, b4)
         , (a7, a8, a9, b5, b6)
         )
         = a2 + a4 + a6 + a8
