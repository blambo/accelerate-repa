import Data.Array.Accelerate as Acc
import Data.Array.Accelerate.Repa as Repa
import Data.Array.Accelerate.Interpreter as Interpreter

main = do
   -- stencil
   putStrLn "---STENCIL---"
   putStrLn $ Repa.run $ myStencil
   --putStrLn $ show $ Interpreter.run $ myStencil
   putStrLn $ show $ Repa.run $ myStencil
   -- stencil2

myStencil :: Acc (Vector Int)
myStencil = stencil myFun (Constant 0) $ theArray

theArray :: Acc (Vector Int)
theArray = generate (index1 10) (\_ -> constant 1)

--myFun :: (Integral a) => (a, a, a) -> a
myFun (a,b,c) = a + b + c
