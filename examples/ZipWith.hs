import Data.Array.Accelerate      as Acc
import Data.Array.Accelerate.Repa as Repa

main :: IO ()
main = do
   putStrLn $ Repa.run $ Acc.zipWith (\x y -> x) (use $ fromList Z [3::Int])
                                              (use $ fromList Z [4::Int])
