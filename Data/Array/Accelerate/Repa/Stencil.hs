-- |
-- Module     : Data.Array.Accelerate.Repa.Stencil
--
-- Maintainer : Ben Lambert-Smith <blambo+github@gmail.com>
--
-- This module is for generating the code necessary for Stencil
-- operations that behave in the same way as in Accelerate

module Data.Array.Accelerate.Repa.Stencil
   ( stencilDoc
   )
   where

import Text.PrettyPrint

stencilDoc
 =  classDefDoc
 $$ baseStencils

classDefDoc
 =  text "class (Elt e, Shape sh) => MyStencil sh e tup where"
 $$ nest 1 (text "stencilData :: (sh -> e) -> Array sh e -> sh -> tup")

baseStencils
 =  text "instance (Elt e) => MyStencil (Z:.Int) e (e, e, e) where"
 $$ nest 1 ((stenData "Z:.idx")
      <+> (lparen <+> rf "idx-1"
        $$ comma  <+> rf "idx"
        $$ comma  <+> rf "idx+1"
        $$ rparen
      $$ text "where"
      $$ nest 1 (text "rf' id = rf (Z:.id)")))

rf :: String -> Doc
rf s = parens $ text "rf'" <+> parens (text s)

stenData :: String -> Doc
stenData s = text "stencilData rf arr" <+> parens (text s) <+> equals

{-
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeOperators #-}
import Data.Array.Repa

class (Elt e, Shape sh) => MyStencil sh e tup where
   stencilData :: (sh -> e) -> Array sh e -> sh -> tup

instance (Elt e) => MyStencil (Z:.Int) e (e, e, e) where
   stencilData rf arr (Z:.idx) = ( (rf' (idx-1))
                                 , (rf' idx)
                                 , (rf' (idx+1))
                                 )
                               where
                                 rf' id = rf (Z:.id)

instance (Elt e) => MyStencil (Z:.Int) e (e, e, e, e, e) where
   stencilData rf arr (Z:.idx) = ( (rf' (idx-2))
                                 , (rf' (idx-1))
                                 , (rf' idx)
                                 , (rf' (idx+1))
                                 , (rf' (idx+2))
                                 )
                               where
                                 rf' id = rf (Z:.id)

instance (Elt e) => MyStencil (Z:.Int) e (e, e, e, e, e, e, e) where
   stencilData rf arr (Z:.idx) = ( (rf' (idx-3))
                                 , (rf' (idx-2))
                                 , (rf' (idx-1))
                                 , (rf' idx)
                                 , (rf' (idx+1))
                                 , (rf' (idx+2))
                                 , (rf' (idx+3))
                                 )
                               where
                                 rf' id = rf (Z:.id)

instance (Elt e) => MyStencil (Z:.Int) e (e, e, e, e, e, e, e, e, e) where
   stencilData rf arr (Z:.idx) = ( (rf' (idx-4))
                                 , (rf' (idx-3))
                                 , (rf' (idx-2))
                                 , (rf' (idx-1))
                                 , (rf' idx)
                                 , (rf' (idx+1))
                                 , (rf' (idx+2))
                                 , (rf' (idx+3))
                                 , (rf' (idx+4))
                                 )
                               where
                                 rf' id = rf (Z:.id)

data Boundary a = Clamp
                | Mirror
                | Wrap
                | Constant a

-- Convert to a list
-- Perform editing of values
-- Convert back to shape
bound :: (Shape sh, Elt a)
      => Array sh a
      -> Boundary a
      -> sh
      -> sh
      -> a
bound arr bndy sh ix =
   case bound' (listOfShape sh) (listOfShape ix) bndy of
      Left  val -> val
      Right sh  -> arr ! (shapeOfList sh)

bound' :: [Int] -> [Int] -> Boundary a -> Either a [Int]
bound' (sh:shs) (ix:ixs) bndy
   | ix < 0    = case bndy of
                  Clamp     -> bound' shs ixs bndy `addDim` 0
                  Mirror    -> bound' shs ixs bndy `addDim` (-1)
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
bound' [] [] _
   = Right []

main = do
   putStrLn $ show $ (aFunFloat . (stencilData (bound array Clamp (arrayExtent array)) array)) (Z:.(4::Int))

aFun :: (Int, Int, Int) -> Int
aFun (a,b,c) = a * b + c

aFunFloat :: (Float, Float, Float) -> Float
aFunFloat (a,b,c) = a + b + c

array :: Array DIM1 Float
array = fromFunction (Z:.(5 :: Int)) (\(Z:.ix) -> fromIntegral ix)

 -}
