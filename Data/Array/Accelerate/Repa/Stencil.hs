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
 $$ boundary
 $$ bound

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

boundary
 = text "data Boundary a" <+> (text "= Clamp"
                            $$ text "| Mirror"
                            $$ text "| Wrap"
                            $$ text "| Constant a")

rf :: String -> Doc
rf s = parens $ text "rf'" <+> parens (text s)

stenData :: String -> Doc
stenData s = text "stencilData rf arr" <+> parens (text s) <+> equals

bound
 = text "bound" <+> (text ":: (Shape sh, Elt a)"
                  $$ text "=> (sh -> a)"
                  $$ text "-> Boundary a"
                  $$ text "-> sh"
                  $$ text "-> sh"
                  $$ text "-> a")
  $$ text "bound lookup bndy sh ix ="
  $$ nest 1 (text "case bound' (listOfShape sh) (listOfShape ix) bndy of"
      $$ nest 1 (text "Left  val -> val"
              $$ text "Right sh  -> lookup (shapeOfList sh)"))
  $$ text "bound' :: [Int] -> [Int] -> Boundary a -> Either a [Int]"
  $$ text "bound' (sh:shs) (ix:ixs) bndy"
  $$ nest 1 (text "| ix < 0    ="
            <+> (text "case bndy of"
               $$ nest 1 (text "Clamp     -> bound' shs ixs bndy `addDim` 0"
                       $$ text "Mirror    -> bound' shs ixs bndy `addDim` (-ix)"
                       $$ text "Wrap      -> bound' shs ixs bndy `addDim` (sh+ix)"
                       $$ text "Constant e -> Left e"))
          $$ text "| ix >= sh  ="
            <+> (text "case bndy of"
               $$ nest 1 (text "Clamp     -> bound' shs ixs bndy `addDim` (sh-1)"
                       $$ text "Mirror    -> bound' shs ixs bndy `addDim` (sh-(ix-sh+2))"
                       $$ text "Wrap      -> bound' shs ixs bndy `addDim` (ix-sh)"
                       $$ text "Constant e -> Left e"))
          $$ text "| otherwise = bound' shs ixs bndy `addDim` ix"
          $$ text "where"
            $$ nest 1 (text "addDim (Right ds) d = Right (d:ds)"
                    $$ text "addDim (Left  e)  _ = Left  e"))
  $$ text "bound' [] [] _ = Right []"

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
