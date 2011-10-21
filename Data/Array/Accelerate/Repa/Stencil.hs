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
 $$ nestedStencils
 $$ boundary
 $$ bound

classDefDoc
 =  text "class (Elt e, Shape sh) => MyStencil sh e tup where"
 $$ nest 1 (text "stencilData :: (sh -> e) -> sh -> tup")

baseStencils
 =  text "instance (Elt e) => MyStencil (Z:.Int) e (e, e, e) where"
 $$ nest 1 ((stenData "Z:.idx")
      <+> (lparen <+> rf "idx-1"
        $$ comma  <+> rf "idx"
        $$ comma  <+> rf "idx+1"
        $$ rparen
      $$ text "where"
      $$ nest 1 (text "rf' id = rf (Z:.id)")))
 $$ text "instance (Elt e) => MyStencil (Z:.Int) e (e, e, e, e, e) where"
 $$ nest 1 ((stenData "Z:.idx")
      <+> (lparen <+> rf "idx-2"
        $$ comma  <+> rf "idx-1"
        $$ comma  <+> rf "idx"
        $$ comma  <+> rf "idx+1"
        $$ comma  <+> rf "idx+2"
        $$ rparen
      $$ text "where"
      $$ nest 1 (text "rf' id = rf (Z:.id)")))
 $$ text "instance (Elt e) => MyStencil (Z:.Int) e (e, e, e, e, e, e, e) where"
 $$ nest 1 ((stenData "Z:.idx")
      <+> (lparen <+> rf "idx-3"
        $$ comma  <+> rf "idx-2"
        $$ comma  <+> rf "idx-1"
        $$ comma  <+> rf "idx"
        $$ comma  <+> rf "idx+1"
        $$ comma  <+> rf "idx+2"
        $$ comma  <+> rf "idx+3"
        $$ rparen
      $$ text "where"
      $$ nest 1 (text "rf' id = rf (Z:.id)"))) 
 $$ text "instance (Elt e) => MyStencil (Z:.Int) e (e, e, e, e, e, e, e, e, e) where"
 $$ nest 1 ((stenData "Z:.idx")
      <+> (lparen <+> rf "idx-4"
        $$ comma  <+> rf "idx-3"
        $$ comma  <+> rf "idx-2"
        $$ comma  <+> rf "idx-1"
        $$ comma  <+> rf "idx"
        $$ comma  <+> rf "idx+1"
        $$ comma  <+> rf "idx+2"
        $$ comma  <+> rf "idx+3"
        $$ comma  <+> rf "idx+4"
        $$ rparen
      $$ text "where"
      $$ nest 1 (text "rf' id = rf (Z:.id)"))) 

nestedStencils
 = text "instance" <+> parens (nestStenClass 3)
                   <+> text "=> MyStencil (sh:.Int:.Int) a (row1, row2, row3) where"
 $$ nest 1 ((stenData "ix:.i")
      <+> (lparen <+> text "stencilData" <+> rf "i-1" <+> text "ix"
        $$ comma  <+> text "stencilData" <+> rf "i"   <+> text "ix"
        $$ comma  <+> text "stencilData" <+> rf "i+1" <+> text "ix"
        $$ rparen
      $$ text "where"
      $$ nest 1 (text "rf' d ds = rf (ds :. d)")))

boundary
 = text "data Boundary a" <+> (text "= Clamp"
                            $$ text "| Mirror"
                            $$ text "| Wrap"
                            $$ text "| Constant a")


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

----------------------
-- HELPER FUNCTIONS --
----------------------

rf :: String -> Doc
rf s = parens $ text "rf'" <+> parens (text s)

stenData :: String -> Doc
stenData s = text "stencilData rf" <+> parens (text s) <+> equals

nestStenClass :: Int -> Doc
nestStenClass 1 = text "MyStencil (sh:.Int) a row1"
nestStenClass i =  nestStenClass (i-1) <> comma
                $$ text "MyStencil (sh:.Int) a row" <> int i
