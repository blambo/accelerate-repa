-- {-# LANGUAGE GADTs #-}
-- |
-- Module     : Data.Array.Accelerate.Repa
--
-- Maintainer : Ben Lambert-Smith <blambo+github@gmail.com>
--
-- This module implements the Repa back-end for the accelerate EDSL
-- The current structure follows closely on 
-- Data.Array.Accelerate.Interpreter

module Data.Array.Accelerate.Repa
   ( Arrays
   , run
   , compileToFile
   )
   where

import Data.Array.Accelerate.AST
import qualified Data.Array.Accelerate.Smart as Smart

import Data.Array.Accelerate.Repa.Evaluations (evalAcc)
import Data.Array.Accelerate.Repa.Stencil (stencilDoc)

import Text.PrettyPrint

import GHC       -- For compiling and running using GHC API
import GHC.Paths (libdir) -- simplifies use of GHC API
import System.IO -- For writing source to a file

-- | Used to compile and run an embedded array program using the Repa backend
run :: Arrays a => Smart.Acc a -> String
run acc = show $
   headS $$ (nest 1 (evalAcc $ Smart.convertAcc acc))
         $$ tailS
         $+$ text " "
         $$ stencilDoc

compileToFile :: Arrays a => Maybe String -> Smart.Acc a -> IO ()
compileToFile targetFile acc = do
   let f = case targetFile of
              Just s -> s
              Nothing -> defaultFile
   -- writes source to file as currently can't compile from String
   writeFile f $ run acc
   -- using GHC API from here
   r <- loadAndCompile f
   case r of
      Just err -> error err
      Nothing  -> return ()

loadAndCompile :: String -> IO (Maybe String)
loadAndCompile targetFile = runGhc (Just libdir) $ do
   dflags <- getSessionDynFlags
   setSessionDynFlags (dflags{
      optLevel = 2
      })
   target <- guessTarget targetFile Nothing
   addTarget target
   r <- load LoadAllTargets
   return $ case r of
      Failed    -> Just "Error in module loading"
      Succeeded -> Nothing

headS :: Doc
{-# INLINE headS #-}
headS =
   text "{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeOperators #-}" $+$
   text "{-# LANGUAGE FlexibleContexts #-}" $+$
   text "module" <+> text modName <+> text "where" $+$
   text "import Data.Array.Repa as Repa" $+$
   text "import Data.Bits -- required for Prim ops" $+$
   text "import Data.Char -- required for Prim ops" $+$
   text "import Data.Int  -- required for Prim ops" $+$
   text "import Data.List (sortBy)  -- required for permute" $+$
   text "import Data.Ord  (compare) -- required for permute" $+$
   text " " $+$
   text "main = putStrLn $ show $"

tailS :: Doc
{-# INLINE tailS #-}
tailS = empty

modName :: String
{-# INLINE modName #-}
modName = "Main"

defaultFile :: String
{-# INLINE defaultFile #-}
defaultFile = "AccRepa.hs"
