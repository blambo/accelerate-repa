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
   , accToRepa
   , compile
   , exec
   , run
   )
   where

import Data.Array.Accelerate
import qualified Data.Array.Accelerate.Smart as Smart
import Data.Array.Accelerate.Repa.Evaluations (evalAcc)
import Data.Array.Accelerate.Repa.Stencil (stencilDoc)

import Text.PrettyPrint

import qualified Data.Array.Repa as Repa

import GHC
import GHC.Paths (libdir)
import DynFlags

import Unsafe.Coerce
import System.IO
import System.Directory (removeFile)

-- | Using the provided Accelerate program, run will compile, execute
-- and return the result of a given Accelerate program using Repa for
-- execution
--
-- TODO: Remove Repa.Elt type class
-- TODO: Update return type for new Array type
run :: (Arrays a, Repa.Shape sh, Repa.Elt e)
    => Smart.Acc a -- ^ The Accelerate program
    -> IO (Repa.Array sh e)
run acc = do
   -- Generate source code from Accelerate program
   let src = accToRepa acc
   -- Write source code to temporary file in /tmp
   (name, handle) <- openTempFile tempDir fileName
   hPutStr handle src
   hClose handle

   -- Perform GHC API operations
   result <- runGhc (Just libdir) $ do
      -- compile
      err <- compile name
      -- execute compiled, checking for error
      case err of
         Just errStr -> error errStr
         Nothing     -> exec modName fnName

   -- Delete temporary file
   removeFile name

   -- Return result of Accelerate program
   return result

-- | Compiles the given file name
compile :: String -- ^ The source file
        -> Ghc (Maybe String)
compile path = do
   dflags <- getSessionDynFlags
   setSessionDynFlags (dflags{
      optLevel = 2,
      ghcLink = LinkInMemory,
      hscTarget = HscInterpreted
      })
   target <- guessTarget path Nothing
   addTarget target
   r <- load LoadAllTargets
   return $ case r of
      Failed    -> Just "Error in module loading"
      Succeeded -> Nothing

-- | Executes the given function in the given module, must already be
-- compiled and loaded
--
-- TODO: Change return type for new version of Repa.Array
-- TODO: Remove Repa.Elt type class reference
exec :: (Repa.Shape sh, Repa.Elt e)
     => String -- ^ The module name
     -> String -- ^ The function name
     -> Ghc (Repa.Array sh e)
exec modName fnName = do
   mod <- findModule (mkModuleName modName) Nothing
   setContext [mod] []
   value <- compileExpr (modName Prelude.++ "." Prelude.++ fnName)

   let value' = (unsafeCoerce value) :: Repa.Array sh e
   return value'

-- | Converts an Accelerate program to a Repa program and returns the
-- source as a String
-- 
-- TODO: Add module for 'Arrays' reference to make reading easier
accToRepa :: (Arrays a)
          => Smart.Acc a -- ^ The Accelerate program
          -> String
accToRepa acc = show $
   headD $$ (nest 1 (evalAcc (Smart.convertAcc acc)))
         $$ tailD
         $$ stencilDoc

headD :: Doc
{-# INLINE headD #-}
headD =
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
   text "main = putStrLn $ show $" <+> text fnName $+$
   text fnName <+> equals

tailD :: Doc
{-# INLINE tailD #-}
tailD = empty

modName :: String
{-# INLINE modName #-}
modName = "RepaTest"

fnName :: String
{-# INLINE fnName #-}
fnName = "repa"

fileName :: String
{-# INLINE fileName #-}
fileName = modName Prelude.++ ".hs"

tempDir :: FilePath
{-# INLINE tempDir #-}
tempDir = "/tmp/"
