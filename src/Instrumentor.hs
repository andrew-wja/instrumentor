{-# LANGUAGE CPP #-}

{-|
Module      : Util
Description : Miscellaneous helper functions
Copyright   : (c) Andrew Anderson, 2021
License     : BSD-3
Maintainer  : aanderso@tcd.ie
Stability   : experimental
-}

module Instrumentor (readIR, printIR, writeIR,
                     readBC, printBC, writeBC,
                     toAST, fromAST, verifyAST,
                     (!!),
                     InstrumentorPass, InstrumentorPassStep, runInstrumentorPass) where

import Prelude hiding ((!!), readFile, writeFile, print)
import qualified LLVM.AST as AST
import LLVM.Internal.Analysis (verify)
import LLVM.Internal.Context (withContext)
import LLVM.Internal.Module (withModuleFromLLVMAssembly, moduleLLVMAssembly,
                             withModuleFromBitcode, moduleBitcode,
                             withModuleFromAST, moduleAST,
                             Module(..), File(..))
import LLVM.IRBuilder.Monad as LLVM
import LLVM.IRBuilder.Module as LLVM
import Data.ByteString (readFile, writeFile, ByteString)
import Data.Map ((!), Map)
import Control.Monad.RWS hiding (void)

-- | Read a module from a .ll file (LLVM assembly)
readIR :: String -> IO AST.Module
readIR file = do
  fcts <- readFile file
  withContext (\ctx -> do
    withModuleFromLLVMAssembly ctx fcts toAST)

-- | Read a module from a .bc file (LLVM bitcode)
readBC :: String -> IO AST.Module
readBC file = do
  withContext (\ctx -> do
    withModuleFromBitcode ctx (File file) toAST)

-- | Print a module to a 'ByteString' (LLVM assembly)
printIR :: AST.Module -> IO ByteString
printIR m = do
  withContext $ (\ctx -> do
    withModuleFromAST ctx m moduleLLVMAssembly)

-- | Print a module to a 'ByteString' (LLVM bitcode)
printBC :: AST.Module -> IO ByteString
printBC m = do
  withContext $ (\ctx -> do
    withModuleFromAST ctx m moduleBitcode)

-- | Write a module to disk (LLVM assembly)
writeIR :: String -> AST.Module -> IO ()
writeIR file m = do
  ir <- printIR m
  writeFile file ir

-- | Write a module to disk (LLVM bitcode)
writeBC :: String -> AST.Module -> IO ()
writeBC file m = do
  ir <- printBC m
  writeFile file ir

-- | Alias for 'moduleAST'. Convert a C++ module to an AST module (Haskell)
toAST :: LLVM.Internal.Module.Module -> IO AST.Module
toAST = moduleAST

-- | Convert an AST module (Haskell) to a C++ module
fromAST :: AST.Module -> IO LLVM.Internal.Module.Module
fromAST m =
  withContext $ (\ctx -> do
    withModuleFromAST ctx m pure)

-- | Ask the LLVM verifier to check the module
verifyAST :: AST.Module -> IO ()
verifyAST m = do
  m' <- fromAST m
  verify m'

infixl 9 !!
-- | Pair the key with the associated value in the map
(!!) :: Ord k => Map k a -> k -> (k, a)
(!!) m k = (k, m ! k)
#if __GLASGOW_HASKELL__
{-# INLINE (!!) #-}
#endif

-- | The Instrumentor monad transformer stack
type InstrumentorPassStep ipInput ipOutput ipState ipResult = LLVM.IRBuilderT (RWST ipInput ipOutput ipState LLVM.ModuleBuilder) ipResult

-- | The type of an Instrumentor pass
type InstrumentorPass ipInput ipOutput ipState ipResult = AST.Module -> InstrumentorPassStep ipInput ipOutput ipState ipResult

-- | Run an Instrumentor pass given an input LLVM module and an initial state. Produces the instrumented module, result, and output from the pass.
runInstrumentorPass :: Monoid o => (InstrumentorPass i o s r) -> s -> i -> AST.Module -> IO ((AST.Module, r), o)
runInstrumentorPass p s i m = do
  let irBuilderState = LLVM.emptyIRBuilder
  let modBuilderState = LLVM.emptyModuleBuilder
  let (((result, _), output), defs) = LLVM.runModuleBuilder modBuilderState $ do
                              (\x -> evalRWST x i s) $
                                runIRBuilderT irBuilderState $ p m
  return ((m { AST.moduleDefinitions = defs }, result), output)
