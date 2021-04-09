module Lib (parseIR, printIR, parseBC, printBC) where

import Prelude hiding (readFile, print)
import qualified LLVM.AST as AST
import LLVM.Internal.Context (withContext)
import LLVM.Internal.Module (withModuleFromLLVMAssembly, moduleLLVMAssembly, withModuleFromBitcode, moduleBitcode, withModuleFromAST, moduleAST, Module(..), File(..))
import Data.ByteString (readFile, ByteString)

parseIR :: String -> IO AST.Module
parseIR file = do
  fcts <- readFile file
  withContext (\ctx -> do
    withModuleFromLLVMAssembly ctx fcts toAST)

parseBC :: String -> IO AST.Module
parseBC file = do
  withContext (\ctx -> do
    withModuleFromBitcode ctx (File file) toAST)

printIR :: AST.Module -> IO ByteString
printIR m = do
  withContext $ (\ctx -> do
    withModuleFromAST ctx m moduleLLVMAssembly)

printBC :: AST.Module -> IO ByteString
printBC m = do
  withContext $ (\ctx -> do
    withModuleFromAST ctx m moduleBitcode)

toAST :: LLVM.Internal.Module.Module -> IO AST.Module
toAST = moduleAST
