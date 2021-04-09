module Lib (readIR, printIR, writeIR, readBC, printBC, writeBC) where

import Prelude hiding (readFile, writeFile, print)
import qualified LLVM.AST as AST
import LLVM.Internal.Context (withContext)
import LLVM.Internal.Module (withModuleFromLLVMAssembly, moduleLLVMAssembly, withModuleFromBitcode, moduleBitcode, withModuleFromAST, moduleAST, Module(..), File(..))
import Data.ByteString (readFile, writeFile, ByteString)

readIR :: String -> IO AST.Module
readIR file = do
  fcts <- readFile file
  withContext (\ctx -> do
    withModuleFromLLVMAssembly ctx fcts toAST)

readBC :: String -> IO AST.Module
readBC file = do
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

writeIR :: String -> AST.Module -> IO ()
writeIR file m = do
  ir <- printIR m
  writeFile file ir

writeBC :: String -> AST.Module -> IO ()
writeBC file m = do
  ir <- printBC m
  writeFile file ir

toAST :: LLVM.Internal.Module.Module -> IO AST.Module
toAST = moduleAST
