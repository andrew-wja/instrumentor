{-# LANGUAGE CPP #-}
module Utils (readIR, printIR, writeIR, readBC, printBC, writeBC, writeBC', toAST, fromAST, verifyAST, (!!)) where

import Prelude hiding ((!!), readFile, writeFile, print)
import qualified LLVM.AST as AST
import LLVM.Internal.Analysis (verify)
import LLVM.Internal.Context (withContext)
import LLVM.Internal.Module (withModuleFromLLVMAssembly, moduleLLVMAssembly, withModuleFromBitcode, moduleBitcode, withModuleFromAST, moduleAST, Module(..), File(..))
import Data.ByteString (readFile, writeFile, ByteString)
import Data.Map ((!), Map)

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

writeBC' :: String -> LLVM.Internal.Module.Module -> IO ()
writeBC' file m = do
  ir <- moduleBitcode m
  writeFile file ir

toAST :: LLVM.Internal.Module.Module -> IO AST.Module
toAST = moduleAST

fromAST :: AST.Module -> IO LLVM.Internal.Module.Module
fromAST m =
  withContext $ (\ctx -> do
    withModuleFromAST ctx m pure)

verifyAST :: AST.Module -> IO ()
verifyAST m = do
  m' <- fromAST m
  verify m'

infixl 9 !!

(!!) :: Ord k => Map k a -> k -> (k, a)
(!!) m k = (k, m ! k)
#if __GLASGOW_HASKELL__
{-# INLINE (!!) #-}
#endif
