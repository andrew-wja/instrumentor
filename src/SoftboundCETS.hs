module SoftboundCETS (instrument) where

import LLVM.AST
import LLVM.AST.Name
import LLVM.AST.Type
import LLVM.IRBuilder.Module
import Data.String (IsString(..))
{-
  We create a new module and place the declaration of the instrumentation
  interface API in it. This lets us just link this instrumentation module with
  the original module we were asked to transform and avoid all the nonsense of
  injecting function declarations one-by-one with getOrInsertFunction()
-}

createSBCETSModule :: ModuleBuilder ()
createSBCETSModule = do
  extern (mkName "__softboundcets_spatial_load_dereference_check")
         [ptr void, ptr void, ptr void, i64]
         void

  extern (mkName "__softboundcets_spatial_store_dereference_check")
         [ptr void, ptr void, ptr void, i64]
         void

  return ()

instrument :: Module -> IO (Module, Module)
instrument m = do
  let sbcModule = buildModule (fromString "softboundcets") createSBCETSModule
  return (sbcModule, m)
