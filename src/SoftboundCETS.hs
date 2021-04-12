module SoftboundCETS (instrument) where

import LLVM.AST
import LLVM.AST.Type
import LLVM.IRBuilder.Module
import Data.String (IsString(..))

{-
  We create a new module and place the declaration of the instrumentation
  interface API in it. This lets us just link this instrumentation module with
  the original module we were asked to transform and avoid all the nonsense of
  injecting function declarations one-by-one with getOrInsertFunction()
-}

sbcetsModule :: ModuleBuilder ()
sbcetsModule = do
  _ <- extern (mkName "__softboundcets_spatial_load_dereference_check")
         [ptr void, ptr void, ptr void, i64] void

  _ <- extern (mkName "__softboundcets_spatial_store_dereference_check")
         [ptr void, ptr void, ptr void, i64] void

  _ <- extern (mkName "__softboundcets_temporal_load_dereference_check")
         [ptr void, i64, ptr void, ptr void] void

  _ <- extern (mkName "__softboundcets_temporal_store_dereference_check")
         [ptr void, i64, ptr void, ptr void] void

  _ <- extern (mkName "__softboundcets_get_global_lock") [] (ptr void)

  _ <- extern (mkName "__softboundcets_introspect_metadata")
         [ptr void, ptr void, ptr void, i64] void

  _ <- extern (mkName "__softboundcets_copy_metadata")
         [ptr void, ptr void, i64] void

  _ <- extern (mkName "__softboundcets_allocate_shadow_stack_space")
         [i32] void

  _ <- extern (mkName "__softboundcets_deallocate_shadow_stack_space") [] void

  _ <- extern (mkName "__softboundcets_store_base_shadow_stack")
         [ptr void, i32] void

  _ <- extern (mkName "__softboundcets_store_bound_shadow_stack")
         [ptr void, i32] void

  _ <- extern (mkName "__softboundcets_load_base_shadow_stack")
         [i32] (ptr void)

  _ <- extern (mkName "__softboundcets_load_bound_shadow_stack")
         [i32] (ptr void)

  _ <- extern (mkName "__softboundcets_load_key_shadow_stack")
         [i32] i64

  _ <- extern (mkName "__softboundcets_load_lock_shadow_stack")
         [i32] (ptr void)

  _ <- extern (mkName "__softboundcets_store_key_shadow_stack")
         [i64, i32] void

  _ <- extern (mkName "__softboundcets_store_lock_shadow_stack")
         [ptr void, i32] void

  _ <- extern (mkName "__softboundcets_stack_memory_allocation")
         [ptr void, i64] void

  _ <- extern (mkName "__softboundcets_stack_memory_deallocation")
         [i64] void

  _ <- extern (mkName "__softboundcets_metadata_map")
         [ptr void] (ptr void)

  _ <- extern (mkName "__softboundcets_metadata_load_base")
         [ptr void] (ptr void)

  _ <- extern (mkName "__softboundcets_metadata_load_bound")
         [ptr void] (ptr void)

  _ <- extern (mkName "__softboundcets_metadata_load_key")
         [ptr void] i64

  _ <- extern (mkName "__softboundcets_metadata_load_lock")
         [ptr void] (ptr void)

  _ <- extern (mkName "__softboundcets_metadata_load_vector")
         [ptr void, ptr void, ptr void, i64, ptr void, i32] void

  _ <- extern (mkName "__softboundcets_metadata_store_vector")
         [ptr void, ptr void, ptr void, i64, ptr void, i32] void

  _ <- extern (mkName "__softboundcets_metadata_load")
         [ptr void, ptr void, ptr void, ptr i64, ptr void] void

  _ <- extern (mkName "__softboundcets_metadata_store")
         [ptr void, ptr void, ptr void, i64, ptr void] void

  _ <- extern (mkName "__softboundcets_spatial_call_dereference_check")
         [ptr void, ptr void, ptr void] void

  _ <- extern (mkName "__softboundcets_memcopy_check")
         [ptr void, ptr void, i64, ptr void, ptr void, ptr void, ptr void, i64,
          ptr void, i64, ptr void] void

  _ <- extern (mkName "__softboundcets_memset_check")
         [ptr void, i64, ptr void, ptr void, i64, ptr void] void

  return ()

instrument :: Module -> IO (Module, Module)
instrument m = do
  let sbcModule = buildModule (fromString "softboundcets") sbcetsModule
  return (sbcModule, m)
