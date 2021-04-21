{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module SoftboundCETS (instrument) where

import Prelude hiding ((!!))
import Control.Monad.State hiding (void)
import Control.Monad.RWS hiding (void)
import Data.Set hiding (map, filter, null)
import Data.Map hiding (map, filter, null)
import Data.Maybe (isJust, fromJust)
import Data.List (unzip4)
import Data.String (IsString(..))
import Data.Text.Lazy (unpack)
import LLVM.AST
import LLVM.AST.Global
import LLVM.AST.Type
import qualified LLVM.AST.Constant as Const
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Internal.SnocList
import LLVM.Pretty (ppll)
import Utils

data SBCETSState = SBCETSState { globalLockPtr :: Maybe Operand
                               , functionKey :: Maybe Operand
                               , functionLock :: Maybe Operand
                               , stackAllocations :: Set Name
                               , metadataTable :: Map Operand (Operand, Operand, Operand, Operand)
                               , instrumentationCandidates :: Set Name
                               , renamingCandidates :: Set Name
                               , wrapperFunctionPrototypes :: Map String Type
                               , runtimeFunctionPrototypes :: Map String Type
                               }

emptySBCETSState :: SBCETSState
emptySBCETSState = SBCETSState Nothing Nothing Nothing
                               Data.Set.empty Data.Map.empty
                               Data.Set.empty Data.Set.empty
                               Data.Map.empty Data.Map.empty

initSBCETSState :: SBCETSState
initSBCETSState = emptySBCETSState
  { wrapperFunctionPrototypes = Data.Map.fromList [
    ("softboundcets_malloc", FunctionType (ptr i8) [i64] False),
    ("softboundcets_free", FunctionType (void) [ptr i8] False)
    ]

  , runtimeFunctionPrototypes = Data.Map.fromList [
    ("__softboundcets_get_global_lock", FunctionType (ptr i8) [] False),
    ("__softboundcets_metadata_load", FunctionType void [ptr i8, (ptr $ ptr i8), (ptr $ ptr i8), ptr i64, (ptr $ ptr i8)] False),
    ("__softboundcets_metadata_store", FunctionType void [ptr i8, ptr i8, ptr i8, i64, ptr i8] False),
    ("__softboundcets_load_base_shadow_stack", FunctionType (ptr i8) [i32] False),
    ("__softboundcets_load_bound_shadow_stack", FunctionType (ptr i8) [i32] False),
    ("__softboundcets_load_key_shadow_stack", FunctionType (i64) [i32] False),
    ("__softboundcets_load_lock_shadow_stack", FunctionType (ptr i8) [i32] False),
    ("__softboundcets_store_base_shadow_stack", FunctionType void [ptr i8, i32] False),
    ("__softboundcets_store_bound_shadow_stack", FunctionType void [ptr i8, i32] False),
    ("__softboundcets_store_key_shadow_stack", FunctionType void [i64, i32] False),
    ("__softboundcets_store_lock_shadow_stack", FunctionType void [ptr i8, i32] False),
    ("__softboundcets_allocate_shadow_stack_space", FunctionType void [i32] False),
    ("__softboundcets_deallocate_shadow_stack_space", FunctionType void [] False),
    ("__softboundcets_spatial_load_dereference_check", FunctionType void [ptr i8, ptr i8, ptr i8, i64] False),
    ("__softboundcets_temporal_load_dereference_check", FunctionType void [ptr i8, i64, ptr i8, ptr i8] False),
    ("__softboundcets_spatial_store_dereference_check", FunctionType void [ptr i8, ptr i8, ptr i8, i64] False),
    ("__softboundcets_temporal_store_dereference_check", FunctionType void [ptr i8, i64, ptr i8, ptr i8] False),
    ("__softboundcets_create_stack_key", FunctionType void [(ptr $ ptr i8), ptr i64] False),
    ("__softboundcets_destroy_stack_key", FunctionType void [i64] False)
    ]
  }

ignoredFunctions :: Set Name
ignoredFunctions = Data.Set.fromList $ map mkName [
  "asprintf", "compare_pic_by_pic_num_desc", "dup2", "dup", "error", "execlp",
  "execl", "execv", "_exit", "fcntl", "fflush_unlocked", "flockfile", "fork",
  "__fpending", "fprintf", "fscanf", "full_write", "funlockfile",
  "fwrite_unlocked", "__hashProbeAddrOfPtr", "ioctl", "_IO_getc", "_IO_putc",
  "longjmp", "__memcopyCheck_i64", "__memcopyCheck", "__option_is_short",
  "__overflow", "pipe", "printf", "puts", "safe_calloc", "safe_free",
  "safe_malloc", "safe_mmap", "safe_read", "scanf", "select", "_setjmp",
  "setuid", "__shrinkBounds", "snprintf", "__softboundcets_abort",
  "__softboundcets_add_to_free_map", "__softboundcets_allocate_lock_location",
  "__softboundcets_allocate_shadow_stack_space",
  "__softboundcets_allocation_secondary_trie_allocate_range",
  "__softboundcets_allocation_secondary_trie_allocate",
  "__SOFTBOUNDCETS_ASSERT_fail", "SOFTBOUNDCETS_ASSERT",
  "__softboundcets_check_remove_from_free_map",
  "__softboundcets_copy_metadata",
  "__softboundcets_deallocate_shadow_stack_space", "__softboundcets_dummy",
  "__softboundcets_get_global_lock", "__softboundcets_global_init",
  "__softboundcets_init", "__softboundcets_intermediate",
  "__softboundcets_introspect_metadata",
  "__softboundcets_load_base_shadow_stack",
  "__softboundcets_load_bound_shadow_stack",
  "__softboundcets_load_key_shadow_stack",
  "__softboundcets_load_lock_shadow_stack", "__softboundcets_memcopy_check",
  "__softboundcets_memory_allocation", "__softboundcets_memory_deallocation",
  "__softboundcets_metadata_load", "__softboundcets_metadata_load_vector",
  "__softboundcets_metadata_store", "__softboundcets_metadata_store_vector",
  "__softboundcets_printf", "__softboundcets_print_metadata",
  "__softboundcets_spatial_call_dereference_check",
  "__softboundcets_spatial_load_dereference_check",
  "__softboundcets_spatial_store_dereference_check",
  "__softboundcets_create_stack_key",
  "__softboundcets_destroy_stack_key",
  "__softboundcets_store_base_shadow_stack",
  "__softboundcets_store_bound_shadow_stack",
  "__softboundcets_store_key_shadow_stack",
  "__softboundcets_store_lock_shadow_stack", "__softboundcets_stub",
  "__softboundcets_temporal_load_dereference_check",
  "__softboundcets_temporal_store_dereference_check",
  "__softboundcets_trie_allocate", "sprintf", "sscanf", "__strcspn_c2",
  "__stroul_internal", "__strspn_c2", "__strtod_internal", "__strtol_internal",
  "__strtoul_internal", "__uflow", "vasprintf", "vfprintf", "vsnprintf",
  "vsprintf", "waitpid", "wprintf" ]

wrappedFunctions :: Set Name
wrappedFunctionNames :: Map Name Name
(wrappedFunctions, wrappedFunctionNames) =
  let names = [ "abort", "abs", "acos", "atan2", "atexit", "atof", "atoi", "atol",
                "ceilf", "ceil", "chdir", "chown", "chroot", "clock",
                "closedir", "close", "cosf", "cosl", "cos", "ctime",
                "__ctype_b_loc", "__ctype_tolower_loc", "__ctype_toupper_loc",
                "difftime", "drand48", "__errno_location", "exit", "exp2",
                "expf", "exp", "fabsf", "fabs", "fclose", "fdopen", "feof",
                "ferror", "fflush", "fgetc", "fgets", "fileno", "floorf",
                "floor", "fopen", "fputc", "fputs", "fread", "fseek", "fstat",
                "ftell", "ftruncate", "fwrite", "getcwd", "getenv",
                "getrlimit", "gets", "gettimeofday", "getuid", "isatty",
                "ldexp", "localtime", "log10", "log", "lrand48", "lseek",
                "memchr", "memcmp", "mkdir", "mkstemp", "opendir", "open",
                "pclose", "perror", "popen", "pow", "putchar", "qsort", "rand",
                "readdir", "read", "remove", "rename", "rewind", "rindex",
                "rmdir", "select", "setbuf", "setreuid", "setrlimit", "signal",
                "sinf", "sinl", "sin", "sleep", "sqrtf", "sqrt", "srand48",
                "srand", "stat", "strcasecmp", "strcat", "strchr", "strcmp",
                "strcpy", "strcspn", "strdup", "strerror", "strftime",
                "strlen", "strncasecmp", "strncat", "strncmp", "strncpy",
                "strpbrk", "strrchr", "strspn", "strstr", "strtod", "strtok",
                "strtol", "strtoul", "system",
                "tanf", "tanl", "tan", "times", "time", "tmpfile", "tolower", "toupper",
                "umask", "unlink", "write", "calloc", "free", "main", "malloc", "mmap",
                "realloc" ]
  in (Data.Set.fromList $ map mkName names,
      Data.Map.fromList $ map (\n -> (mkName n, mkName ("softboundcets_" ++ n))) names)


instrument :: Module -> IO Module
instrument m = do
  let (warnings, instrumented) = instrumentDefinitions $ moduleDefinitions m
  mapM_ (putStrLn . ("instrumentor: "++)) warnings
  return $ m { moduleDefinitions = instrumented }
  where
    instrumentDefinitions :: [Definition] -> ([String], [Definition])
    instrumentDefinitions defs =
      let sbcetsState = initSBCETSState { instrumentationCandidates = functionsToInstrument defs
                                        , renamingCandidates = Data.Set.singleton $ mkName "main"
                                        }
          irBuilderState = emptyIRBuilder { builderNameSuggestion = Just $ fromString "sbcets" }
          modBuilderState = emptyModuleBuilder
          ((_, warnings), result) = runModuleBuilder modBuilderState $ do
                                      (\x -> evalRWST x () sbcetsState) $
                                        execIRBuilderT irBuilderState $ do
                                          mapM_ emitRuntimeAPIFunctionDecl $ assocs $ runtimeFunctionPrototypes sbcetsState
                                          mapM_ emitRuntimeAPIFunctionDecl $ assocs $ wrapperFunctionPrototypes sbcetsState
                                          mapM_ instrumentDefinition defs
                                          return ()
      in (warnings, result)

    functionsToInstrument :: [Definition] -> Set Name
    functionsToInstrument defs = Data.Set.difference (Data.Set.fromList $ map getFuncName
                                                                        $ filter isFuncDef
                                                                        $ defs)
                                                     (Data.Set.union ignoredFunctions
                                                                     wrappedFunctions)

    isFuncDef (GlobalDefinition (Function {})) = True
    isFuncDef _ = False

    getFuncName (GlobalDefinition f@(Function {})) = name f
    getFuncName _ = undefined

    emitRuntimeAPIFunctionDecl :: (String, Type) -> IRBuilderT (RWST () [String] SBCETSState ModuleBuilder) ()
    emitRuntimeAPIFunctionDecl (fname, (FunctionType retType argTypes _)) = do
      _ <- extern (mkName fname) argTypes retType
      return ()

    emitRuntimeAPIFunctionDecl (_, _) = undefined

    instrumentDefinition g@(GlobalDefinition f@(Function {}))
      -- Don't instrument empty functions
      | null $ basicBlocks f = emitDefn g
      -- We do not currently instrument varargs functions
      | snd $ parameters f = emitDefn g
      | otherwise = do
        shouldInstrument <- gets $ (Data.Set.member $ name f) . instrumentationCandidates
        shouldRename <- gets $ (Data.Set.member $ name f) . renamingCandidates
        if shouldInstrument || shouldRename then do
          instrumentFunction shouldRename f
        else emitDefn g

    instrumentDefinition x = emitDefn x

    bbName (BasicBlock n _ _) = n

    instrumentFunction shouldRename f@(Function {}) = do
      let name' = if shouldRename then wrappedFunctionNames ! (name f) else name f
      let firstBlockLabel = bbName $ head $ basicBlocks f
      (_, blocks) <- runIRBuilderT emptyIRBuilder { builderNameSuggestion = Just $ fromString "sbcets" } $ do
        instrumentPointerArgs firstBlockLabel $ fst $ parameters f
        instrumentBlocks $ basicBlocks f
        return ()
      let def = GlobalDefinition $ f { name = name'
                                     , basicBlocks = blocks
                                     }
      emitDefn def
      return ()

    instrumentFunction _ _ = undefined

    -- Set up the instrumentation of any pointer arguments to the function, and
    -- then branch unconditionally to the first block in the function body.

    instrumentPointerArgs fblabel pms = do
      let pointerArgs = filter isPointerArg pms
      let shadowStackIndices :: [Integer] = [1..]
      emitBlockStart (mkName "sbcets_parameter_metadata_init")
      zipWithM_ emitPointerParameterMetadataLoadFromShadowStack pointerArgs shadowStackIndices
      emitTerm $ Br fblabel []
      where
        isPointerArg (Parameter (PointerType _ _) _ _) = True
        isPointerArg _ = False

    -- Load the metadata for a pointer function parameter from the shadow stack.
    -- The location of that metadata in the shadow stack is given by the position
    -- of the pointer argument in the list of pointer arguments (starting from one).
    -- The zeroth shadow stack location is reserved for metadata about the return
    -- value, but unused if the return value is not a pointer

    emitPointerParameterMetadataLoadFromShadowStack (Parameter argType argName _) ix = do
      ix' <- pure $ int32 ix
      (baseName, baseProto) <- gets((!! "__softboundcets_load_base_shadow_stack") . runtimeFunctionPrototypes)
      base <- call (ConstantOperand $ Const.GlobalReference (ptr baseProto) $ mkName baseName) [(ix', [])]
      (boundName, boundProto) <- gets((!! "__softboundcets_load_bound_shadow_stack") . runtimeFunctionPrototypes)
      bound <- call (ConstantOperand $ Const.GlobalReference (ptr boundProto) $ mkName boundName) [(ix', [])]
      (keyName, keyProto) <- gets((!! "__softboundcets_load_key_shadow_stack") . runtimeFunctionPrototypes)
      key <- call (ConstantOperand $ Const.GlobalReference (ptr keyProto) $ mkName keyName) [(ix', [])]
      (lockName, lockProto) <- gets((!! "__softboundcets_load_lock_shadow_stack") . runtimeFunctionPrototypes)
      lock <- call (ConstantOperand $ Const.GlobalReference (ptr lockProto) $ mkName lockName) [(ix', [])]
      modify $ \s -> s { metadataTable = Data.Map.insert (LocalReference argType argName) (base, bound, key, lock) $ metadataTable s }

    instrumentBlocks [] = return ()

    -- The state of the metadata table is saved prior to instrumenting a function
    -- and restored immediately afterwards. We can't leak metadata about pointers
    -- inside a function to the global context, because those pointers might have
    -- name clashes with pointers in other functions. Saving and restoring the
    -- metadata table takes care of this nicely.

    instrumentBlocks (first:[]) = do
      saved <- get
      emitFirstBlock first
      put saved

    instrumentBlocks (first:blocks) = do
      saved <- get
      emitFirstBlock first
      mapM_ emitBlock blocks
      put saved

    -- The first thing we need to do in the main body of any function is to call
    -- __softboundcets_get_global_lock() to get a pointer to the lock for global
    -- variables. This allows us to detect use after free of globals.

    -- We record the local variable which contains the global lock pointer in
    -- the state variable globalLockPtr. Hereafter `gets globalLockPtr` will
    -- give us this variable as an Operand, so that we can pass it to things.

    emitFirstBlock (BasicBlock n i t) = do
      emitBlockStart n
      (fname, fproto) <- gets ((!! "__softboundcets_get_global_lock") . runtimeFunctionPrototypes)
      glp <- call (ConstantOperand $ Const.GlobalReference (ptr fproto) $ mkName fname) []
      modify $ \s -> s { globalLockPtr = Just glp }
      emitLocalKeyAndLockCreation
      mapM_ instrumentInst i
      instrumentTerm t

    -- Create a local key and lock for entities allocated on the stack inside this function

    emitLocalKeyAndLockCreation = do
      keyPtr <- alloca i64 Nothing 8
      lockPtr <- alloca (ptr i8) Nothing 8
      (fname, fproto) <- gets ((!! "__softboundcets_create_stack_key") . runtimeFunctionPrototypes)
      _ <- call (ConstantOperand $ Const.GlobalReference (ptr fproto) $ mkName fname)
                [(lockPtr, []), (keyPtr, [])]
      key <- load keyPtr 0
      lock <- load lockPtr 0
      modify $ \s -> s { functionKey = Just key, functionLock = Just lock }
      return ()

    -- Invalidate the local key; We do this just prior to returning from the function.
    -- Subsequent use of a leaked stack-allocated variable will trigger a runtime error.

    emitLocalKeyAndLockDestruction = do
      localKey <- gets functionKey
      (fname, fproto) <- gets ((!! "__softboundcets_destroy_stack_key") . runtimeFunctionPrototypes)
      _ <- call (ConstantOperand $ Const.GlobalReference (ptr fproto) $ mkName fname)
                [(fromJust localKey, [])]
      return ()

    emitBlock (BasicBlock n i t) = do
      emitBlockStart n
      mapM_ instrumentInst i
      instrumentTerm t

    -- Location 0 in the shadow stack is for metadata about the return value of
    -- a function, when that return value is a pointer. When it is not a pointer,
    -- location 0 in the shadow stack is unused.

    instrumentTerm i@(Do (Ret (Just op@(LocalReference (PointerType _ _) _)) _)) = do
      emitPointerMetadataStoreToShadowStack op 0
      emitLocalKeyAndLockDestruction
      emitNamedTerm i

    instrumentTerm i = do
      tell ["skipping: " ++ (unpack $ ppll i)]
      emitNamedTerm i

    -- If the pointer is in the metadataTable, emit the shadow stack initialization
    -- code for the pointer's base, bound, key, and lock, placing them in the
    -- shadow stack at the specified position.

    emitPointerMetadataStoreToShadowStack op@(LocalReference _ _) ix = do
      maybeBBKL <- gets (Data.Map.lookup op . metadataTable)
      case maybeBBKL of
        (Just (base, bound, key, lock)) -> do
          ix' <- pure $ int32 ix
          (baseName, baseProto) <- gets ((!! "__softboundcets_store_base_shadow_stack") .runtimeFunctionPrototypes)
          _ <- call (ConstantOperand $ Const.GlobalReference (ptr baseProto) $ mkName baseName)
                    [(base, []), (ix', [])]
          (boundName, boundProto) <- gets ((!! "__softboundcets_store_bound_shadow_stack") .runtimeFunctionPrototypes)
          _ <- call (ConstantOperand $ Const.GlobalReference (ptr boundProto) $ mkName boundName)
                    [(bound, []), (ix', [])]
          (keyName, keyProto) <- gets ((!! "__softboundcets_store_key_shadow_stack") .runtimeFunctionPrototypes)
          _ <- call (ConstantOperand $ Const.GlobalReference (ptr keyProto) $ mkName keyName)
                    [(key, []), (ix', [])]
          (lockName, lockProto) <- gets ((!! "__softboundcets_store_lock_shadow_stack") .runtimeFunctionPrototypes)
          _ <- call (ConstantOperand $ Const.GlobalReference (ptr lockProto) $ mkName lockName)
                    [(lock, []), (ix', [])]
          return ()
        Nothing -> do
          -- Pointer is not in the local symbol table; look it up in the runtime
          ix' <- pure $ int32 ix
          (base, bound, key, lock) <- getMetadataForPointer op
          base' <- load base 0
          bound' <- load bound 0
          key' <- load key 0
          lock' <- load lock 0
          (baseName, baseProto) <- gets ((!! "__softboundcets_store_base_shadow_stack") .runtimeFunctionPrototypes)
          _ <- call (ConstantOperand $ Const.GlobalReference (ptr baseProto) $ mkName baseName)
                    [(base', []), (ix', [])]
          (boundName, boundProto) <- gets ((!! "__softboundcets_store_bound_shadow_stack") .runtimeFunctionPrototypes)
          _ <- call (ConstantOperand $ Const.GlobalReference (ptr boundProto) $ mkName boundName)
                    [(bound', []), (ix', [])]
          (keyName, keyProto) <- gets ((!! "__softboundcets_store_key_shadow_stack") .runtimeFunctionPrototypes)
          _ <- call (ConstantOperand $ Const.GlobalReference (ptr keyProto) $ mkName keyName)
                    [(key', []), (ix', [])]
          (lockName, lockProto) <- gets ((!! "__softboundcets_store_lock_shadow_stack") .runtimeFunctionPrototypes)
          _ <- call (ConstantOperand $ Const.GlobalReference (ptr lockProto) $ mkName lockName)
                    [(lock', []), (ix', [])]
          return ()

    emitPointerMetadataStoreToShadowStack (ConstantOperand _) _ = undefined
    emitPointerMetadataStoreToShadowStack (MetadataOperand _) _ = undefined

    emitReturnedPointerMetadataLoadFromShadowStack (LocalReference argType argName) = do
      ix' <- pure $ int32 0
      (baseName, baseProto) <- gets((!! "__softboundcets_load_base_shadow_stack") . runtimeFunctionPrototypes)
      base <- call (ConstantOperand $ Const.GlobalReference (ptr baseProto) $ mkName baseName) [(ix', [])]
      (boundName, boundProto) <- gets((!! "__softboundcets_load_bound_shadow_stack") . runtimeFunctionPrototypes)
      bound <- call (ConstantOperand $ Const.GlobalReference (ptr boundProto) $ mkName boundName) [(ix', [])]
      (keyName, keyProto) <- gets((!! "__softboundcets_load_key_shadow_stack") . runtimeFunctionPrototypes)
      key <- call (ConstantOperand $ Const.GlobalReference (ptr keyProto) $ mkName keyName) [(ix', [])]
      (lockName, lockProto) <- gets((!! "__softboundcets_load_lock_shadow_stack") . runtimeFunctionPrototypes)
      lock <- call (ConstantOperand $ Const.GlobalReference (ptr lockProto) $ mkName lockName) [(ix', [])]
      modify $ \s -> s { metadataTable = Data.Map.insert (LocalReference argType argName) (base, bound, key, lock) $ metadataTable s }

    emitReturnedPointerMetadataLoadFromShadowStack (ConstantOperand _) = undefined
    emitReturnedPointerMetadataLoadFromShadowStack (MetadataOperand _) = undefined

    -- We should never see this happen -- "named terminator" is a quirk of the IR

    emitNamedTerm (_ := _) = undefined

    emitNamedTerm (Do t) = do
      modifyBlock $ \bb -> bb
        { partialBlockTerm = Just (Do t) }

    getMetadataForPointer addr@(LocalReference (PointerType _ _) _) = do
      isLocal <- gets ((Data.Map.member addr) . metadataTable)
      if isLocal then do
        gets ((! addr) . metadataTable)
      else do
        base <- alloca (ptr i8) Nothing 8
        bound <- alloca (ptr i8) Nothing 8
        key <- alloca (i64) Nothing 8
        lock <- alloca (ptr i8) Nothing 8
        addr' <- bitcast addr (ptr i8)
        -- Get the metadata for the pointer from the runtime by calling __softboundcets_metadata_load()
        (fname, fproto) <- gets ((!! "__softboundcets_metadata_load") . runtimeFunctionPrototypes)
        _ <- call (ConstantOperand $ Const.GlobalReference (ptr fproto) $ mkName fname)
                  [(addr', []), (base, []), (bound, []), (key, []), (lock, [])]
        modify $ \s -> s { metadataTable = Data.Map.insert addr (base, bound, key, lock) $ metadataTable s }
        return (base, bound, key, lock)

    getMetadataForPointer _ = undefined

    instrumentInst i@(v := o)
      -- Instrument alloca instructions
      | (Alloca {}) <- o = do
        modify $ \s -> s { stackAllocations = Data.Set.insert v $ stackAllocations s }
        emitNamedInst i

      -- Instrument a load instruction
      -- If we're loading a pointer from memory then we also need to load the
      -- metadata for that pointer from the runtime, and record the local variables
      -- holding that metadata in the symbol table. getMetadataForPointer does this
      -- so just call it and ignore the result.
      | (Load _ addr@(LocalReference (PointerType (PointerType _ _) _) _) _ _ _) <- o = do
        _ <- getMetadataForPointer addr
        emitNamedInst i

      -- Instrument a call instruction unless it is calling inline assembly
      | (Call _ _ _ (Right (ConstantOperand (Const.GlobalReference (PointerType (FunctionType rt _ False) _) fname))) opds _ _) <- o = do
        let ptrArgs = map fst $ filter (isPointerOperand . fst) opds
        -- allocate shadow stack space
        emitShadowStackAllocation (fromIntegral $ 1 + length ptrArgs)
        -- write the pointer metadata into the shadow stack
        zipWithM_ emitPointerMetadataStoreToShadowStack ptrArgs [1..]
        -- call the function or call the wrapper if there is one
        if Data.Set.member fname wrappedFunctions then
          emitNamedInst $ v := (rewriteCalledFunctionName (wrappedFunctionNames ! fname) o)
        else emitNamedInst i
        -- read the pointer metadata for the return value if it is a pointer
        when (isPointerType rt) $ emitReturnedPointerMetadataLoadFromShadowStack (LocalReference rt v)
        -- deallocate the shadow stack space
        emitShadowStackDeallocation

      -- Instrument a getelementptr instruction when the operand is not a
      -- constant or metadata reference. Just propagate the metadata for the
      -- source pointer through to the destination pointer.
      | (GetElementPtr _ addr@(LocalReference ty _) _ _) <- o = do
        maybeBBKL <- gets (Data.Map.lookup addr . metadataTable)
        case maybeBBKL of
          (Just (base, bound, key, lock)) -> do
            modify $ \s -> s { metadataTable = Data.Map.insert (LocalReference ty v) (base, bound, key, lock) $ metadataTable s }
            emitNamedInst i
          Nothing -> do
            emitNamedInst i

      | (BitCast addr@(LocalReference _ _) ty' _) <- o = do
        maybeBBKL <- gets (Data.Map.lookup addr . metadataTable)
        case maybeBBKL of
          (Just (base, bound, key, lock)) -> do
            modify $ \s -> s { metadataTable = Data.Map.insert (LocalReference ty' v) (base, bound, key, lock) $ metadataTable s }
            emitNamedInst i
          Nothing -> do
            emitNamedInst i

      -- If someone is using inttoptr then they forego any kind of protection
      | (IntToPtr _ ty _) <- o = do
        base <- inttoptr (int8 0) (ptr i8)
        bound <- inttoptr (int8 0) (ptr i8)
        key <- pure $ int64 0
        lock <- inttoptr (int8 0) (ptr i8)
        modify $ \s -> s { metadataTable = Data.Map.insert (LocalReference ty v) (base, bound, key, lock) $ metadataTable s }
        emitNamedInst i

      -- Instrument a select instruction if it is selecting between two pointers
      | (Select cond tval@(LocalReference ty@(PointerType {}) _)
                     fval@(LocalReference (PointerType {}) _) _) <- o = do
        maybeBBKLTrue <- gets (Data.Map.lookup tval . metadataTable)
        maybeBBKLFalse <- gets (Data.Map.lookup fval . metadataTable)

        case (maybeBBKLTrue, maybeBBKLFalse) of
          (Just (tbase, tbound, tkey, tlock), Just (fbase, fbound, fkey, flock)) -> do
            base <- select cond tbase fbase
            bound <- select cond tbound fbound
            key <- select cond tkey fkey
            lock <- select cond tlock flock
            modify $ \s -> s { metadataTable = Data.Map.insert (LocalReference ty v) (base, bound, key, lock) $ metadataTable s }
            emitNamedInst i
          _ -> emitNamedInst i

      -- Instrument a phi node if the incoming values are pointers
      | (Phi ty@(PointerType {}) incoming _) <- o = do
          maybeBBKLs <- mapM (\x -> gets ((Data.Map.lookup (fst x)) . metadataTable)) incoming
          if all isJust maybeBBKLs then do
            let (ibases, ibounds, ikeys, ilocks) = unzip4 $ map fromJust maybeBBKLs
            let preds = map snd incoming
            base <- phi $ zip ibases preds
            bound <- phi $ zip ibounds preds
            key <- phi $ zip ikeys preds
            lock <- phi $ zip ilocks preds
            modify $ \s -> s { metadataTable = Data.Map.insert (LocalReference ty v) (base, bound, key, lock) $ metadataTable s }
            emitNamedInst i
          else emitNamedInst i

      | otherwise = do
        tell ["skipping: " ++ (unpack $ ppll i)]
        emitNamedInst i

    instrumentInst i@(Do o)
      -- Instrument a call instruction unless it is calling inline assembly
      -- The return value is not captured, so don't emit checks for it
      | (Call _ _ _ (Right (ConstantOperand (Const.GlobalReference (PointerType (FunctionType _ _ False) _) fname))) opds _ _) <- o = do
        let ptrArgs = map fst $ filter (isPointerOperand . fst) opds
        -- allocate shadow stack space
        emitShadowStackAllocation (fromIntegral $ 1 + length ptrArgs)
        -- write the pointer metadata into the shadow stack
        zipWithM_ emitPointerMetadataStoreToShadowStack ptrArgs [1..]
        -- call the function or call the wrapper if there is one
        if Data.Set.member fname wrappedFunctions then
          emitNamedInst $ Do $ rewriteCalledFunctionName (wrappedFunctionNames ! fname) o
        else emitNamedInst i
        -- deallocate the shadow stack space
        emitShadowStackDeallocation

      -- Instrument a store instruction
      -- If we ever store a pointer to memory, we need to record the metadata
      -- for that pointer in the runtime, so that it can be looked up by whoever
      -- loads it back from memory later.
      | (Store _ addr@(LocalReference (PointerType (PointerType _ _) _) _) val@(LocalReference (PointerType _ _) _) _ _ _) <- o = do
        (base, bound, key, lock) <- getMetadataForPointer val
        addr' <- bitcast addr (ptr i8)
        (fname', fproto') <- gets ((!! "__softboundcets_metadata_store") . runtimeFunctionPrototypes)
        _ <- call (ConstantOperand $ Const.GlobalReference (ptr fproto') $ mkName fname')
                    [(addr', []), (base, []), (bound, []), (key, []), (lock, [])]
        emitNamedInst i

      | otherwise = do
        tell ["skipping: " ++ (unpack $ ppll i)]
        emitNamedInst i
{-
    getSizeOfType ty = do
      tyNullPtr <- inttoptr (int64 0) (ptr ty)
      tySzPtr <- gep tyNullPtr [int64 1]
      ptrtoint tySzPtr i64
-}
    isPointerOperand (LocalReference (PointerType _ _) _) = True
    isPointerOperand _ = False

    isPointerType (PointerType _ _) = True
    isPointerType _ = False

    rewriteCalledFunctionName n (Call tckind cconv retAttrs (Right (ConstantOperand (Const.GlobalReference fty _))) params attrs meta) =
      Call tckind cconv retAttrs (Right (ConstantOperand (Const.GlobalReference fty n))) params attrs meta

    rewriteCalledFunctionName _ _ = undefined

    emitShadowStackAllocation numArgs = do
      numArgs' <- pure $ int32 numArgs
      (fname, fproto) <- gets ((!! "__softboundcets_allocate_shadow_stack_space") . runtimeFunctionPrototypes)
      _ <- call (ConstantOperand $ Const.GlobalReference (ptr fproto) $ mkName fname)
                [(numArgs', [])]
      return ()

    emitShadowStackDeallocation = do
      (fname, fproto) <- gets ((!! "__softboundcets_deallocate_shadow_stack_space") . runtimeFunctionPrototypes)
      _ <- call (ConstantOperand $ Const.GlobalReference (ptr fproto) $ mkName fname) []
      return ()

    emitNamedInst (n := i) = do
      modifyBlock $ \bb -> bb
        { partialBlockInstrs = partialBlockInstrs bb `snoc` (n := i) }

    emitNamedInst (Do i) = do
      emitInstrVoid i
