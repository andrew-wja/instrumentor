{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module SoftboundCETS (instrument) where

import Prelude hiding ((!!))
import Control.Monad.State hiding (void)
import Data.Set hiding (map, filter, null)
import Data.Map hiding (map, filter, null)
import Data.String (IsString(..))
import LLVM.AST
import LLVM.AST.Global
import LLVM.AST.Type
import qualified LLVM.AST.Constant as Const
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Internal.SnocList
import Utils
import LLVMHSExtensions

data SBCETSState = SBCETSState { globalLockPtr :: Maybe Operand
                               , metadataTable :: Map Operand (Operand, Operand, Operand, Operand)
                               , instrumentationCandidates :: Set Name
                               , renamingCandidates :: Set Name
                               , runtimeFunctionPrototypes :: Map String Type
                               }

emptySBCETSState :: SBCETSState
emptySBCETSState = SBCETSState Nothing Data.Map.empty Data.Set.empty Data.Set.empty $
  Data.Map.fromList [
  ("__softboundcets_get_global_lock", FunctionType (ptr i8) [] False),
  ("__softboundcets_metadata_load", FunctionType void [ptr i8, (ptr $ ptr i8), (ptr $ ptr i8), ptr i64, (ptr $ ptr i8)] False),
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
  ("__softboundcets_temporal_store_dereference_check", FunctionType void [ptr i8, i64, ptr i8, ptr i8] False)
  ]

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
  "__softboundcets_stack_memory_allocation",
  "__softboundcets_stack_memory_deallocation",
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
wrappedFunctions = Data.Set.fromList $ map mkName [
  "abort", "abs", "acos", "atan2", "atexit", "atof", "atoi", "atol",
  "ceilf", "ceil", "chdir", "chown", "chroot", "clock", "closedir", "close",
  "cosf", "cosl", "cos", "ctime", "__ctype_b_loc", "__ctype_tolower_loc",
  "__ctype_toupper_loc", "difftime", "drand48", "__errno_location", "exit",
  "exp2", "expf", "exp", "fabsf", "fabs", "fclose", "fdopen", "feof",
  "ferror", "fflush", "fgetc", "fgets", "fileno", "floorf", "floor",
  "fopen", "fputc", "fputs", "fread", "fseek", "fstat", "ftell",
  "ftruncate", "fwrite", "getcwd", "getenv", "getrlimit", "gets",
  "gettimeofday", "getuid", "isatty", "ldexp", "localtime", "log10", "log",
  "lrand48", "lseek", "memchr", "memcmp", "mkdir", "mkstemp", "opendir",
  "open", "pclose", "perror", "popen", "pow", "putchar", "qsort", "rand",
  "readdir", "read", "remove", "rename", "rewind", "rindex", "rmdir",
  "select", "setbuf", "setreuid", "setrlimit", "signal", "sinf", "sinl",
  "sin", "sleep", "sqrtf", "sqrt", "srand48", "srand", "stat", "strcasecmp",
  "strcat", "strchr", "strcmp", "strcpy", "strcspn", "strdup", "strerror",
  "strftime", "strlen", "strncasecmp", "strncat", "strncmp", "strncpy",
  "strpbrk", "strrchr", "strspn", "strstr", "strtod", "strtok", "strtol",
  "strtoul", "system", "tanf", "tanl", "tan", "times", "time", "tmpfile",
  "tolower", "toupper", "umask", "unlink", "write",
  "calloc", "free", "main", "malloc", "mmap", "realloc" ]

instrument :: Module -> IO Module
instrument m = do
  let instrumented = instrumentDefinitions $ moduleDefinitions m
  return $ m { moduleDefinitions = instrumented }
  where
    instrumentDefinitions :: [Definition] -> [Definition]
    instrumentDefinitions defs =
      let sbcetsState = emptySBCETSState { instrumentationCandidates = functionsToInstrument defs
                                         , renamingCandidates = functionsToWrap defs
                                         }
          irBuilderState = emptyIRBuilder { builderNameSuggestion = Just $ fromString "sbcets" }
          modBuilderState = emptyModuleBuilder
      in execModuleBuilder modBuilderState $
         flip evalStateT sbcetsState $
         execIRBuilderT irBuilderState $ do
           mapM_ emitRuntimeAPIFunctionDecl $ assocs $ runtimeFunctionPrototypes sbcetsState
           mapM_ instrumentDefinition defs
           return ()

    functionsToInstrument :: [Definition] -> Set Name
    functionsToInstrument defs = Data.Set.difference (Data.Set.fromList $ map getFuncName
                                                                        $ filter isFuncDef
                                                                        $ defs)
                                                     (Data.Set.union ignoredFunctions
                                                                     wrappedFunctions)

    functionsToWrap :: [Definition] -> Set Name
    functionsToWrap defs = Data.Set.intersection (Data.Set.fromList $ map getFuncName
                                                                  $ filter isFuncDef
                                                                  $ defs)
                                                 wrappedFunctions

    isFuncDef (GlobalDefinition (Function {})) = True
    isFuncDef _ = False

    getFuncName (GlobalDefinition f@(Function {})) = name f
    getFuncName _ = undefined

    emitRuntimeAPIFunctionDecl :: (String, Type) -> IRBuilderT (StateT SBCETSState ModuleBuilder) ()
    emitRuntimeAPIFunctionDecl (fname, (FunctionType retType argTypes _)) = do
      _ <- extern (mkName fname) argTypes retType
      return ()

    emitRuntimeAPIFunctionDecl (_, _) = undefined

    instrumentDefinition :: Definition -> IRBuilderT (StateT SBCETSState ModuleBuilder) ()
    instrumentDefinition g@(GlobalDefinition f@(Function {}))
      -- Don't instrument empty functions
      | null $ basicBlocks f = emitDefn g
      -- We do not currently instrument varargs functions
      | snd $ parameters f = emitDefn g
      | otherwise = do
        shouldInstrument <- gets $ (Data.Set.member $ name f) . instrumentationCandidates
        shouldRename <- gets $ (Data.Set.member $ name f) . renamingCandidates

        -- FIXME: how do we avoid remapping function parameter names here?
        if shouldInstrument then do
          let params = map (\(Parameter a b c) -> (a, name2ParamName b, c)) $ fst $ parameters f
          let instBody = instrumentFunctionBody f
          _ <- lift $ functionWithAttrs (name f) params (returnType f) instBody
          return ()

        else if shouldRename then do
          let params = map (\(Parameter a b c) -> (a, name2ParamName b, c)) $ fst $ parameters f
          let instBody = instrumentFunctionBody f
          let instName = mkName $ ("softboundcets_" ++) $ show $ name f
          _ <- lift $ functionWithAttrs instName params (returnType f) instBody
          return ()

        else emitDefn g

    instrumentDefinition x = emitDefn x

    name2ParamName (Name x) = ParameterName x
    name2ParamName (UnName _) = NoParameterName

    bbName (BasicBlock n _ _) = n

    instrumentFunctionBody :: Global -> [Operand] -> IRBuilderT (StateT SBCETSState ModuleBuilder) ()
    instrumentFunctionBody f@(Function {}) _ = do
      let firstBlockLabel = bbName $ head $ basicBlocks f
      instrumentPointerArgs firstBlockLabel $ fst $ parameters f
      instrumentBlocks $ basicBlocks f
      return ()

    instrumentFunctionBody _ _ = undefined

    -- Set up the instrumentation of any pointer arguments to the function, and
    -- then branch unconditionally to the first block in the function body.

    instrumentPointerArgs fblabel pms = do
      let pointerArgs = filter isPointerArg pms
      let shadowStackIndices :: [Integer] = [1..]
      emitBlockStart (mkName "sbcets_parameter_metadata_init")
      zipWithM_ emitPointerParameterMetadataLoad pointerArgs shadowStackIndices
      emitTerm $ Br fblabel []
      where
        isPointerArg (Parameter (PointerType _ _) _ _) = True
        isPointerArg _ = False

    -- Load the metadata for a pointer function parameter from the shadow stack.
    -- The location of that metadata in the shadow stack is given by the position
    -- of the pointer argument in the list of pointer arguments (starting from one).
    -- The zeroth shadow stack location is reserved for metadata about the return
    -- value, but unused if the return value is not a pointer

    emitPointerParameterMetadataLoad (Parameter argType argName _) ix = do
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

    -- The first thing we need to do in the main body of any function is to call
    -- __softboundcets_get_global_lock(), unless the function body is empty

    instrumentBlocks [] = return ()

    -- The state of the metadata table is saved prior to instrumenting a function
    -- and restored immediately afterwards. We can't leak metadata about pointers
    -- inside a function to the global context, because those pointers might have
    -- name clashes with pointers in other functions. Saving and restoring the
    -- metadata table takes care of this nicely.

    instrumentBlocks (first:[]) = do
      savedTable <- gets metadataTable
      emitFirstBlock first
      modify $ \s -> s { globalLockPtr = Nothing, metadataTable = savedTable }

    instrumentBlocks (first:blocks) = do
      savedTable <- gets metadataTable
      emitFirstBlock first
      mapM_ emitBlock blocks
      modify $ \s -> s { globalLockPtr = Nothing, metadataTable = savedTable }

    -- We record the local variable which contains the global lock pointer in
    -- the state variable globalLockPtr. Hereafter `gets globalLockPtr` will
    -- give us this variable as an Operand, so that we can pass it to things.
    -- We always unset the globalLockPtr at the end of each function (above),
    -- just so that something will break visibly if we ever miss initializing
    -- the local pointer to the global lock for any function.

    emitFirstBlock (BasicBlock n i t) = do
      emitBlockStart n
      (fname, fproto) <- gets ((!! "__softboundcets_get_global_lock") . runtimeFunctionPrototypes)
      glp <- call (ConstantOperand $ Const.GlobalReference (ptr fproto) $ mkName fname) []
      modify $ \s -> s { globalLockPtr = Just glp }
      mapM_ instrumentInst i
      instrumentTerm t

    emitBlock (BasicBlock n i t) = do
      emitBlockStart n
      mapM_ instrumentInst i
      instrumentTerm t

    -- Location 0 in the shadow stack is for metadata about the return value of
    -- a function, when that return value is a pointer. When it is not a pointer,
    -- location 0 in the shadow stack is unused.

    instrumentTerm x@(Do (Ret (Just op@(LocalReference (PointerType _ _) _)) _)) = do
      emitPointerOperandMetadataStore op 0
      emitNamedTerm x

    instrumentTerm x = emitNamedTerm x

    -- If the pointer is in the metadataTable, emit the shadow stack initialization
    -- code for the pointer's base, bound, key, and lock, placing them in the
    -- shadow stack at the specified position.

    emitPointerOperandMetadataStore op@(LocalReference _ _) ix = do
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
          -- use default metadata because the pointer is not tracked
          ix' <- pure $ int32 ix
          base <- inttoptr (int8 0) (ptr i8)
          (baseName, baseProto) <- gets ((!! "__softboundcets_store_base_shadow_stack") .runtimeFunctionPrototypes)
          _ <- call (ConstantOperand $ Const.GlobalReference (ptr baseProto) $ mkName baseName)
                    [(base, []), (ix', [])]
          bound <- inttoptr (int8 0) (ptr i8)
          (boundName, boundProto) <- gets ((!! "__softboundcets_store_bound_shadow_stack") .runtimeFunctionPrototypes)
          _ <- call (ConstantOperand $ Const.GlobalReference (ptr boundProto) $ mkName boundName)
                    [(bound, []), (ix', [])]
          key <- pure $ int64 0
          (keyName, keyProto) <- gets ((!! "__softboundcets_store_key_shadow_stack") .runtimeFunctionPrototypes)
          _ <- call (ConstantOperand $ Const.GlobalReference (ptr keyProto) $ mkName keyName)
                    [(key, []), (ix', [])]
          lock <- inttoptr (int8 0) (ptr i8)
          (lockName, lockProto) <- gets ((!! "__softboundcets_store_lock_shadow_stack") .runtimeFunctionPrototypes)
          _ <- call (ConstantOperand $ Const.GlobalReference (ptr lockProto) $ mkName lockName)
                    [(lock, []), (ix', [])]
          return ()

    emitPointerOperandMetadataStore (ConstantOperand _) _ = undefined
    emitPointerOperandMetadataStore (MetadataOperand _) _ = undefined

    emitPointerOperandMetadataLoad (LocalReference argType argName) ix = do
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

    emitPointerOperandMetadataLoad (ConstantOperand _) _ = undefined
    emitPointerOperandMetadataLoad (MetadataOperand _) _ = undefined

    -- We should never see this happen -- "named terminator" is a quirk of the IR

    emitNamedTerm (_ := _) = undefined

    emitNamedTerm (Do t) = do
      modifyBlock $ \bb -> bb
        { partialBlockTerm = Just (Do t) }

    instrumentInst i@(v := o)
      -- Instrument a load instruction if it is loading a pointer.
      | (Load _ addr@(LocalReference (PointerType (PointerType ty _) _) _) _ _ _) <- o = do
        base <- alloca (ptr i8) Nothing 8
        bound <- alloca (ptr i8) Nothing 8
        key <- alloca (i64) Nothing 8
        lock <- alloca (ptr i8) Nothing 8
        addr' <- bitcast addr (ptr i8)
        -- Get the metadata for the load address by calling __softboundcets_metadata_load()
        (fname, fproto) <- gets ((!! "__softboundcets_metadata_load") . runtimeFunctionPrototypes)
        _ <- call (ConstantOperand $ Const.GlobalReference (ptr fproto) $ mkName fname)
                  [(addr', []), (base, []), (bound, []), (key, []), (lock, [])]
        modify $ \s -> s { metadataTable = Data.Map.insert (LocalReference ty v) (base, bound, key, lock) $ metadataTable s }
        -- Check that the load is not a spatial memory violation
        (fname', fproto') <- gets ((!! "__softboundcets_spatial_load_dereference_check") . runtimeFunctionPrototypes)
        baseCast <- bitcast base (ptr i8)
        boundCast <- bitcast bound (ptr i8)
        tsize <- getSizeOfType ty
        _ <- call (ConstantOperand $ Const.GlobalReference (ptr fproto') $ mkName fname')
                  [(baseCast, []), (boundCast, []), (addr', []), (tsize, [])]
        -- Check that the load is not a temporal violation
        (fname'', fproto'') <- gets ((!! "__softboundcets_temporal_load_dereference_check") . runtimeFunctionPrototypes)
        lockCast <- bitcast lock (ptr i8)
        keyValue <- load key 0
        _ <- call (ConstantOperand $ Const.GlobalReference (ptr fproto'') $ mkName fname'')
                  [(lockCast, []), (keyValue, []), (baseCast, []), (boundCast, [])]
        -- Emit the load
        emitNamedInst i

      -- Instrument a call instruction unless it is calling inline assembly
      | (Call _ _ _ (Right (ConstantOperand (Const.GlobalReference (FunctionType rt _ False) _))) opds _ _) <- o = do
        let ptrArgs = map fst $ filter (isPointerOperand . fst) opds
        -- allocate shadow stack space
        emitShadowStackAllocation (fromIntegral $ 1 + length ptrArgs)
        -- write the pointer metadata into the shadow stack
        zipWithM_ emitPointerOperandMetadataStore ptrArgs [1..]
        -- call the function
        emitNamedInst i
        -- read the pointer metadata for the return value if it is a pointer
        when (isPointerType rt) $ emitPointerOperandMetadataLoad (LocalReference rt v) 0
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

      | otherwise = emitNamedInst i

    instrumentInst i@(Do o)
      -- Instrument a call instruction unless it is calling inline assembly
      -- The return value is not captured, so don't emit checks for it
      | (Call _ _ _ (Right (ConstantOperand (Const.GlobalReference (FunctionType _ _ False) _))) opds _ _) <- o = do
        let ptrArgs = map fst $ filter (isPointerOperand . fst) opds
        -- allocate shadow stack space
        emitShadowStackAllocation (fromIntegral $ 1 + length ptrArgs)
        -- write the pointer metadata into the shadow stack
        zipWithM_ emitPointerOperandMetadataStore ptrArgs [1..]
        -- call the function
        emitNamedInst i
        -- deallocate the shadow stack space
        emitShadowStackDeallocation

      -- Instrument a store instruction always
      | (Store _ addr@(LocalReference (PointerType ty _) _) _ _ _ _) <- o = do
        base <- alloca (ptr i8) Nothing 8
        bound <- alloca (ptr i8) Nothing 8
        key <- alloca (i64) Nothing 8
        lock <- alloca (ptr i8) Nothing 8
        addr' <- bitcast addr (ptr i8)
        -- Get the metadata for the store address by calling __softboundcets_metadata_load()
        (fname, fproto) <- gets ((!! "__softboundcets_metadata_load") . runtimeFunctionPrototypes)
        _ <- call (ConstantOperand $ Const.GlobalReference (ptr fproto) $ mkName fname)
                  [(addr', []), (base, []), (bound, []), (key, []), (lock, [])]
        -- Check that the store is not a spatial memory violation
        (fname', fproto') <- gets ((!! "__softboundcets_spatial_store_dereference_check") . runtimeFunctionPrototypes)
        baseCast <- bitcast base (ptr i8)
        boundCast <- bitcast bound (ptr i8)
        tsize <- getSizeOfType ty
        _ <- call (ConstantOperand $ Const.GlobalReference (ptr fproto') $ mkName fname')
                  [(baseCast, []), (boundCast, []), (addr', []), (tsize, [])]
        -- Check that the store is not a temporal violation
        (fname'', fproto'') <- gets ((!! "__softboundcets_temporal_store_dereference_check") . runtimeFunctionPrototypes)
        lockCast <- bitcast lock (ptr i8)
        keyValue <- load key 0
        _ <- call (ConstantOperand $ Const.GlobalReference (ptr fproto'') $ mkName fname'')
                  [(lockCast, []), (keyValue, []), (baseCast, []), (boundCast, [])]
        return ()
        -- Emit the store
        emitNamedInst i

      | otherwise = emitNamedInst i

    getSizeOfType ty = do
      tyNullPtr <- inttoptr (int64 0) (ptr ty)
      -- ~ tySzPtr <- emitInstr (ptr ty) $ GetElementPtr False tyNullPtr [int64 1] []
      tySzPtr <- gep tyNullPtr [int64 1]
      ptrtoint tySzPtr i64

    isPointerOperand (LocalReference (PointerType _ _) _) = True
    isPointerOperand _ = False

    isPointerType (PointerType _ _) = True
    isPointerType _ = False

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
