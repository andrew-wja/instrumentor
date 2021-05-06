{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module SoftboundCETS (instrument) where

import Prelude hiding ((!!))
import Control.Monad.State hiding (void)
import Control.Monad.RWS hiding (void)
import qualified Data.Set
import Data.Map hiding (map, filter, null, foldr, drop)
import Data.Maybe (isJust, fromJust)
import Data.String (IsString(..))
import Data.List (isInfixOf)
import LLVM.AST
import LLVM.AST.Global
import LLVM.AST.Type
import LLVM.AST.Typed (typeOf)
import qualified LLVM.AST.Constant as Const
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Internal.SnocList
import Utils

data SBCETSState = SBCETSState { globalLockPtr :: Maybe Operand
                               , localStackFrameKeyPtr :: Maybe Operand
                               , localStackFrameLockPtr :: Maybe Operand
                               , instrumentationCandidates :: Data.Set.Set Name
                               , renamingCandidates :: Data.Set.Set Name
                               , wrapperFunctionPrototypes :: Map String Type
                               , runtimeFunctionPrototypes :: Map String Type

                               -- metadataTable must be saved and restored around basic block entry and exit,
                               -- otherwise we will leak metadata identifiers and potentially violate SSA form.
                               , metadataTable :: Map Operand (Operand, Operand, Operand, Operand)
                               }

emptySBCETSState :: SBCETSState
emptySBCETSState = SBCETSState Nothing Nothing Nothing
                               Data.Set.empty Data.Set.empty
                               Data.Map.empty Data.Map.empty Data.Map.empty

initSBCETSState :: SBCETSState
initSBCETSState = emptySBCETSState
  { wrapperFunctionPrototypes = Data.Map.fromList [
    ("softboundcets_calloc", FunctionType (ptr i8) [i64, i64] False),
    ("softboundcets_malloc", FunctionType (ptr i8) [i64] False),
    ("softboundcets_realloc", FunctionType (ptr i8) [ptr i8, i64] False),
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
    ("__softboundcets_temporal_load_dereference_check", FunctionType void [ptr i8, i64] False),
    ("__softboundcets_spatial_store_dereference_check", FunctionType void [ptr i8, ptr i8, ptr i8, i64] False),
    ("__softboundcets_temporal_store_dereference_check", FunctionType void [ptr i8, i64] False),
    ("__softboundcets_create_stack_key", FunctionType void [(ptr $ ptr i8), ptr i64] False),
    ("__softboundcets_destroy_stack_key", FunctionType void [i64] False)
    ]
  }

ignoredFunctions :: Data.Set.Set Name
ignoredFunctions = Data.Set.fromList $ map mkName [
  "asprintf", "compare_pic_by_pic_num_desc", "dup2", "dup", "error", "execlp",
  "execl", "execv", "_exit", "fcntl", "fflush_unlocked", "flockfile", "fork",
  "__fpending", "fprintf", "fscanf", "full_write", "funlockfile",
  "fwrite_unlocked", "__hashProbeAddrOfPtr", "ioctl", "_IO_getc", "_IO_putc",
  "longjmp", "__memcopyCheck_i64", "__memcopyCheck", "__option_is_short",
  "__overflow", "pipe", "printf", "puts", "safe_calloc", "safe_free",
  "safe_malloc", "safe_mmap", "safe_read", "scanf", "select", "_setjmp",
  "setuid", "__shrinkBounds", "snprintf", "sprintf", "sscanf", "__strcspn_c2",
  "__strdup", "__stroul_internal", "__strspn_c2", "__strtod_internal",
  "__strtol_internal", "__strtoul_internal", "__uflow", "vasprintf",
  "vfprintf", "vsnprintf", "vsprintf", "waitpid", "wprintf" ]

wrappedFunctions :: Data.Set.Set Name
wrappedFunctionNames :: Map Name Name
(wrappedFunctions, wrappedFunctionNames) =
  let names = [ --"abort", "abs", "acos", "atan2", "atexit", "atof", "atoi", "atol",
                --"ceilf", "ceil", "chdir", "chown", "chroot", "clock",
                --"closedir", "close", "cosf", "cosl", "cos", "ctime",
                --"__ctype_b_loc", "__ctype_tolower_loc", "__ctype_toupper_loc",
                --"difftime", "drand48", "__errno_location", "exit", "exp2",
                --"expf", "exp", "fabsf", "fabs", "fclose", "fdopen", "feof",
                --"ferror", "fflush", "fgetc", "fgets", "fileno", "floorf",
                --"floor", "fopen", "fputc", "fputs", "fread", "fseek", "fstat",
                --"ftell", "ftruncate", "fwrite", "getcwd", "getenv",
                --"getrlimit", "gets", "gettimeofday", "getuid", "isatty",
                --"ldexp", "localtime", "log10", "log", "lrand48", "lseek",
                --"memchr", "memcmp", "mkdir", "mkstemp", "mmap", "opendir", "open",
                --"pclose", "perror", "popen", "pow", "putchar", "qsort", "rand",
                --"readdir", "read", "remove", "rename", "rewind", "rindex",
                --"rmdir", "select", "setbuf", "setreuid", "setrlimit", "signal",
                --"sinf", "sinl", "sin", "sleep", "sqrtf", "sqrt", "srand48",
                --"srand", "stat", "strcasecmp", "strcat", "strchr", "strcmp",
                --"strcpy", "strcspn", "strdup", "strerror", "strftime",
                --"strlen", "strncasecmp", "strncat", "strncmp", "strncpy",
                --"strpbrk", "strrchr", "strspn", "strstr", "strtod", "strtok",
                --"strtol", "strtoul", "system",
                --"tanf", "tanl", "tan", "times", "time", "tmpfile", "tolower", "toupper",
                --"umask", "unlink", "write",
                "calloc", "free", "main", "malloc", "realloc" ]
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

    functionsToInstrument :: [Definition] -> Data.Set.Set Name
    functionsToInstrument defs = Data.Set.filter (not . isInfixOfName "isoc99") $
                                 Data.Set.filter (not . isInfixOfName "llvm.") $
                                 Data.Set.difference (Data.Set.fromList $ map getFuncName
                                                                        $ filter isFuncDef
                                                                        $ defs)
                                                     (Data.Set.union ignoredFunctions
                                                                     wrappedFunctions)

    isInfixOfName :: String -> Name -> Bool
    isInfixOfName s (Name s') = isInfixOf s $ show s'
    isInfixOfName _ _ = False

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

    -- Setup the instrumentation of any pointer arguments to the function, and
    -- then branch unconditionally to the first block in the function body.

    instrumentPointerArgs fblabel pms = do
      let pointerArgs = map (\(Parameter t n _) -> (t, n)) $ filter isPointerArg pms
      let shadowStackIndices :: [Integer] = [1..]
      emitBlockStart (mkName "sbcets_parameter_metadata_init")
      zipWithM_ emitMetadataLoadFromShadowStack pointerArgs shadowStackIndices
      emitTerm $ Br fblabel []
      where
        isPointerArg (Parameter (PointerType _ _) _ _) = True
        isPointerArg _ = False

    instrumentBlocks [] = return ()

    instrumentBlocks (first:[]) = do
      emitFirstBlock first

    instrumentBlocks (first:blocks) = do
      emitFirstBlock first
      mapM_ emitBlock blocks

    -- The first thing we need to do in any function is call
    -- __softboundcets_get_global_lock() to get a pointer to the lock for global
    -- variables. This allows us to detect use after free of globals.
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
      modify $ \s -> s { localStackFrameKeyPtr = Just keyPtr, localStackFrameLockPtr = Just lockPtr }
      return ()

    emitBlock (BasicBlock n i t) = do
      saved <- gets metadataTable
      emitBlockStart n
      mapM_ instrumentInst i
      instrumentTerm t
      modify $ \s -> s { metadataTable = saved }

    instrumentTerm i@(Do (Ret (Just op@(LocalReference (PointerType _ _) _)) _)) = do
      emitMetadataStoreToShadowStack op 0
      emitLocalKeyAndLockDestruction
      emitNamedTerm i

    instrumentTerm i = do
      emitNamedTerm i

    -- Invalidate the local key; We do this just prior to returning from the function.
    -- Subsequent use of a leaked stack-allocated variable from inside the function
    -- will trigger a runtime error with a key mismatch.
    emitLocalKeyAndLockDestruction = do
      keyPtr <- liftM fromJust $ gets localStackFrameKeyPtr
      key <- load keyPtr 0
      (fname, fproto) <- gets ((!! "__softboundcets_destroy_stack_key") . runtimeFunctionPrototypes)
      _ <- call (ConstantOperand $ Const.GlobalReference (ptr fproto) $ mkName fname)
                [(key, [])]
      return ()

    emitNamedTerm t = do
      modifyBlock $ \bb -> bb
        { partialBlockTerm = Just t }

    getMetadataForPointee addr@(LocalReference (PointerType _ _) _) = do
      basePtr <- alloca (ptr i8) Nothing 8
      boundPtr <- alloca (ptr i8) Nothing 8
      keyPtr <- alloca (i64) Nothing 8
      lockPtr <- alloca (ptr i8) Nothing 8
      addr' <- bitcast addr (ptr i8)
      (fname, fproto) <- gets ((!! "__softboundcets_metadata_load") . runtimeFunctionPrototypes)
      _ <- call (ConstantOperand $ Const.GlobalReference (ptr fproto) $ mkName fname)
                [(addr', []), (basePtr, []), (boundPtr, []), (keyPtr, []), (lockPtr, [])]
      return (basePtr, boundPtr, keyPtr, lockPtr)

    getMetadataForPointee x@_ = error $ "getMetadataForPointee: expected pointer but saw " ++ show x

    getNullMetadata = do
      nullPtr <- inttoptr (int64 0) (ptr i8)
      basePtr <- alloca (ptr i8) Nothing 8
      boundPtr <- alloca (ptr i8) Nothing 8
      keyPtr <- alloca (i64) Nothing 8
      lockPtr <- alloca (ptr i8) Nothing 8
      (fname, fproto) <- gets ((!! "__softboundcets_metadata_load") . runtimeFunctionPrototypes)
      _ <- call (ConstantOperand $ Const.GlobalReference (ptr fproto) $ mkName fname)
                [(nullPtr, []), (basePtr, []), (boundPtr, []), (keyPtr, []), (lockPtr, [])]
      return (basePtr, boundPtr, keyPtr, lockPtr)

    verifyMetadata _ md@((LocalReference (PointerType (PointerType (IntegerType 8) _) _) _),
                         (LocalReference (PointerType (PointerType (IntegerType 8) _) _) _),
                         (LocalReference (PointerType (IntegerType 64) _) _),
                         (LocalReference (PointerType (PointerType (IntegerType 8) _) _) _)) = md

    verifyMetadata inst md@_ = error $ "Incorrect types in metadata: " ++ show md ++ " while processing instruction " ++ show inst

    instrumentInst i@(v := o)
      | (Load _ addr@(LocalReference (PointerType ty _) _) _ _ _) <- o = do
        haveMetadata <- gets ((Data.Map.member addr) . metadataTable)
        when haveMetadata $ do
          (basePtr, boundPtr, keyPtr, lockPtr) <- liftM (verifyMetadata i) $ gets ((! addr) . metadataTable)
          base <- load basePtr 0
          bound <- load boundPtr 0
          addr' <- bitcast addr (ptr i8)
          tySize <- sizeof 64 ty
          -- Check the load is spatially in bounds
          (fname, fproto) <- gets ((!! "__softboundcets_spatial_load_dereference_check") . runtimeFunctionPrototypes)
          _ <- call (ConstantOperand $ Const.GlobalReference (ptr fproto) $ mkName fname)
                    [(base, []), (bound, []), (addr', []), (tySize, [])]
          -- Check the load is temporally in bounds
          (fname', fproto') <- gets ((!! "__softboundcets_temporal_load_dereference_check") . runtimeFunctionPrototypes)
          lock <- load lockPtr 0
          key <- load keyPtr 0
          _ <- call (ConstantOperand $ Const.GlobalReference (ptr fproto') $ mkName fname')
                    [(lock, []), (key, [])]
          return ()

        emitNamedInst i

        -- If we just loaded a pointer, fetch the metadata for that pointer.
        let loadedValueIsPointer = isPointerType ty
        when loadedValueIsPointer $ do
          let loadedPtr = LocalReference ty v
          loadedPtrMetadata <- liftM (verifyMetadata i) $ getMetadataForPointee addr
          modify $ \s -> s { metadataTable = Data.Map.insert loadedPtr loadedPtrMetadata $ metadataTable s }

      -- Instrument a call instruction unless it is calling inline assembly.
      | (Call _ _ _ (Right (ConstantOperand (Const.GlobalReference (PointerType (FunctionType rt _ False) _) fname))) opds _ _) <- o = do
        let ptrArgs = map fst $ filter (isPointerOperand . fst) opds
        emitShadowStackAllocation (fromIntegral $ 1 + length ptrArgs)
        zipWithM_ emitMetadataStoreToShadowStack ptrArgs [1..]
        if Data.Set.member fname wrappedFunctions then
          emitNamedInst $ v := (rewriteCalledFunctionName (wrappedFunctionNames ! fname) o)
        else emitNamedInst i
        -- The function could deallocate any of the passed pointers so behave as if it has deallocated all of them
        modify $ \s -> s { metadataTable = foldr ($) (metadataTable s) $ map Data.Map.delete ptrArgs }
        -- Read the pointer metadata for the return value if it is a pointer
        when (isPointerType rt) $ emitMetadataLoadFromShadowStack (rt, v) 0
        emitShadowStackDeallocation

      | (GetElementPtr _ addr@(LocalReference (PointerType {}) _) ixs _) <- o = do
        haveMetadata <- gets ((Data.Map.member addr) . metadataTable)
        when haveMetadata $ do
          ty <- computeIndexedType (typeOf addr) ixs
          -- If we cannot compute the type of the indexed entity, don't instrument this pointer to it.
          -- This can happen in the case of opaque structure types. The original softboundcets code also bails here.
          when (isJust ty) $ do
            let newPtr = LocalReference (ptr $ fromJust ty) v
            newMetadata <- liftM (verifyMetadata i) $ gets ((! addr) . metadataTable)
            modify $ \s -> s { metadataTable = Data.Map.insert newPtr newMetadata $ metadataTable s }
        emitNamedInst i

      | (BitCast addr@(LocalReference (PointerType {}) _) ty _) <- o = do
        haveMetadata <- gets ((Data.Map.member addr) . metadataTable)
        when haveMetadata $ do
          let newPtr = LocalReference ty v
          newMetadata <- liftM (verifyMetadata i) $ gets ((! addr) . metadataTable)
          modify $ \s -> s { metadataTable = Data.Map.insert newPtr newMetadata $ metadataTable s }
        emitNamedInst i

      | otherwise = do
        emitNamedInst i

    instrumentInst i@(Do o)
      -- This alternative is the non-capturing variant (call ignoring return value, if any).
      -- We don't need to emit checks for the return value here because it is unused.
      | (Call _ _ _ (Right (ConstantOperand (Const.GlobalReference (PointerType (FunctionType _ _ False) _) fname))) opds _ _) <- o = do
        let ptrArgs = map fst $ filter (isPointerOperand . fst) opds
        emitShadowStackAllocation (fromIntegral $ 1 + length ptrArgs)
        zipWithM_ emitMetadataStoreToShadowStack ptrArgs [1..]
        if Data.Set.member fname wrappedFunctions then
          emitNamedInst $ Do $ rewriteCalledFunctionName (wrappedFunctionNames ! fname) o
        else emitNamedInst i
        -- the function could deallocate any of the passed pointers so behave as if it has deallocated all of them
        modify $ \s -> s { metadataTable = foldr ($) (metadataTable s) $ map Data.Map.delete ptrArgs }
        emitShadowStackDeallocation

      | (Store _ tgt@(LocalReference (PointerType ty _) _) src _ _ _) <- o = do
        haveTargetMetadata <- gets ((Data.Map.member tgt) . metadataTable)
        when haveTargetMetadata $ do
          (tgtBasePtr, tgtBoundPtr, tgtKeyPtr, tgtLockPtr) <- liftM (verifyMetadata i) $ gets ((! tgt) . metadataTable)
          -- Check the store is spatially in bounds
          (fname, fproto) <- gets ((!! "__softboundcets_spatial_store_dereference_check") . runtimeFunctionPrototypes)
          tgtBase <- load tgtBasePtr 0
          tgtBound <- load tgtBoundPtr 0
          tgtAddr <- bitcast tgt (ptr i8)
          tySize <- sizeof 64 ty
          _ <- call (ConstantOperand $ Const.GlobalReference (ptr fproto) $ mkName fname)
                    [(tgtBase, []), (tgtBound, []), (tgtAddr, []), (tySize, [])]
          -- Check the store is temporally in bounds
          (fname'', fproto'') <- gets ((!! "__softboundcets_temporal_store_dereference_check") . runtimeFunctionPrototypes)
          tgtKey <- load tgtKeyPtr 0
          tgtLock <- load tgtLockPtr 0
          _ <- call (ConstantOperand $ Const.GlobalReference (ptr fproto'') $ mkName fname'')
                    [(tgtLock, []), (tgtKey, [])]
          return ()

        emitNamedInst i

        let storedValueIsPointer = isPointerType ty
        let storedValueIsHandled = isLocalReference src
        when (storedValueIsPointer && storedValueIsHandled) $ do
          haveSourceMetadata <- gets ((Data.Map.member src) . metadataTable)
          when haveSourceMetadata $ do
            (srcBasePtr, srcBoundPtr, srcKeyPtr, srcLockPtr) <- liftM (verifyMetadata i) $ gets ((! src) . metadataTable)
            tgtAddr <- bitcast tgt (ptr i8)
            srcBase <- load srcBasePtr 0
            srcBound <- load srcBoundPtr 0
            srcKey <- load srcKeyPtr 0
            srcLock <- load srcLockPtr 0
            (fname', fproto') <- gets ((!! "__softboundcets_metadata_store") . runtimeFunctionPrototypes)
            _ <- call (ConstantOperand $ Const.GlobalReference (ptr fproto') $ mkName fname')
                        [(tgtAddr, []), (srcBase, []), (srcBound, []), (srcKey, []), (srcLock, [])]
            return ()

      | otherwise = do
        emitNamedInst i

    isLocalReference (LocalReference {}) = True
    isLocalReference _ = False

    isPointerOperand (LocalReference (PointerType {}) _) = True
    isPointerOperand _ = False

    isPointerType (PointerType {}) = True
    isPointerType _ = False

    rewriteCalledFunctionName n (Call tckind cconv retAttrs (Right (ConstantOperand (Const.GlobalReference fty _))) params attrs meta) =
      Call tckind cconv retAttrs (Right (ConstantOperand (Const.GlobalReference fty n))) params attrs meta

    rewriteCalledFunctionName _ _ = undefined

    -- Load the metadata for a pointer function parameter from the shadow stack.
    -- The location of that metadata in the shadow stack is given by the position
    -- of the pointer argument in the list of pointer arguments (starting from one).
    -- The zeroth shadow stack location is reserved for metadata about the return
    -- value, but unused if the return value is not a pointer.
    emitMetadataLoadFromShadowStack (localTy, localName) ix = do
      ix' <- pure $ int32 ix
      (baseName, baseProto) <- gets((!! "__softboundcets_load_base_shadow_stack") . runtimeFunctionPrototypes)
      base <- call (ConstantOperand $ Const.GlobalReference (ptr baseProto) $ mkName baseName) [(ix', [])]
      (boundName, boundProto) <- gets((!! "__softboundcets_load_bound_shadow_stack") . runtimeFunctionPrototypes)
      bound <- call (ConstantOperand $ Const.GlobalReference (ptr boundProto) $ mkName boundName) [(ix', [])]
      (keyName, keyProto) <- gets((!! "__softboundcets_load_key_shadow_stack") . runtimeFunctionPrototypes)
      key <- call (ConstantOperand $ Const.GlobalReference (ptr keyProto) $ mkName keyName) [(ix', [])]
      (lockName, lockProto) <- gets((!! "__softboundcets_load_lock_shadow_stack") . runtimeFunctionPrototypes)
      lock <- call (ConstantOperand $ Const.GlobalReference (ptr lockProto) $ mkName lockName) [(ix', [])]
      basePtr <- alloca (ptr i8) Nothing 8
      boundPtr <- alloca (ptr i8) Nothing 8
      keyPtr <- alloca (i64) Nothing 8
      lockPtr <- alloca (ptr i8) Nothing 8
      store basePtr 8 base
      store boundPtr 8 bound
      store keyPtr 8 key
      store lockPtr 8 lock
      modify $ \s -> s { metadataTable = Data.Map.insert (LocalReference localTy localName) (basePtr, boundPtr, keyPtr, lockPtr) $ metadataTable s }

    -- Store the metadata for a pointer on the shadow stack at the specified position.
    emitMetadataStoreToShadowStack op@(LocalReference (PointerType {}) _) ix = do
      haveMetadata <- gets ((Data.Map.member op) . metadataTable)
      (basePtr, boundPtr, keyPtr, lockPtr) <- if haveMetadata
                                              then gets ((! op) . metadataTable)
                                              else do tell ["Pointer passed to function has no metadata: " ++ show op]
                                                      getNullMetadata
      ix' <- pure $ int32 ix
      base <- load basePtr 0
      bound <- load boundPtr 0
      key <- load keyPtr 0
      lock <- load lockPtr 0
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

    emitMetadataStoreToShadowStack (LocalReference {}) _ = undefined
    emitMetadataStoreToShadowStack (ConstantOperand {}) _ = undefined
    emitMetadataStoreToShadowStack (MetadataOperand {}) _ = undefined

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

    computeIndexedType :: MonadModuleBuilder m => Type -> [Operand] -> m (Maybe Type)
    computeIndexedType ty [] = pure $ Just ty
    computeIndexedType (PointerType ty _) (_:is') = computeIndexedType ty is'
    computeIndexedType (StructureType _ elTys) (ConstantOperand (Const.Int 32 val):is') =
      computeIndexedType (head $ drop (fromIntegral val) elTys) is'
    computeIndexedType (StructureType _ _) (i:_) = error $ "Field indices for structure types must be i32 constants, got: " ++ show i
    computeIndexedType (VectorType _ elTy) (_:is') = computeIndexedType elTy is'
    computeIndexedType (ArrayType _ elTy) (_:is') = computeIndexedType elTy is'
    computeIndexedType (NamedTypeReference nm) is' = do
      mayTy <- liftModuleState (gets (Data.Map.lookup nm . builderTypeDefs))
      case mayTy of
        Nothing -> pure Nothing
        Just ty -> computeIndexedType ty is'
    computeIndexedType t (_:_) = error $ "Can't index into type: " ++ show t
