{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-|
Module      : SoftboundCETS
Description : An implementation of the SoftboundCETS instrumentation algorithm
Copyright   : (c) Andrew Anderson, 2021
License     : BSD-3
Maintainer  : aanderso@tcd.ie
Stability   : experimental
-}

module SoftboundCETS where

import Prelude hiding ((!!))
import Control.Monad.State hiding (void)
import Control.Monad.RWS hiding (void)
import qualified Data.Set
import Data.Map hiding (map, filter, null, foldr, drop)
import Data.Maybe (isJust, fromJust)
import Data.String (IsString(..))
import Data.List (isInfixOf, intercalate)
import LLVM.AST hiding (index, Metadata)
import LLVM.AST.Global
import LLVM.AST.Type
import LLVM.AST.Typed (typeOf)
import qualified LLVM.AST.Constant as Const
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Internal.SnocList
import LLVM.Pretty (ppll)
import Data.Text.Lazy (unpack)
import Utils
import qualified CLI

-- | Metadata is a 4-tuple of pointers to stack-allocated entities:
--   the base pointer, bound pointer, key value, and lock location
--   associated with some user pointer.
type Metadata = (Operand, Operand, Operand, Operand)

data SBCETSState = SBCETSState { globalLockPtr :: Maybe Operand
                                 -- ^ Pointer to the global lock.
                               , localStackFrameKeyPtr :: Maybe Operand
                                 -- ^ Pointer to the key value for the current stack frame.
                               , localStackFrameLockPtr :: Maybe Operand
                                 -- ^ Pointer to the lock location for the current stack frame.
                               , instrumentationCandidates :: Data.Set.Set Name
                                 -- ^ The set of names of functions to instrument in the current module.
                               , renamingCandidates :: Data.Set.Set Name
                                 -- ^ The set of names of functions to rename in the current module.
                                 -- Renamed functions are prefixed with "softboundcets_".
                               , wrapperFunctionPrototypes :: Map String Type
                                 -- ^ Prototypes of the runtime wrapper functions, used to create calls.
                               , runtimeFunctionPrototypes :: Map String Type
                                 -- ^ Prototypes of the runtime instrumentation functions, used to create calls.
                               , metadataTable :: Map Operand Metadata
                               -- ^ The symbol table mapping pointers to their local metadata.
                               --  'metadataTable' must be saved and restored around basic block entry and exit,
                               --   otherwise we will leak metadata identifiers and potentially violate SSA form.
                               , stackMetadataTable :: Map Operand Metadata
                               -- ^ The symbol table mapping pointers to stack allocated entities to their metadata.
                               --  'stackMetadataTable' only needs saving and restoring around function entry and exit.
                               --  This is because it's always possible to instrument stack accesses in a way that preserves SSA form without doing any extra analysis.
                               , options :: CLI.Options
                               -- ^ The command line options are stored for inspection.
                               , current :: Maybe Global
                               -- ^ The current function we are processing (needed for decent error reporting).
                               , safe :: Data.Set.Set Name
                               -- ^ The set of known safe pointers.
                               , blacklist :: Data.Set.Set Name
                               -- ^ The set of blacklisted function symbols (these will not be instrumented).
                               }

-- | Create an empty 'SBCETSState'
emptySBCETSState :: SBCETSState
emptySBCETSState = SBCETSState Nothing Nothing Nothing
                               Data.Set.empty Data.Set.empty
                               Data.Map.empty Data.Map.empty
                               Data.Map.empty Data.Map.empty
                               CLI.defaultOptions Nothing
                               Data.Set.empty Data.Set.empty

-- | The initial 'SBCETSState' has 'wrapperFunctionPrototypes' and 'runtimeFunctionPrototypes' populated since these are fixed at build time.
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
    ("__softboundcets_metadata_check", FunctionType void [(ptr $ ptr i8), (ptr $ ptr i8), ptr i64, (ptr $ ptr i8)] False),
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

-- The set of names of standard library functions which have runtime wrappers.
wrappedFunctions :: Data.Set.Set Name
-- | Map from the names of standard library functions to the names of their runtime wrappers.
wrappedFunctionNames :: Map Name Name
(wrappedFunctions, wrappedFunctionNames) =
  let names = [ "calloc", "free", "main", "malloc", "realloc" ]
  in (Data.Set.fromList $ map mkName names,
      Data.Map.fromList $ map (\n -> (mkName n, mkName ("softboundcets_" ++ n))) names)

-- | Decide whether the given function symbol is a function that needs instrumentation.
ignore :: Data.Set.Set Name -> Name -> Bool
ignore blist func
 | isInfixOfName "__softboundcets" func = True
 | isInfixOfName "isoc99" func = True
 | isInfixOfName "llvm." func = True
 | Data.Set.member func blist = True
 | otherwise = False
  where
    isInfixOfName :: String -> Name -> Bool
    isInfixOfName s (Name s') = isInfixOf s $ show s'
    isInfixOfName _ _ = False

-- | Instrument a given module according to the supplied command-line options and list of blacklisted function symbols.
instrument :: [String] -> CLI.Options -> Module -> IO Module
instrument blist opts m = do
  let (warnings, instrumented) = instrumentDefinitions $ moduleDefinitions m
  mapM_ (putStrLn . ("instrumentor: "++)) warnings
  return $ m { moduleDefinitions = instrumented }
  where
    instrumentDefinitions :: [Definition] -> ([String], [Definition])
    instrumentDefinitions defs =
      let blist' = Data.Set.fromList $ map mkName blist
          sbcetsState = initSBCETSState { instrumentationCandidates = functionsToInstrument blist' defs
                                        , renamingCandidates = Data.Set.singleton $ mkName "main"
                                        , options = opts
                                        , blacklist = blist'
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

    functionsToInstrument :: Data.Set.Set Name -> [Definition] -> Data.Set.Set Name
    functionsToInstrument bl defs = Data.Set.filter (not . ignore bl) $
                                    Data.Set.difference (Data.Set.fromList $ map getFuncName
                                                                           $ filter isFuncDef
                                                                           $ defs)
                                                        (Data.Set.union bl wrappedFunctions)

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
        modify $ \s -> s { globalLockPtr = Nothing
                         , localStackFrameKeyPtr = Nothing
                         , localStackFrameLockPtr = Nothing
                         , current = Just f
                         , safe = Data.Set.empty
                         , stackMetadataTable = Data.Map.empty
                         }
        metadataTable' <- gets metadataTable
        instrumentPointerArgs firstBlockLabel $ fst $ parameters f
        instrumentBlocks $ basicBlocks f
        modify $ \s -> s { metadataTable = metadataTable' }
        return ()

      let def = GlobalDefinition $ f { name = name'
                                     , basicBlocks = blocks
                                     }
      emitDefn def
      return ()

    instrumentFunction _ _ = undefined

    -- Setup the instrumentation of any non-function pointer arguments, and
    -- then branch unconditionally to the first block in the function body.
    instrumentPointerArgs fblabel pms = do
      let pointerArgs = map (\(Parameter t n _) -> (t, n)) $ filter isNonFunctionPointerArg pms
      let shadowStackIndices :: [Integer] = [1..]
      emitBlockStart (mkName "sbcets_parameter_metadata_init")
      zipWithM_ emitMetadataLoadFromShadowStack pointerArgs shadowStackIndices
      emitTerm $ Br fblabel []
      where
        isNonFunctionPointerArg (Parameter (PointerType ty _) _ _) =
          case ty of
            (FunctionType {}) -> False
            _ -> True
        isNonFunctionPointerArg _ = False

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
      emitMetadataStoreToShadowStack Nothing op 0
      emitLocalKeyAndLockDestruction
      emitNamedTerm i

    instrumentTerm i@(Do (Ret _ _)) = do
      emitLocalKeyAndLockDestruction
      emitNamedTerm i

    instrumentTerm i = do
      emitNamedTerm i

    -- Invalidate the local key; We do this just prior to returning from the function.
    -- Subsequent use of a leaked stack-allocated variable from inside the function
    -- will trigger a runtime error with a key mismatch.
    emitLocalKeyAndLockDestruction = do
      keyPtr <- gets (fromJust . localStackFrameKeyPtr)
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
      emitCheck <- gets (CLI.emitChecks . options)
      when emitCheck $ do
        (fname', fproto') <- gets ((!! "__softboundcets_metadata_check") . runtimeFunctionPrototypes)
        _ <- call (ConstantOperand $ Const.GlobalReference (ptr fproto') $ mkName fname')
                  [(basePtr, []), (boundPtr, []), (keyPtr, []), (lockPtr, [])]
        return ()

      return (basePtr, boundPtr, keyPtr, lockPtr)

    getMetadataForPointee x@_ = error $ "getMetadataForPointee: expected pointer but saw " ++ (unpack $ ppll x)

    verifyMetadata _ md@((LocalReference (PointerType (PointerType (IntegerType 8) _) _) _),
                         (LocalReference (PointerType (PointerType (IntegerType 8) _) _) _),
                         (LocalReference (PointerType (IntegerType 64) _) _),
                         (LocalReference (PointerType (PointerType (IntegerType 8) _) _) _)) = md

    verifyMetadata inst (basePtr, boundPtr, keyPtr, lockPtr) = error $
      "Incorrect types in metadata: " ++ "(" ++
        intercalate ", " [ unpack $ ppll basePtr
                         , unpack $ ppll boundPtr
                         , unpack $ ppll keyPtr
                         , unpack $ ppll lockPtr ] ++
        ")" ++ " while processing instruction " ++ (unpack $ ppll inst)

    instrumentInst i@(v := o)
      | (Alloca ty count _ _) <- o = do
        -- We emit the alloca first because we reference the result in the instrumentation
        emitNamedInst i
        modify $ \s -> s { safe = Data.Set.insert v $ safe s }
        enable <- gets (CLI.instrumentStack . options)
        when enable $ do
          eltSize <- sizeof 64 ty
          intCount <- if isJust count
                      then sext (fromJust count) i64
                      else pure $ ConstantOperand $ Const.Int 64 1
          allocSize <- mul eltSize intCount
          base <- bitcast (LocalReference (ptr ty) v) (ptr i8)
          intBase <- ptrtoint base i64
          intBound <- add allocSize intBase
          bound <- inttoptr intBound (ptr i8)
          basePtr <- alloca (ptr i8) Nothing 8
          boundPtr <- alloca (ptr i8) Nothing 8
          keyPtr <- gets (fromJust . localStackFrameKeyPtr)
          lockPtr <- gets (fromJust . localStackFrameLockPtr)
          store basePtr 8 base
          store boundPtr 8 bound
          modify $ \s -> s { stackMetadataTable = Data.Map.insert (LocalReference (ptr ty) v) (basePtr, boundPtr, keyPtr, lockPtr) $ stackMetadataTable s }

      | (Load _ addr@(LocalReference (PointerType ty _) n) _ _ _) <- o = do
        enable <- gets (CLI.instrumentLoad . options)
        when (enable && (not $ isFunctionType ty)) $ do
          unsafe <- gets (not . Data.Set.member n . safe)
          haveMetadata <- gets ((Data.Map.member addr) . metadataTable)
          haveStackMetadata <- gets ((Data.Map.member addr) . stackMetadataTable)
          when (unsafe && (haveMetadata || haveStackMetadata)) $ do
            (basePtr, boundPtr, keyPtr, lockPtr) <- if haveStackMetadata
                                                    then liftM (verifyMetadata i) $ gets ((! addr) . stackMetadataTable)
                                                    else liftM (verifyMetadata i) $ gets ((! addr) . metadataTable)
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

        when (not $ isFunctionType ty) $ do
          -- If we just loaded a pointer, fetch the metadata for that pointer.
          let loadedValueIsPointer = isPointerType ty
          when loadedValueIsPointer $ do
            let loadedPtr = LocalReference ty v
            loadedPtrMetadata <- liftM (verifyMetadata i) $ getMetadataForPointee addr
            modify $ \s -> s { metadataTable = Data.Map.insert loadedPtr loadedPtrMetadata $ metadataTable s }

      -- Instrument a call instruction unless it is calling inline assembly or a computed function pointer.
      | (Call _ _ _ (Right (ConstantOperand (Const.GlobalReference (PointerType (FunctionType rt _ False) _) fname))) opds _ _) <- o = do
        enable <- gets (CLI.instrumentCall . options)
        bl <- gets blacklist
        if (ignore bl fname) || not enable
        then emitNamedInst i
        else do
          case fname of
            (Name {}) -> do -- Calling a function symbol
              let ptrArgs = map fst $ filter (isNonFunctionPointerOperand . fst) opds
              emitShadowStackAllocation (fromIntegral $ 1 + length ptrArgs)
              zipWithM_ (emitMetadataStoreToShadowStack $ Just fname) ptrArgs [1..]
              if Data.Set.member fname wrappedFunctions then
                emitNamedInst $ v := (rewriteCalledFunctionName (wrappedFunctionNames ! fname) o)
              else emitNamedInst i
              -- The function could deallocate any of the passed pointers so behave as if it has deallocated all of them
              modify $ \s -> s { metadataTable = foldr ($) (metadataTable s) $ map Data.Map.delete ptrArgs }
              -- Read the pointer metadata for the return value if it is a pointer
              when (isPointerType rt) $ emitMetadataLoadFromShadowStack (rt, v) 0
              emitShadowStackDeallocation
            (UnName {}) -> do -- Calling a computed function pointer
              emitNamedInst i

      | (GetElementPtr _ addr@(LocalReference (PointerType ty _) _) ixs _) <- o = do
        when (not $ isFunctionType ty) $ do
          haveMetadata <- gets ((Data.Map.member addr) . metadataTable)
          haveStackMetadata <- gets ((Data.Map.member addr) . stackMetadataTable)
          when (haveMetadata || haveStackMetadata) $ do
            ty' <- index (typeOf addr) ixs
            -- If we cannot compute the type of the indexed entity, don't instrument this pointer to it.
            -- This can happen in the case of opaque structure types. The original softboundcets code also bails here.
            when (isJust ty') $ do
              let newPtr = LocalReference (ptr $ fromJust ty') v
              newMetadata <- if haveStackMetadata
                             then liftM (verifyMetadata i) $ gets ((! addr) . stackMetadataTable)
                             else liftM (verifyMetadata i) $ gets ((! addr) . metadataTable)
              -- We might get a pointer back from getelementptr that does not point to the stack
              modify $ \s -> s { metadataTable = Data.Map.insert newPtr newMetadata $ metadataTable s }
        emitNamedInst i

      | (BitCast addr@(LocalReference (PointerType ty' _) _) ty _) <- o = do
        enable <- gets (CLI.instrumentBitcast . options)
        if not enable
        then emitNamedInst i
        else do
          when (not $ isFunctionType ty') $ do
            haveMetadata <- gets ((Data.Map.member addr) . metadataTable)
            haveStackMetadata <- gets ((Data.Map.member addr) . stackMetadataTable)
            when (haveMetadata || haveStackMetadata) $ do
              let newPtr = LocalReference ty v
              newMetadata <- if haveStackMetadata
                             then liftM (verifyMetadata i) $ gets ((! addr) . stackMetadataTable)
                             else liftM (verifyMetadata i) $ gets ((! addr) . metadataTable)
              -- Bitcasting a pointer to the stack doesn't make it not a pointer to the stack
              if haveStackMetadata
              then modify $ \s -> s { stackMetadataTable = Data.Map.insert newPtr newMetadata $ stackMetadataTable s }
              else modify $ \s -> s { metadataTable = Data.Map.insert newPtr newMetadata $ metadataTable s }
          emitNamedInst i

      | otherwise = do
        emitNamedInst i

    instrumentInst i@(Do o)
      -- This alternative is the non-capturing variant (call ignoring return value, if any).
      -- We don't need to emit checks for the return value here because it is unused.
      | (Call _ _ _ (Right (ConstantOperand (Const.GlobalReference (PointerType (FunctionType _ _ False) _) fname))) opds _ _) <- o = do
        enable <- gets (CLI.instrumentCall . options)
        bl <- gets blacklist
        if (ignore bl fname) || not enable
        then emitNamedInst i
        else do
          case fname of
            (Name {}) -> do -- Calling a function symbol
              let ptrArgs = map fst $ filter (isNonFunctionPointerOperand . fst) opds
              emitShadowStackAllocation (fromIntegral $ 1 + length ptrArgs)
              zipWithM_ (emitMetadataStoreToShadowStack $ Just fname) ptrArgs [1..]
              if Data.Set.member fname wrappedFunctions then
                emitNamedInst $ Do $ rewriteCalledFunctionName (wrappedFunctionNames ! fname) o
              else emitNamedInst i
              -- The function could deallocate any of the passed pointers so (conservatively) behave as if it has deallocated all of them
              modify $ \s -> s { metadataTable = foldr ($) (metadataTable s) $ map Data.Map.delete ptrArgs }
              emitShadowStackDeallocation
            (UnName {}) -> do -- Calling a computed function pointer
              emitNamedInst i

      | (Store _ tgt@(LocalReference (PointerType ty _) n) src _ _ _) <- o = do
        enable <- gets (CLI.instrumentStore . options)
        when (enable && (not $ isFunctionType ty)) $ do
          haveTargetMetadata <- gets ((Data.Map.member tgt) . metadataTable)
          haveTargetStackMetadata <- gets ((Data.Map.member tgt) . stackMetadataTable)
          unsafe <- gets (not . Data.Set.member n . safe)
          when (unsafe && (haveTargetMetadata || haveTargetStackMetadata)) $ do
            (tgtBasePtr, tgtBoundPtr, tgtKeyPtr, tgtLockPtr) <- if haveTargetStackMetadata
                                                                then liftM (verifyMetadata i) $ gets ((! tgt) . stackMetadataTable)
                                                                else liftM (verifyMetadata i) $ gets ((! tgt) . metadataTable)

            emitCheck <- gets (CLI.emitChecks . options)
            when emitCheck $ do
              (fname', fproto') <- gets ((!! "__softboundcets_metadata_check") . runtimeFunctionPrototypes)
              _ <- call (ConstantOperand $ Const.GlobalReference (ptr fproto') $ mkName fname')
                        [(tgtBasePtr, []), (tgtBoundPtr, []), (tgtKeyPtr, []), (tgtLockPtr, [])]
              return ()

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

        when (not $ isFunctionType ty) $ do
          let storedValueIsPointer = isPointerType ty
          let storedValueIsHandled = isLocalReference src
          when (storedValueIsPointer && storedValueIsHandled) $ do
            haveSourceMetadata <- gets ((Data.Map.member src) . metadataTable)
            haveSourceStackMetadata <- gets ((Data.Map.member src) . stackMetadataTable)
            when (haveSourceMetadata || haveSourceStackMetadata) $ do
              (srcBasePtr, srcBoundPtr, srcKeyPtr, srcLockPtr) <- if haveSourceStackMetadata
                                                                  then liftM (verifyMetadata i) $ gets ((! src) . stackMetadataTable)
                                                                  else liftM (verifyMetadata i) $ gets ((! src) . metadataTable)

              emitCheck <- gets (CLI.emitChecks . options)
              when emitCheck $ do
                (fname'', fproto'') <- gets ((!! "__softboundcets_metadata_check") . runtimeFunctionPrototypes)
                _ <- call (ConstantOperand $ Const.GlobalReference (ptr fproto'') $ mkName fname'')
                          [(srcBasePtr, []), (srcBoundPtr, []), (srcKeyPtr, []), (srcLockPtr, [])]
                return ()

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

    isFunctionType (FunctionType {}) = True
    isFunctionType _ = False

    isNonFunctionPointerOperand o = (isPointerOperand o) && (not $ isFunctionType $ pointerReferent $ typeOf o)

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
      -- We cannot assume that function arguments are always passed on the stack. If we could, we could update stackMetadataTable here instead.
      modify $ \s -> s { metadataTable = Data.Map.insert (LocalReference localTy localName) (basePtr, boundPtr, keyPtr, lockPtr) $ metadataTable s }

    -- Store the metadata for a pointer on the shadow stack at the specified position.
    emitMetadataStoreToShadowStack :: Maybe Name -> Operand -> Integer -> _
    emitMetadataStoreToShadowStack callee op@(LocalReference (PointerType {}) _) ix = do
      haveMetadata <- gets ((Data.Map.member op) . metadataTable)
      haveStackMetadata <- gets ((Data.Map.member op) . stackMetadataTable)
      (basePtr, boundPtr, keyPtr, lockPtr) <- if (haveMetadata || haveStackMetadata)
                                              then
                                                if haveStackMetadata
                                                then gets ((! op) . stackMetadataTable)
                                                else gets ((! op) . metadataTable)
                                              else do
                                                fn <- gets (name . fromJust . current)
                                                tell ["in function " ++ (unpack $ ppll fn) ++
                                                      ": no metadata for pointer " ++ (unpack $ ppll op) ++
                                                      (if isJust callee
                                                       then " passed to callee " ++
                                                         (if Data.Set.member (fromJust callee) wrappedFunctions
                                                          then "(wrapped) " ++ (unpack $ ppll $ fromJust callee)
                                                          else (unpack $ ppll $ fromJust callee))
                                                       else " being returned") ++
                                                      " (storing don't-care metadata to callee shadow stack)" ]
                                                nullPtr <- inttoptr (int64 0) (ptr i8)
                                                basePtr <- alloca (ptr i8) Nothing 8
                                                boundPtr <- alloca (ptr i8) Nothing 8
                                                keyPtr <- alloca (i64) Nothing 8
                                                lockPtr <- alloca (ptr i8) Nothing 8
                                                (fname, fproto) <- gets ((!! "__softboundcets_metadata_load") . runtimeFunctionPrototypes)
                                                _ <- call (ConstantOperand $ Const.GlobalReference (ptr fproto) $ mkName fname)
                                                          [(nullPtr, []), (basePtr, []), (boundPtr, []), (keyPtr, []), (lockPtr, [])]
                                                return (basePtr, boundPtr, keyPtr, lockPtr)

      emitCheck <- gets (CLI.emitChecks . options)
      when emitCheck $ do
        (fname', fproto') <- gets ((!! "__softboundcets_metadata_check") . runtimeFunctionPrototypes)
        _ <- call (ConstantOperand $ Const.GlobalReference (ptr fproto') $ mkName fname')
                  [(basePtr, []), (boundPtr, []), (keyPtr, []), (lockPtr, [])]
        return ()

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

    emitMetadataStoreToShadowStack _ (LocalReference {}) _ = undefined
    emitMetadataStoreToShadowStack _ (ConstantOperand {}) _ = undefined
    emitMetadataStoreToShadowStack _ (MetadataOperand {}) _ = undefined

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

    -- | Index into a type with a list of consecutive 'Operand' indices.
    --   May be used to e.g. compute the type of a structure field access, the return type of a getelementptr instruction, and so on.
    index :: MonadModuleBuilder m => Type -> [Operand] -> m (Maybe Type)
    index ty [] = pure $ Just ty
    index (PointerType ty _) (_:is') = index ty is'
    index (StructureType _ elTys) (ConstantOperand (Const.Int 32 val):is') =
      index (head $ drop (fromIntegral val) elTys) is'
    index (StructureType _ _) (i:_) = error $ "Field indices for structure types must be i32 constants, got: " ++ show i
    index (VectorType _ elTy) (_:is') = index elTy is'
    index (ArrayType _ elTy) (_:is') = index elTy is'
    index (NamedTypeReference nm) is' = do
      mayTy <- liftModuleState (gets (Data.Map.lookup nm . builderTypeDefs))
      case mayTy of
        Nothing -> pure Nothing
        Just ty -> index ty is'
    index t (_:_) = error $ "Can't index into type: " ++ show t
