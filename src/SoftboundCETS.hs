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
import Data.List (isInfixOf, nub, sort)
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
                               , blockMetadataTable :: Map Operand Metadata
                               -- ^ The symbol table mapping pointers to their local metadata.
                               --  'blockMetadataTable' must be saved and restored around basic block entry and exit,
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
                               , dontCareMetadata :: Maybe Metadata
                               -- ^ Metadata that can never cause any runtime checks to fail.
                               , preallocatedMetadataStorage :: Map Operand Metadata
                               -- ^ Pre-allocated stack slots to hold the metadata for each pointer requiring a runtime metadata load
                               }

-- | Create an empty 'SBCETSState'
emptySBCETSState :: SBCETSState
emptySBCETSState = SBCETSState Nothing Nothing Nothing
                               Data.Set.empty Data.Set.empty
                               Data.Map.empty Data.Map.empty
                               Data.Map.empty Data.Map.empty
                               CLI.defaultOptions Nothing
                               Data.Set.empty Data.Set.empty
                               Nothing Data.Map.empty

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

    emitRuntimeAPIFunctionDecl :: (String, Type) -> IRBuilderT (RWST () [String] SBCETSState ModuleBuilder) ()
    emitRuntimeAPIFunctionDecl decl
      | (fname, (FunctionType retType argTypes _)) <- decl = do
          _ <- extern (mkName fname) argTypes retType
          return ()
      | otherwise = undefined

    instrumentDefinition g
      -- Don't instrument empty functions
      | (GlobalDefinition f@(Function {})) <- g, null $ basicBlocks f = emitDefn g
      -- We do not currently instrument varargs functions
      | (GlobalDefinition f@(Function {})) <- g, snd $ parameters f = emitDefn g
      | (GlobalDefinition f@(Function {})) <- g = do
          shouldInstrument <- gets $ (Data.Set.member $ name f) . instrumentationCandidates
          shouldRename <- gets $ (Data.Set.member $ name f) . renamingCandidates
          if shouldInstrument || shouldRename then do
            instrumentFunction shouldRename f
          else emitDefn g
      | otherwise = emitDefn g

    instrumentFunction shouldRename f
      | (Function {}) <- f = do
          let name' = if shouldRename then wrappedFunctionNames ! (name f) else name f
          (_, blocks) <- runIRBuilderT emptyIRBuilder { builderNameSuggestion = Just $ fromString "sbcets" } $ do
            modify $ \s -> s { globalLockPtr = Nothing
                            , localStackFrameKeyPtr = Nothing
                            , localStackFrameLockPtr = Nothing
                            , current = Just f
                            , safe = Data.Set.empty
                            , blockMetadataTable = Data.Map.empty
                            , stackMetadataTable = Data.Map.empty
                            , dontCareMetadata = Nothing
                            , preallocatedMetadataStorage = Data.Map.empty
                            }
            emitInstrumentationSetup f
            instrumentBlocks $ basicBlocks f
            return ()
          emitDefn $ GlobalDefinition $ f { name = name', basicBlocks = blocks }
          return ()
      | otherwise = undefined

    instrumentBlocks bs
      | [] <- bs = return ()
      | (first:[]) <- bs = emitFirstBlock first
      | (first:blocks) <- bs = do
          emitFirstBlock first
          mapM_ emitBlock blocks

    instrumentInst i@(v := o)
      | (Alloca ty count _ _) <- o = do
        -- We emit the alloca first because we reference the result in the instrumentation
        emitNamedInst i
        modify $ \s -> s { safe = Data.Set.insert v $ safe s }
        enable <- gets (CLI.instrumentStack . options)
        when enable $ do
          eltSize <- sizeof 64 ty
          intCount <- if isJust count
                      then
                        if not ((typeOf $ fromJust count) == i64)
                        then sext (fromJust count) i64
                        else pure $ fromJust count
                      else pure $ ConstantOperand $ Const.Int 64 1
          allocSize <- mul eltSize intCount
          base <- bitcast (LocalReference (ptr ty) v) (ptr i8)
          intBase <- ptrtoint base i64
          intBound <- add allocSize intBase
          bound <- inttoptr intBound (ptr i8)
          basePtr <- alloca (ptr i8) Nothing 8
          boundPtr <- alloca (ptr i8) Nothing 8
          keyPtr <- alloca (i64) Nothing 8
          lockPtr <- alloca (ptr i8) Nothing 8
          store basePtr 8 base
          store boundPtr 8 bound
          functionKeyPtr <- gets (fromJust . localStackFrameKeyPtr)
          functionLockPtr <- gets (fromJust . localStackFrameLockPtr)
          functionKey <- load functionKeyPtr 0
          functionLock <- load functionLockPtr 0
          store keyPtr 8 functionKey
          store lockPtr 8 functionLock
          modify $ \s -> s { stackMetadataTable = Data.Map.insert (LocalReference (ptr ty) v) (basePtr, boundPtr, keyPtr, lockPtr) $ stackMetadataTable s }

      | (Load _ addr@(LocalReference (PointerType ty _) n) _ _ _) <- o = do
        enable <- gets (CLI.instrumentLoad . options)
        when (enable && (not $ isFunctionType ty)) $ do
          unsafe <- gets (not . Data.Set.member n . safe)
          haveBlockMetadata <- gets ((Data.Map.member addr) . blockMetadataTable)
          haveStackMetadata <- gets ((Data.Map.member addr) . stackMetadataTable)
          when (unsafe && (haveBlockMetadata || haveStackMetadata)) $ do
            (basePtr, boundPtr, keyPtr, lockPtr) <- if haveStackMetadata
                                                    then gets ((! addr) . stackMetadataTable)
                                                    else gets ((! addr) . blockMetadataTable)
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
            loadedPtrMetadata <- getOrCreateMetadataForPointee addr
            modify $ \s -> s { blockMetadataTable = Data.Map.insert loadedPtr loadedPtrMetadata $ blockMetadataTable s }

      -- Instrument a call instruction unless it is calling inline assembly or a computed function pointer.
      | (Call _ _ _ (Right (ConstantOperand (Const.GlobalReference (PointerType (FunctionType rt _ False) _) fname))) opds _ _) <- o = do
        enable <- gets (CLI.instrumentCall . options)
        bl <- gets blacklist
        if (ignore bl fname) || not enable
        then emitNamedInst i
        else do
          case fname of
            (Name {}) -> do -- Calling a function symbol
              let ptrArgs = filter (not . isFunctionType . pointerReferent . typeOf) $
                            filter isPointerOperand $ map fst opds
              emitShadowStackAllocation (fromIntegral $ 1 + length ptrArgs)
              zipWithM_ (emitMetadataStoreToShadowStack $ Just fname) ptrArgs [1..]
              if Data.Set.member fname wrappedFunctions then
                emitNamedInst $ v := (rewriteCalledFunctionName (wrappedFunctionNames ! fname) o)
              else emitNamedInst i
              -- The function could deallocate any of the passed pointers so behave as if it has deallocated all of them
              modify $ \s -> s { blockMetadataTable = foldr ($) (blockMetadataTable s) $ map Data.Map.delete ptrArgs }
              -- Read the pointer metadata for the return value if it is a pointer
              when (isPointerType rt) $ emitMetadataLoadFromShadowStack (LocalReference rt v) 0
              emitShadowStackDeallocation
            (UnName {}) -> do -- Calling a computed function pointer
              emitNamedInst i

      | (GetElementPtr _ addr@(LocalReference (PointerType ty _) _) ixs _) <- o = do
        when (not $ isFunctionType ty) $ do
          haveBlockMetadata <- gets ((Data.Map.member addr) . blockMetadataTable)
          haveStackMetadata <- gets ((Data.Map.member addr) . stackMetadataTable)
          when (haveBlockMetadata || haveStackMetadata) $ do
            ty' <- index (typeOf addr) ixs
            -- If we cannot compute the type of the indexed entity, don't instrument this pointer to it.
            -- This can happen in the case of opaque structure types. The original softboundcets code also bails here.
            when (isJust ty') $ do
              let newPtr = LocalReference (ptr $ fromJust ty') v
              newMetadata <- if haveStackMetadata
                             then gets ((! addr) . stackMetadataTable)
                             else gets ((! addr) . blockMetadataTable)
              -- We might get a pointer back from getelementptr that does not point to the stack
              modify $ \s -> s { blockMetadataTable = Data.Map.insert newPtr newMetadata $ blockMetadataTable s }
        emitNamedInst i

      | (BitCast addr@(LocalReference (PointerType ty' _) _) ty _) <- o = do
        enable <- gets (CLI.instrumentBitcast . options)
        if not enable
        then emitNamedInst i
        else do
          when (not $ isFunctionType ty') $ do
            haveBlockMetadata <- gets ((Data.Map.member addr) . blockMetadataTable)
            haveStackMetadata <- gets ((Data.Map.member addr) . stackMetadataTable)
            when (haveBlockMetadata || haveStackMetadata) $ do
              let newPtr = LocalReference ty v
              newMetadata <- if haveStackMetadata
                             then gets ((! addr) . stackMetadataTable)
                             else gets ((! addr) . blockMetadataTable)
              -- Bitcasting a pointer to the stack doesn't make it not a pointer to the stack
              if haveStackMetadata
              then modify $ \s -> s { stackMetadataTable = Data.Map.insert newPtr newMetadata $ stackMetadataTable s }
              else modify $ \s -> s { blockMetadataTable = Data.Map.insert newPtr newMetadata $ blockMetadataTable s }
          emitNamedInst i

      | otherwise = emitNamedInst i

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
              let ptrArgs = filter (not . isFunctionType . pointerReferent . typeOf) $
                            filter isPointerOperand $ map fst opds
              emitShadowStackAllocation (fromIntegral $ 1 + length ptrArgs)
              zipWithM_ (emitMetadataStoreToShadowStack $ Just fname) ptrArgs [1..]
              if Data.Set.member fname wrappedFunctions then
                emitNamedInst $ Do $ rewriteCalledFunctionName (wrappedFunctionNames ! fname) o
              else emitNamedInst i
              -- The function could deallocate any of the passed pointers so (conservatively) behave as if it has deallocated all of them
              modify $ \s -> s { blockMetadataTable = foldr ($) (blockMetadataTable s) $ map Data.Map.delete ptrArgs }
              emitShadowStackDeallocation
            (UnName {}) -> do -- Calling a computed function pointer
              emitNamedInst i

      | (Store _ tgt@(LocalReference (PointerType ty _) n) src _ _ _) <- o = do
        enable <- gets (CLI.instrumentStore . options)
        when (enable && (not $ isFunctionType ty)) $ do
          haveTargetMetadata <- gets ((Data.Map.member tgt) . blockMetadataTable)
          haveTargetStackMetadata <- gets ((Data.Map.member tgt) . stackMetadataTable)
          unsafe <- gets (not . Data.Set.member n . safe)
          when (unsafe && (haveTargetMetadata || haveTargetStackMetadata)) $ do
            (tgtBasePtr, tgtBoundPtr, tgtKeyPtr, tgtLockPtr) <- if haveTargetStackMetadata
                                                                then gets ((! tgt) . stackMetadataTable)
                                                                else gets ((! tgt) . blockMetadataTable)
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
            haveSourceMetadata <- gets ((Data.Map.member src) . blockMetadataTable)
            haveSourceStackMetadata <- gets ((Data.Map.member src) . stackMetadataTable)
            when (haveSourceMetadata || haveSourceStackMetadata) $ do
              (srcBasePtr, srcBoundPtr, srcKeyPtr, srcLockPtr) <- if haveSourceStackMetadata
                                                                  then gets ((! src) . stackMetadataTable)
                                                                  else gets ((! src) . blockMetadataTable)
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

      | otherwise = emitNamedInst i

    instrumentTerm i
      | (Do (Ret (Just op@(LocalReference (PointerType _ _) _)) _)) <- i = do
          -- Returning a pointer, put the metadata on the shadow stack
          emitMetadataStoreToShadowStack Nothing op 0
          -- Invalidate the key for this function's local allocations
          emitLocalKeyAndLockDestruction
          emitNamedTerm i
      | (Do (Ret _ _)) <- i = do
          -- Returning a non-pointer, just invalidate the key for this function's local allocations
          emitLocalKeyAndLockDestruction
          emitNamedTerm i
      -- Not a return instruction, don't instrument
      | otherwise = emitNamedTerm i

    rewriteCalledFunctionName n f
      | (Call tckind cconv retAttrs (Right (ConstantOperand (Const.GlobalReference fty _))) params attrs meta) <- f =
          Call tckind cconv retAttrs (Right (ConstantOperand (Const.GlobalReference fty n))) params attrs meta
      | otherwise = undefined

    emitFirstBlock (BasicBlock n i t) = do
      emitBlockStart n
      -- Set up a handle to the global lock
      (fname, fproto) <- gets ((!! "__softboundcets_get_global_lock") . runtimeFunctionPrototypes)
      glp <- call (ConstantOperand $ Const.GlobalReference (ptr fproto) $ mkName fname) []
      modify $ \s -> s { globalLockPtr = Just glp }
      -- Create a lock for local allocations
      emitLocalKeyAndLockCreation
      mapM_ instrumentInst i
      instrumentTerm t

    emitBlock (BasicBlock n i t) = do
      saved <- gets blockMetadataTable
      emitBlockStart n
      mapM_ instrumentInst i
      instrumentTerm t
      modify $ \s -> s { blockMetadataTable = saved }

    emitNamedTerm t = do
      modifyBlock $ \bb -> bb
        { partialBlockTerm = Just t }

    emitNamedInst (n := i) = do
      modifyBlock $ \bb -> bb
        { partialBlockInstrs = partialBlockInstrs bb `snoc` (n := i) }

    emitNamedInst (Do i) = do
      emitInstrVoid i

-- | Look up or create (allocate on the stack) the metadata for the pointed-to pointer.
getOrCreateMetadataForPointee :: (MonadState SBCETSState m, MonadIRBuilder m) => Operand -> m Metadata
getOrCreateMetadataForPointee addr
  | (LocalReference (PointerType _ _) _) <- addr = do
    preallocated <- gets ((Data.Map.lookup addr) . preallocatedMetadataStorage)
    (basePtr, boundPtr, keyPtr, lockPtr) <- if isJust preallocated
                                            then gets ((! addr) . preallocatedMetadataStorage)
                                            else do
                                              basePtr <- alloca (ptr i8) Nothing 8
                                              boundPtr <- alloca (ptr i8) Nothing 8
                                              keyPtr <- alloca (i64) Nothing 8
                                              lockPtr <- alloca (ptr i8) Nothing 8
                                              return (basePtr, boundPtr, keyPtr, lockPtr)
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
  | otherwise = error $ "getOrCreateMetadataForPointee: expected pointer but saw " ++ (unpack $ ppll addr)

-- | Helper predicate.
isLocalReference :: Operand -> Bool
isLocalReference (LocalReference {}) = True
isLocalReference _ = False

-- | Helper predicate.
isPointerOperand :: Operand -> Bool
isPointerOperand (LocalReference (PointerType {}) _) = True
isPointerOperand _ = False

-- | Helper predicate.
isPointerType :: Type -> Bool
isPointerType (PointerType {}) = True
isPointerType _ = False

-- | Helper predicate.
isFunctionType :: Type -> Bool
isFunctionType (FunctionType {}) = True
isFunctionType _ = False

-- | Helper predicate.
isFuncDef :: Definition -> Bool
isFuncDef (GlobalDefinition (Function {})) = True
isFuncDef _ = False

-- | Helper predicate.
getFuncName :: Definition -> Name
getFuncName (GlobalDefinition f@(Function {})) = name f
getFuncName _ = undefined

-- | Generate the instrumentation setup block for a function. Allocate space for metadata of any non-function-type pointer arguments, create
--   stack slots eagerly for all locally allocated metadata and then branch unconditionally to the first block in the function body.
emitInstrumentationSetup :: (MonadIRBuilder m, MonadState SBCETSState m) => Global -> m ()
emitInstrumentationSetup f
  | (Function {}) <- f = do
    let firstBlockLabel = (\(BasicBlock n _ _) -> n) $ head $ basicBlocks f
    let pointerArgs = map (\(Parameter t n _) -> (LocalReference t n)) $ filter isNonFunctionPointerParam $ fst $ parameters f
    let shadowStackIndices :: [Integer] = [1..]
    emitBlockStart (mkName "sbcets_metadata_init")
    zipWithM_ emitMetadataLoadFromShadowStack pointerArgs shadowStackIndices
    -- Create the don't-care metadata.
    nullPtr <- inttoptr (int64 0) (ptr i8)
    dcBasePtr <- alloca (ptr i8) Nothing 8
    dcBoundPtr <- alloca (ptr i8) Nothing 8
    dcKeyPtr <- alloca (i64) Nothing 8
    dcLockPtr <- alloca (ptr i8) Nothing 8
    (fname, fproto) <- gets ((!! "__softboundcets_metadata_load") . runtimeFunctionPrototypes)
    _ <- call (ConstantOperand $ Const.GlobalReference (ptr fproto) $ mkName fname)
              [(nullPtr, []), (dcBasePtr, []), (dcBoundPtr, []), (dcKeyPtr, []), (dcLockPtr, [])]
    modify $ \s -> s { dontCareMetadata = Just (dcBasePtr, dcBoundPtr, dcKeyPtr, dcLockPtr) }
    metadataAllocationSites <- liftM (nub . sort . concat) $ mapM collectMetadataAllocationSites $ basicBlocks f
    mapM_ createMetadataStackSlots metadataAllocationSites
    emitTerm $ Br firstBlockLabel []
  | otherwise = undefined
  where
      isNonFunctionPointerParam p
        | (Parameter (PointerType (FunctionType {}) _) _ _) <- p = False
        | (Parameter (PointerType {}) _ _) <- p = True
        | otherwise = False

      collectMetadataAllocationSites (BasicBlock _ i _) = do
        liftM concat $ mapM examineMetadataAllocationSite i

      examineMetadataAllocationSite site
        | (v := o) <- site, (Load _ (LocalReference (PointerType ty _) _) _ _ _) <- o = do
            enable <- gets (CLI.instrumentLoad . options)
            if (enable && (not $ isFunctionType ty) && isPointerType ty)
            then return [LocalReference ty v]
            else return []
        | otherwise = return []

      createMetadataStackSlots p
        | (LocalReference {}) <- p = do
            basePtr <- alloca (ptr i8) Nothing 8
            boundPtr <- alloca (ptr i8) Nothing 8
            keyPtr <- alloca (i64) Nothing 8
            lockPtr <- alloca (ptr i8) Nothing 8
            modify $ \s -> s { preallocatedMetadataStorage = Data.Map.insert p (basePtr, boundPtr, keyPtr, lockPtr) $ preallocatedMetadataStorage s }
        | otherwise  = error $ "createMetadataStackSlots: expecting a pointer but saw " ++ (unpack $ ppll p)

-- | Create a local key and lock for entities allocated on the stack inside the current function
emitLocalKeyAndLockCreation :: IRBuilderT (IRBuilderT (RWST () [String] SBCETSState ModuleBuilder)) ()
emitLocalKeyAndLockCreation = do
  keyPtr <- alloca i64 Nothing 8
  lockPtr <- alloca (ptr i8) Nothing 8
  (fname, fproto) <- gets ((!! "__softboundcets_create_stack_key") . runtimeFunctionPrototypes)
  _ <- call (ConstantOperand $ Const.GlobalReference (ptr fproto) $ mkName fname)
            [(lockPtr, []), (keyPtr, [])]
  modify $ \s -> s { localStackFrameKeyPtr = Just keyPtr, localStackFrameLockPtr = Just lockPtr }
  return ()

-- | Invalidate the local key; We do this just prior to returning from the function.
--   Subsequent use of a leaked stack-allocated variable from inside the current function
--   will cause a runtime error with a key mismatch.
emitLocalKeyAndLockDestruction :: IRBuilderT (IRBuilderT (RWST () [String] SBCETSState ModuleBuilder)) ()
emitLocalKeyAndLockDestruction = do
  keyPtr <- gets (fromJust . localStackFrameKeyPtr)
  key <- load keyPtr 0
  (fname, fproto) <- gets ((!! "__softboundcets_destroy_stack_key") . runtimeFunctionPrototypes)
  _ <- call (ConstantOperand $ Const.GlobalReference (ptr fproto) $ mkName fname)
            [(key, [])]
  return ()

-- | Allocate space on the shadow stack for the parameters of an instrumented function we are about to call.
emitShadowStackAllocation :: (MonadState SBCETSState m, MonadIRBuilder m) => Integer -> m ()
emitShadowStackAllocation numArgs = do
  numArgs' <- pure $ int32 numArgs
  (fname, fproto) <- gets ((!! "__softboundcets_allocate_shadow_stack_space") . runtimeFunctionPrototypes)
  _ <- call (ConstantOperand $ Const.GlobalReference (ptr fproto) $ mkName fname)
            [(numArgs', [])]
  return ()

-- | Deallocate the shadow stack space for the instrumented function which just returned.
emitShadowStackDeallocation :: IRBuilderT (IRBuilderT (RWST () [String] SBCETSState ModuleBuilder)) ()
emitShadowStackDeallocation = do
  (fname, fproto) <- gets ((!! "__softboundcets_deallocate_shadow_stack_space") . runtimeFunctionPrototypes)
  _ <- call (ConstantOperand $ Const.GlobalReference (ptr fproto) $ mkName fname) []
  return ()

-- | Load the metadata for a pointer function parameter from the shadow stack.
emitMetadataLoadFromShadowStack :: (MonadState SBCETSState m, MonadIRBuilder m) => Operand -> Integer -> m ()
emitMetadataLoadFromShadowStack p ix
  | (LocalReference {}) <- p = do
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
    modify $ \s -> s { blockMetadataTable = Data.Map.insert p (basePtr, boundPtr, keyPtr, lockPtr) $ blockMetadataTable s }
  | otherwise = undefined

-- | Store the metadata for a pointer on the shadow stack at the specified position.
emitMetadataStoreToShadowStack :: (MonadState SBCETSState m, MonadWriter [String] m, MonadIRBuilder m) => Maybe Name -> Operand -> Integer -> m ()
emitMetadataStoreToShadowStack callee op ix
  | (LocalReference (PointerType {}) _) <- op = do
      haveBlockMetadata <- gets ((Data.Map.member op) . blockMetadataTable)
      haveStackMetadata <- gets ((Data.Map.member op) . stackMetadataTable)
      (basePtr, boundPtr, keyPtr, lockPtr) <- if (haveBlockMetadata || haveStackMetadata)
                                              then
                                                if haveStackMetadata
                                                then gets ((! op) . stackMetadataTable)
                                                else gets ((! op) . blockMetadataTable)
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
                                                      " (storing don't-care metadata to callee shadow stack)"]
                                                dcMetadata <- gets (fromJust . dontCareMetadata)
                                                return dcMetadata
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
  | otherwise = undefined

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
