{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
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
import Data.Maybe (isJust, fromJust, isNothing)
import Data.String (IsString(..))
import Data.List (isInfixOf, nub, sort)
import LLVM.AST hiding (args, index, Metadata)
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
import Instrumentor
import qualified CLI

-- | Metadata is a 4-tuple of pointers to stack-allocated entities:
--   the base pointer, bound pointer, key value, and lock location
--   associated with some user pointer.
type Metadata = (Operand, Operand, Operand, Operand)

getBase :: Metadata -> Operand
getBase (base, _, _, _) = base

getBound :: Metadata -> Operand
getBound (_, bound, _, _) = bound

getKey :: Metadata -> Operand
getKey (_, _, key, _) = key

getLock :: Metadata -> Operand
getLock (_, _, _, lock) = lock

type SoftboundCETSPass a = InstrumentorPass () [String] SBCETSState a

data SBCETSState = SBCETSState { globalLockPtr :: Maybe Operand
                               -- ^ Pointer to the global lock.
                               , localStackFrameKeyPtr :: Maybe Operand
                               -- ^ Pointer to the key value for the current stack frame.
                               , localStackFrameLockPtr :: Maybe Operand
                               -- ^ Pointer to the lock location for the current stack frame.
                               , instrumentationCandidates :: Data.Set.Set Name
                               -- ^ The set of names of functions to instrument in the current module.
                               , stdlibWrapperPrototypes :: Map Name (Name, Type)
                               -- ^ The runtime provides wrappers for these standard library functions.
                               , runtimeFunctionPrototypes :: Map Name Type
                               -- ^ Prototypes of the runtime instrumentation API functions.
                               , basicBlockMetadataTable :: Map Operand Metadata
                               -- ^ The symbol table mapping pointers to their local metadata.
                               --  'basicBlockMetadataTable' must be saved and restored around basic block entry and exit,
                               --   otherwise we will leak metadata identifiers and potentially violate SSA form.
                               , functionMetadataTable :: Map Operand Metadata
                               -- ^ The symbol table mapping pointers to stack allocated entities to their metadata.
                               --  'functionMetadataTable' only needs saving and restoring around function entry and exit.
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
                               , nullMetadata :: Maybe Metadata
                               -- ^ Metadata for the null pointer (used for zero initialized pointers).
                               , metadataStorage :: Map Operand Metadata
                               -- ^ A 'Map' from the SSA register names of pointers to the metadata storage allocated to hold their metadata.
                               }

-- | Create an empty 'SBCETSState'
emptySBCETSState :: SBCETSState
emptySBCETSState = SBCETSState Nothing Nothing Nothing
                               Data.Set.empty
                               Data.Map.empty Data.Map.empty
                               Data.Map.empty Data.Map.empty
                               CLI.defaultOptions Nothing
                               Data.Set.empty Data.Set.empty
                               Nothing Nothing Data.Map.empty

-- | The initial 'SBCETSState' has 'stdlibWrapperPrototypes' and 'runtimeFunctionPrototypes' populated since these are fixed at build time.
initSBCETSState :: SBCETSState
initSBCETSState = emptySBCETSState
  { stdlibWrapperPrototypes = Data.Map.fromList [
    (mkName "calloc",   (mkName "softboundcets_calloc",   FunctionType (ptr i8) [i64, i64] False)),
    (mkName "malloc",   (mkName "softboundcets_malloc",   FunctionType (ptr i8) [i64] False)),
    (mkName "realloc",  (mkName "softboundcets_realloc",  FunctionType (ptr i8) [ptr i8, i64] False)),
    (mkName "free",     (mkName "softboundcets_free",     FunctionType (void) [ptr i8] False))
    ]
  , runtimeFunctionPrototypes = Data.Map.fromList [
    (mkName "__softboundcets_get_global_lock",                   FunctionType (ptr i8) [] False),
    (mkName "__softboundcets_metadata_check",                    FunctionType void [(ptr $ ptr i8), (ptr $ ptr i8), ptr i64, (ptr $ ptr i8)] False),
    (mkName "__softboundcets_metadata_load",                     FunctionType void [ptr i8, (ptr $ ptr i8), (ptr $ ptr i8), ptr i64, (ptr $ ptr i8)] False),
    (mkName "__softboundcets_metadata_store",                    FunctionType void [ptr i8, ptr i8, ptr i8, i64, ptr i8] False),
    (mkName "__softboundcets_load_base_shadow_stack",            FunctionType (ptr i8) [i32] False),
    (mkName "__softboundcets_load_bound_shadow_stack",           FunctionType (ptr i8) [i32] False),
    (mkName "__softboundcets_load_key_shadow_stack",             FunctionType (i64) [i32] False),
    (mkName "__softboundcets_load_lock_shadow_stack",            FunctionType (ptr i8) [i32] False),
    (mkName "__softboundcets_store_base_shadow_stack",           FunctionType void [ptr i8, i32] False),
    (mkName "__softboundcets_store_bound_shadow_stack",          FunctionType void [ptr i8, i32] False),
    (mkName "__softboundcets_store_key_shadow_stack",            FunctionType void [i64, i32] False),
    (mkName "__softboundcets_store_lock_shadow_stack",           FunctionType void [ptr i8, i32] False),
    (mkName "__softboundcets_allocate_shadow_stack_space",       FunctionType void [i32] False),
    (mkName "__softboundcets_deallocate_shadow_stack_space",     FunctionType void [] False),
    (mkName "__softboundcets_spatial_load_dereference_check",    FunctionType void [ptr i8, ptr i8, ptr i8, i64] False),
    (mkName "__softboundcets_temporal_load_dereference_check",   FunctionType void [ptr i8, i64] False),
    (mkName "__softboundcets_spatial_store_dereference_check",   FunctionType void [ptr i8, ptr i8, ptr i8, i64] False),
    (mkName "__softboundcets_temporal_store_dereference_check",  FunctionType void [ptr i8, i64] False),
    (mkName "__softboundcets_create_stack_key",                  FunctionType void [(ptr $ ptr i8), ptr i64] False),
    (mkName "__softboundcets_destroy_stack_key",                 FunctionType void [i64] False)
    ]
  }

-- | Decide whether the given function symbol is a function that should not be instrumented.
isIgnoredFunction :: MonadState SBCETSState m => Name -> m Bool
isIgnoredFunction func
  | isInfixOfName "__softboundcets" func = return True  -- One of our runtime functions
  | isInfixOfName "isoc99" func = return True           -- ISO C99 intrinsic functions
  | isInfixOfName "llvm." func = return True            -- LLVM intrinsic functions
  | otherwise = do
      blist <- gets blacklist
      return $ Data.Set.member func blist               -- Function symbols explicitly blacklisted by the user
  where
    isInfixOfName :: String -> Name -> Bool
    isInfixOfName s (Name s') = isInfixOf s $ show s'
    isInfixOfName _ _ = False

-- | Check if the given function symbol is a function with a runtime wrapper
isWrappedFunction :: MonadState SBCETSState m => Name -> m Bool
isWrappedFunction n = gets (Data.Set.member n . Data.Map.keysSet . stdlibWrapperPrototypes)

-- | 'inspectPointer' is probably the most crucial single function in this code.
--   There are *many* ways to write an expression whose value is a pointer in LLVM IR.
--   'inspectPointer' traverses pointer-type expressions and returns the metadata
--   for the ultimate referent, and the referent type. If 'inspectPointer' doesn't handle
--   some LLVM IR construct, that construct is not instrumented.
inspectPointer :: (MonadState SBCETSState m, MonadWriter [String] m, MonadModuleBuilder m) => Operand -> m (Maybe (Type, Metadata))
inspectPointer p
  | (LocalReference (PointerType ty _) _) <- p, isFunctionType ty = return Nothing
  | (LocalReference (PointerType ty _) n) <- p = do
      unsafe <- gets (not . Data.Set.member n . safe)
      if (not unsafe) then return Nothing
      else do
        haveBlockMetadata <- gets ((Data.Map.member p) . basicBlockMetadataTable)
        haveStackMetadata <- gets ((Data.Map.member p) . functionMetadataTable)
        if (haveBlockMetadata && haveStackMetadata)
        then error $ "inspectPointer: have conflicting basic-block scope and function-scope metadata for pointer: " ++ (unpack $ ppll p)
        else if haveStackMetadata
        then gets (Just . (ty,) . (! p) . functionMetadataTable)
        else gets (Just . (ty,) . (! p) . basicBlockMetadataTable)
  {-
  | (ConstantOperand (Const.Null (PointerType ty _))) <- p = return Nothing
  | (ConstantOperand (Const.Undef (PointerType ty _))) <- p = return Nothing
  | (ConstantOperand (Const.GlobalReference (PointerType ty _) n)) <- p = return Nothing
  | (ConstantOperand (Const.GetElementPtr _ addr ixs)) <- p = do
      ty <- typeIndex (typeOf addr) (map ConstantOperand ixs)
      return Nothing
  | (ConstantOperand (Const.IntToPtr _ (PointerType ty _))) <- p = return Nothing
  | (ConstantOperand (Const.BitCast _ (PointerType ty _))) <- p = return Nothing
  | (ConstantOperand (Const.AddrSpaceCast _ (PointerType ty _))) <- p = return Nothing
  | (ConstantOperand op@(Const.Select _ _ _)) <- p, (PointerType ty _) <- typeOf op = return Nothing
  | (ConstantOperand (Const.ExtractElement v _)) <- p, (PointerType ty _) <- elementType $ typeOf v = return Nothing
  | (ConstantOperand (Const.ExtractValue agg ixs)) <- p = do
      ty <- typeIndex (typeOf agg) (map (ConstantOperand . Const.Int 32 . fromIntegral) ixs)
      return Nothing
  -}
  | otherwise = do
      tell ["inspectPointer: unsupported pointer " ++ (unpack $ ppll p)]
      return Nothing

-- | When we encounter a global definition, we need to allocate metadata for
--   it if it is not a function or function alias. This is because all globals
--   in LLVM implicitly have their address taken; global definitions define a
--   *pointer* to an entity, just like 'alloca' definitions.
createMetadataForGlobal :: (MonadModuleBuilder m, MonadIRBuilder m, MonadState SBCETSState m) => Global -> m ()
createMetadataForGlobal g
  | (GlobalVariable {}) <- g, name g == (Name $ fromString "llvm.global_ctors") = return () -- https://llvm.org/docs/LangRef.html#the-llvm-global-ctors-global-variable
  | (GlobalVariable {}) <- g, name g == (Name $ fromString "llvm.global_dtors") = return () -- https://llvm.org/docs/LangRef.html#the-llvm-global-dtors-global-variable
  | (GlobalVariable {}) <- g, section g == (Just $ fromString "llvm.metadata")  = return () -- LLVM puts metadata in a specially named section
  | (GlobalVariable {}) <- g, isNothing $ initializer g                         = return () -- Uninitialized globals do not get metadata
  | (GlobalVariable {}) <- g = do
      let ty = typeOf $ fromJust $ initializer g
      let gRef = ConstantOperand $ Const.GlobalReference (ptr ty) (name g)
      case ty of
        (PointerType {}) -> do
          if (not $ isFunctionType $ pointerReferent ty)
          then do
            addr <- bitcast gRef (ptr i8)
            base <- gep gRef [int64 0]
            base' <- bitcast base (ptr i8)
            bound <- gep gRef [int64 1]
            bound' <- bitcast bound (ptr i8)
            let key = ConstantOperand $ Const.Int 64 1
            glp <- gets (fromJust . globalLockPtr)
            lock <- load glp 0
            (fname', fproto') <- gets ((!! (mkName "__softboundcets_metadata_store")) . runtimeFunctionPrototypes)
            _ <- call (ConstantOperand $ Const.GlobalReference (ptr fproto') fname')
                        [(addr, []), (base', []), (bound', []), (key, []), (lock, [])]
            return ()
          else return ()

        (StructureType {}) -> return ()
        (ArrayType {}) -> return ()
        (IntegerType {}) -> return ()
        (FloatingPointType {}) -> return ()
        (VectorType {}) -> return ()
        _ -> return ()
  | otherwise = error $ "createMetadataForGlobal: expected global variable, but got: " ++ (unpack $ ppll g)

-- | Emit the declaration of a runtime API function.
emitRuntimeAPIFunctionDecl :: (MonadModuleBuilder m) => (Name, Type) -> m ()
emitRuntimeAPIFunctionDecl decl
  | (fname, (FunctionType retType argTypes _)) <- decl = do
      _ <- extern fname argTypes retType
      return ()
  | otherwise = undefined

-- | Emit a call to a runtime API function.
emitRuntimeAPIFunctionCall :: (MonadIRBuilder m, MonadState SBCETSState m) => String -> [Operand] -> m Operand
emitRuntimeAPIFunctionCall n args = do
  (fname, fproto) <- gets ((!! (mkName n)) . runtimeFunctionPrototypes)
  call (ConstantOperand $ Const.GlobalReference (ptr fproto) fname) $ map (\x -> (x, [])) args

-- | Instrument a given module according to the supplied command-line options and list of blacklisted function symbols.
instrument :: [String] -> CLI.Options -> Module -> IO Module
instrument blacklist' opts m = do
  let sbcetsState = initSBCETSState { options = opts, blacklist = Data.Set.fromList $ map mkName blacklist' }
  ((m', _), warnings) <- runInstrumentorPass sbcetsPass sbcetsState () m
  mapM_ (putStrLn . ("instrumentor: "++)) warnings
  return m'
  where
    sbcetsPass :: SoftboundCETSPass ()
    sbcetsPass m' = do
      rtFuncProtos <- gets (assocs . runtimeFunctionPrototypes)
      _ <- mapM_ emitRuntimeAPIFunctionDecl rtFuncProtos
      stdlibWrapperProtos <- gets (map snd . assocs . stdlibWrapperPrototypes)
      _ <- mapM_ emitRuntimeAPIFunctionDecl stdlibWrapperProtos
      mapM_ instrumentDefinition $ moduleDefinitions m'
      return ()

    instrumentDefinition g
      -- Don't instrument empty functions
      | (GlobalDefinition f@(Function {})) <- g, null $ basicBlocks f = emitDefn g
      -- We do not currently instrument varargs functions
      | (GlobalDefinition f@(Function {})) <- g, snd $ parameters f = emitDefn g
      | (GlobalDefinition f@(Function {})) <- g = do
          hasWrapper <- isWrappedFunction $ name f
          ignore <- isIgnoredFunction (name f)
          if (not ignore && not hasWrapper) || name f == mkName "main" then do
            instrumentFunction f
          else emitDefn g
      | otherwise = emitDefn g

    instrumentFunction f
      | (Function {}) <- f = do
          let name' = if name f == mkName "main" then mkName "softboundcets_main" else name f
          (_, blocks) <- runIRBuilderT emptyIRBuilder { builderNameSuggestion = Just $ fromString "sbcets" } $ do
            modify $ \s -> s { globalLockPtr = Nothing
                             , localStackFrameKeyPtr = Nothing
                             , localStackFrameLockPtr = Nothing
                             , current = Just f
                             , safe = Data.Set.empty
                             , basicBlockMetadataTable = Data.Map.empty
                             , functionMetadataTable = Data.Map.empty
                             , dontCareMetadata = Nothing
                             , nullMetadata = Nothing
                             , metadataStorage = Data.Map.empty
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
        -- The address of a stack allocation is always safe
        modify $ \s -> s { safe = Data.Set.insert v $ safe s }
        enable <- gets (CLI.instrumentStack . options)
        when enable $ do
          let resultPtr = LocalReference (ptr ty) v
          eltSize <- sizeof 64 ty
          intCount <- if isJust count
                      then if not ((typeOf $ fromJust count) == i64)
                           then sext (fromJust count) i64
                           else pure $ fromJust count
                      else pure $ ConstantOperand $ Const.Int 64 1
          allocSize <- mul eltSize intCount
          base <- bitcast resultPtr (ptr i8)
          intBase <- ptrtoint base i64
          intBound <- add allocSize intBase
          bound <- inttoptr intBound (ptr i8)
          meta@(basePtr, boundPtr, keyPtr, lockPtr) <- getOrCreateMetadataStorage resultPtr
          store basePtr 8 base
          store boundPtr 8 bound
          functionKeyPtr <- gets (fromJust . localStackFrameKeyPtr)
          functionLockPtr <- gets (fromJust . localStackFrameLockPtr)
          functionKey <- load functionKeyPtr 0
          functionLock <- load functionLockPtr 0
          store keyPtr 8 functionKey
          store lockPtr 8 functionLock
          modify $ \s -> s { functionMetadataTable = Data.Map.insert resultPtr meta $ functionMetadataTable s }

      | (Load _ addr _ _ _) <- o = do
        enable <- gets (CLI.instrumentLoad . options)
        when enable $ do
          meta <- inspectPointer addr
          case meta of
            (Just (ty, (basePtr, boundPtr, keyPtr, lockPtr))) -> do
              base <- load basePtr 0
              bound <- load boundPtr 0
              addr' <- bitcast addr (ptr i8)
              tySize <- sizeof 64 ty
              -- Check the load is spatially in bounds
              _ <- emitRuntimeAPIFunctionCall "__softboundcets_spatial_load_dereference_check" [base, bound, addr', tySize]
              -- Check the load is temporally in bounds
              lock <- load lockPtr 0
              key <- load keyPtr 0
              _ <- emitRuntimeAPIFunctionCall "__softboundcets_temporal_load_dereference_check" [lock, key]
              return ()
            _ -> return ()

          emitNamedInst i
          -- No matter if we were able to instrument the load or not, if a pointer was loaded, ask the runtime for metadata for the load address.
          when (isPointerType $ pointerReferent $ typeOf addr) $ do
            let loadedPtr = LocalReference (pointerReferent $ typeOf addr) v
            loadedPtrMetadata <- getOrCreateMetadataStorage loadedPtr
            _ <- loadMetadataForAddress addr loadedPtrMetadata
            modify $ \s -> s { basicBlockMetadataTable = Data.Map.insert loadedPtr loadedPtrMetadata $ basicBlockMetadataTable s }

          return ()

      -- Instrument a call instruction unless it is calling inline assembly or a computed function pointer.
      | (Call _ _ _ (Right (ConstantOperand (Const.GlobalReference (PointerType (FunctionType rt _ False) _) fname))) opds _ _) <- o = do
        enable <- gets (CLI.instrumentCall . options)
        ignore <- isIgnoredFunction fname
        if (not enable || ignore)
        then emitNamedInst i
        else do
          case fname of
            (Name {}) -> do -- Calling a function symbol
              let ptrArgs = filter (not . isFunctionType . pointerReferent . typeOf) $
                            filter isPointerOperand $ map fst opds
              emitShadowStackAllocation (fromIntegral $ 1 + length ptrArgs)
              zipWithM_ (emitMetadataStoreToShadowStack $ Just fname) ptrArgs [1..]
              hasWrapper <- isWrappedFunction fname
              if hasWrapper
              then do
                wrapperFunctionName <- gets (fst . (! fname) . stdlibWrapperPrototypes)
                emitNamedInst $ v := (rewriteCalledFunctionName wrapperFunctionName o)
              else emitNamedInst i
              -- The function could deallocate any of the passed pointers so behave as if it has deallocated all of them
              modify $ \s -> s { basicBlockMetadataTable = foldr ($) (basicBlockMetadataTable s) $ map Data.Map.delete ptrArgs }
              -- Read the pointer metadata for the return value if it is a pointer
              when (isPointerType rt) $ do
                emitMetadataLoadFromShadowStack (LocalReference rt v) 0
              emitShadowStackDeallocation
            (UnName {}) -> do -- Calling a computed function pointer
              emitNamedInst i

      | (GetElementPtr _ addr@(LocalReference (PointerType ty _) n) ixs _) <- o = do
        when (not $ isFunctionType ty) $ do
          haveBlockMetadata <- gets ((Data.Map.member addr) . basicBlockMetadataTable)
          haveStackMetadata <- gets ((Data.Map.member addr) . functionMetadataTable)
          unsafe <- gets (not . Data.Set.member n . safe)
          when (haveBlockMetadata || haveStackMetadata) $ do
            ty' <- typeIndex (typeOf addr) ixs
            -- If we cannot compute the type of the indexed entity, don't instrument this pointer to it.
            -- This can happen in the case of opaque structure types. The original softboundcets code also bails here.
            when (isJust ty') $ do
              let newPtr = LocalReference (ptr $ fromJust ty') v
              oldMetadata <- if haveStackMetadata
                             then gets ((! addr) . functionMetadataTable)
                             else gets ((! addr) . basicBlockMetadataTable)
              if haveStackMetadata
              then modify $ \s -> s { functionMetadataTable = Data.Map.insert newPtr oldMetadata $ functionMetadataTable s }
              else modify $ \s -> s { basicBlockMetadataTable = Data.Map.insert newPtr oldMetadata $ basicBlockMetadataTable s }
              if not unsafe
              then modify $ \s -> s { safe = Data.Set.insert v $ safe s }
              else return ()
              -- The pointer created by getelementptr aliases a pointer with allocated metadata storage
              modify $ \s -> s { metadataStorage = Data.Map.insert newPtr oldMetadata $ metadataStorage s }
        emitNamedInst i

      | (BitCast addr@(LocalReference (PointerType ty' _) n) ty _) <- o = do
        enable <- gets (CLI.instrumentBitcast . options)
        if not enable
        then emitNamedInst i
        else do
          when (not $ isFunctionType ty') $ do
            haveBlockMetadata <- gets ((Data.Map.member addr) . basicBlockMetadataTable)
            haveStackMetadata <- gets ((Data.Map.member addr) . functionMetadataTable)
            unsafe <- gets (not . Data.Set.member n . safe)
            when (haveBlockMetadata || haveStackMetadata) $ do
              let newPtr = LocalReference ty v
              oldMetadata <- if haveStackMetadata
                             then gets ((! addr) . functionMetadataTable)
                             else gets ((! addr) . basicBlockMetadataTable)
              if haveStackMetadata
              then modify $ \s -> s { functionMetadataTable = Data.Map.insert newPtr oldMetadata $ functionMetadataTable s }
              else modify $ \s -> s { basicBlockMetadataTable = Data.Map.insert newPtr oldMetadata $ basicBlockMetadataTable s }
              if not unsafe
              then modify $ \s -> s { safe = Data.Set.insert v $ safe s }
              else return ()
              -- The pointer created by bitcast aliases a pointer with allocated metadata storage
              modify $ \s -> s { metadataStorage = Data.Map.insert newPtr oldMetadata $ metadataStorage s }

          emitNamedInst i

      | (Select cond tval@(LocalReference (PointerType ty _) tn) fval@(LocalReference _ fn) _) <- o = do
        emitNamedInst i
        haveBlockMetadataT <- gets ((Data.Map.member tval) . basicBlockMetadataTable)
        haveStackMetadataT <- gets ((Data.Map.member tval) . functionMetadataTable)
        haveBlockMetadataF <- gets ((Data.Map.member fval) . basicBlockMetadataTable)
        haveStackMetadataF <- gets ((Data.Map.member fval) . functionMetadataTable)
        let haveMetadata = (haveBlockMetadataT || haveStackMetadataT) && (haveBlockMetadataF || haveStackMetadataF)
        unsafeT <- gets (not . Data.Set.member tn . safe)
        unsafeF <- gets (not . Data.Set.member fn . safe)

        when ((not $ isFunctionType ty) && haveMetadata) $ do
          tMeta <- if haveStackMetadataT
                   then gets ((! tval) . functionMetadataTable)
                   else gets ((! tval) . basicBlockMetadataTable)
          fMeta <- if haveStackMetadataF
                   then gets ((! fval) . functionMetadataTable)
                   else gets ((! fval) . basicBlockMetadataTable)
          basePtr <- select cond (getBase tMeta) (getBase fMeta)
          boundPtr <- select cond (getBound tMeta) (getBound fMeta)
          keyPtr <- select cond (getKey tMeta) (getKey fMeta)
          lockPtr <- select cond (getLock tMeta) (getLock fMeta)
          let newPtr = LocalReference (ptr ty) v
          let newMeta = (basePtr, boundPtr, keyPtr, lockPtr)
          if haveStackMetadataT && haveStackMetadataF
          then modify $ \s -> s { functionMetadataTable = Data.Map.insert newPtr newMeta $ functionMetadataTable s }
          else modify $ \s -> s { basicBlockMetadataTable = Data.Map.insert newPtr newMeta $ basicBlockMetadataTable s }
          if not (unsafeT && unsafeF)
          then modify $ \s -> s { safe = Data.Set.insert v $ safe s }
          else return ()
          -- The pointer created by select aliases a pointer with allocated metadata storage
          modify $ \s -> s { metadataStorage = Data.Map.insert newPtr newMeta $ metadataStorage s }

      | (Phi (PointerType ty _) incoming _) <- o = do
        emitNamedInst i

        let phiMeta f (op, n) = do
              meta <- if isConstantOperand op
                      then gets (fromJust . dontCareMetadata)
                      else do
                        allocated <- gets ((Data.Map.member op) .  metadataStorage)
                        if allocated then gets ((! op) . metadataStorage)
                        else error $ "no metadata storage allocated for incoming pointer " ++ (unpack $ ppll op) ++ " in " ++ (unpack $ ppll o)
              return (f meta, n)

        when (not $ isFunctionType ty) $ do
          incomingBases <- forM incoming (phiMeta getBase)
          basePtr <- phi incomingBases
          incomingBounds <- forM incoming (phiMeta getBound)
          boundPtr <- phi incomingBounds
          incomingKeys <- forM incoming (phiMeta getKey)
          keyPtr <- phi incomingKeys
          incomingLocks <- forM incoming (phiMeta getLock)
          lockPtr <- phi incomingLocks
          let newPtr = LocalReference (ptr ty) v
          let newMeta = (basePtr, boundPtr, keyPtr, lockPtr)
          -- The pointer created by phi is only assumed valid within the current basic block
          modify $ \s -> s { basicBlockMetadataTable = Data.Map.insert newPtr newMeta $ basicBlockMetadataTable s }

      | otherwise = emitNamedInst i

    instrumentInst i@(Do o)
      -- This alternative is the non-capturing variant (call ignoring return value, if any).
      -- We don't need to emit checks for the return value here because it is unused.
      | (Call _ _ _ (Right (ConstantOperand (Const.GlobalReference (PointerType (FunctionType _ _ False) _) fname))) opds _ _) <- o = do
        enable <- gets (CLI.instrumentCall . options)
        ignore <- isIgnoredFunction fname
        if (not enable || ignore)
        then emitNamedInst i
        else do
          case fname of
            (Name {}) -> do -- Calling a function symbol
              let ptrArgs = filter (not . isFunctionType . pointerReferent . typeOf) $
                            filter isPointerOperand $ map fst opds
              emitShadowStackAllocation (fromIntegral $ 1 + length ptrArgs)
              zipWithM_ (emitMetadataStoreToShadowStack $ Just fname) ptrArgs [1..]
              hasWrapper <- isWrappedFunction fname
              if hasWrapper
              then do
                wrapperFunctionName <- gets (fst . (! fname) . stdlibWrapperPrototypes)
                emitNamedInst $ Do $ rewriteCalledFunctionName wrapperFunctionName o
              else emitNamedInst i
              -- The function could deallocate any of the passed pointers so (conservatively) behave as if it has deallocated all of them
              modify $ \s -> s { basicBlockMetadataTable = foldr ($) (basicBlockMetadataTable s) $ map Data.Map.delete ptrArgs }
              emitShadowStackDeallocation
            (UnName {}) -> do -- Calling a computed function pointer
              emitNamedInst i

      | (Store _ tgt@(LocalReference (PointerType ty _) n) src _ _ _) <- o = do
        enable <- gets (CLI.instrumentStore . options)
        when (enable && (not $ isFunctionType ty)) $ do
          haveTargetBlockMetadata <- gets ((Data.Map.member tgt) . basicBlockMetadataTable)
          haveTargetStackMetadata <- gets ((Data.Map.member tgt) . functionMetadataTable)
          unsafe <- gets (not . Data.Set.member n . safe)
          when (unsafe && (haveTargetBlockMetadata || haveTargetStackMetadata)) $ do
            (tgtBasePtr, tgtBoundPtr, tgtKeyPtr, tgtLockPtr) <- if haveTargetStackMetadata
                                                                then gets ((! tgt) . functionMetadataTable)
                                                                else gets ((! tgt) . basicBlockMetadataTable)
            emitCheck <- gets (CLI.emitChecks . options)
            when emitCheck $ do
              _ <- emitRuntimeAPIFunctionCall "__softboundcets_metadata_check" [tgtBasePtr, tgtBoundPtr, tgtKeyPtr, tgtLockPtr]
              return ()
            -- Check the store is spatially in bounds
            tgtBase <- load tgtBasePtr 0
            tgtBound <- load tgtBoundPtr 0
            tgtAddr <- bitcast tgt (ptr i8)
            tySize <- sizeof 64 ty
            _ <- emitRuntimeAPIFunctionCall "__softboundcets_spatial_store_dereference_check" [tgtBase, tgtBound, tgtAddr, tySize]
            -- Check the store is temporally in bounds
            tgtKey <- load tgtKeyPtr 0
            tgtLock <- load tgtLockPtr 0
            _ <- emitRuntimeAPIFunctionCall "__softboundcets_temporal_store_dereference_check" [tgtLock, tgtKey]
            return ()

        emitNamedInst i

        when (not $ isFunctionType ty) $ do
          let storedValueIsPointer = isPointerType ty
          let storedValueIsHandled = isLocalReference src
          when (storedValueIsPointer && storedValueIsHandled) $ do
            haveSourceBlockMetadata <- gets ((Data.Map.member src) . basicBlockMetadataTable)
            haveSourceStackMetadata <- gets ((Data.Map.member src) . functionMetadataTable)
            when (haveSourceBlockMetadata || haveSourceStackMetadata) $ do
              (srcBasePtr, srcBoundPtr, srcKeyPtr, srcLockPtr) <- if haveSourceStackMetadata
                                                                  then gets ((! src) . functionMetadataTable)
                                                                  else gets ((! src) . basicBlockMetadataTable)
              emitCheck <- gets (CLI.emitChecks . options)
              when emitCheck $ do
                _ <- emitRuntimeAPIFunctionCall "__softboundcets_metadata_check" [srcBasePtr, srcBoundPtr, srcKeyPtr, srcLockPtr]
                return ()
              tgtAddr <- bitcast tgt (ptr i8)
              srcBase <- load srcBasePtr 0
              srcBound <- load srcBoundPtr 0
              srcKey <- load srcKeyPtr 0
              srcLock <- load srcLockPtr 0
              _ <- emitRuntimeAPIFunctionCall "__softboundcets_metadata_store" [tgtAddr, srcBase, srcBound, srcKey, srcLock]
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

    emitFirstBlock (BasicBlock n i t) = do
      emitBlockStart n
      -- Set up a handle to the global lock
      glp <- emitRuntimeAPIFunctionCall "__softboundcets_get_global_lock" []
      modify $ \s -> s { globalLockPtr = Just glp }
      -- Create a lock for local allocations
      emitLocalKeyAndLockCreation
      mapM_ instrumentInst i
      instrumentTerm t

    emitBlock (BasicBlock n i t) = do
      saved <- gets basicBlockMetadataTable
      emitBlockStart n
      mapM_ instrumentInst i
      instrumentTerm t
      modify $ \s -> s { basicBlockMetadataTable = saved }

    emitNamedTerm t = do
      modifyBlock $ \bb -> bb
        { partialBlockTerm = Just t }

    emitNamedInst (n := i) = do
      modifyBlock $ \bb -> bb
        { partialBlockInstrs = partialBlockInstrs bb `snoc` (n := i) }

    emitNamedInst (Do i) = do
      emitInstrVoid i

-- | Helper to rewrite the called function symbol (for example, to a wrapper function symbol) at a callsite.
rewriteCalledFunctionName :: Name -> Instruction -> Instruction
rewriteCalledFunctionName n f
  | (Call tckind cconv retAttrs (Right (ConstantOperand (Const.GlobalReference fty _))) params attrs meta) <- f =
      Call tckind cconv retAttrs (Right (ConstantOperand (Const.GlobalReference fty n))) params attrs meta
  | otherwise = undefined

-- | Allocate (or return if already allocated) local variables to hold the metadata for the given pointer.
getOrCreateMetadataStorage :: (MonadState SBCETSState m, MonadIRBuilder m) => Operand -> m Metadata
getOrCreateMetadataStorage pointer
  | (LocalReference (PointerType _ _) _) <- pointer = do
    allocated <- gets ((Data.Map.lookup pointer) . metadataStorage)
    meta <- if isJust allocated
            then gets ((! pointer) . metadataStorage)
            else do
              basePtr <- alloca (ptr i8) Nothing 8
              boundPtr <- alloca (ptr i8) Nothing 8
              keyPtr <- alloca (i64) Nothing 8
              lockPtr <- alloca (ptr i8) Nothing 8
              return (basePtr, boundPtr, keyPtr, lockPtr)
    modify $ \s -> s { metadataStorage = Data.Map.insert pointer meta $ metadataStorage s }
    return meta
  | otherwise = error $ "getOrCreateMetadataStorage: expected pointer but saw " ++ (unpack $ ppll pointer)

-- | Load the metadata for the given address.
loadMetadataForAddress :: (MonadState SBCETSState m, MonadIRBuilder m) => Operand -> Metadata -> m Metadata
loadMetadataForAddress addr meta@(basePtr, boundPtr, keyPtr, lockPtr)
  | (LocalReference (PointerType _ _) _) <- addr = do
    addr' <- bitcast addr (ptr i8)
    _ <- emitRuntimeAPIFunctionCall "__softboundcets_metadata_load" [addr', basePtr, boundPtr, keyPtr, lockPtr]
    emitCheck <- gets (CLI.emitChecks . options)
    when emitCheck $ do
      _ <- emitRuntimeAPIFunctionCall "__softboundcets_metadata_check" [basePtr, boundPtr, keyPtr, lockPtr]
      return ()
    return meta
  | otherwise = error $ "loadMetadataForAddress: expected pointer but saw " ++ (unpack $ ppll addr)

-- | Generate the instrumentation setup block for a function. Allocate space for metadata of any non-function-type pointer arguments, create
--   stack slots eagerly for all locally allocated metadata and then branch unconditionally to the first block in the function body.
emitInstrumentationSetup :: (MonadModuleBuilder m, MonadIRBuilder m, MonadState SBCETSState m) => Global -> m ()
emitInstrumentationSetup f
  | (Function {}) <- f = do
    let firstBlockLabel = (\(BasicBlock n _ _) -> n) $ head $ basicBlocks f
    let pointerArgs = map (\(Parameter t n _) -> (LocalReference t n)) $ filter isNonFunctionPointerParam $ fst $ parameters f
    let shadowStackIndices :: [Integer] = [1..]
    emitBlockStart (mkName "sbcets_metadata_init")
    zipWithM_ emitMetadataLoadFromShadowStack pointerArgs shadowStackIndices
    -- Create the don't-care metadata.
    nullPtr <- inttoptr (int64 0) (ptr i8)
    dcMetadata <- getOrCreateMetadataStorage nullPtr
    _ <- loadMetadataForAddress nullPtr dcMetadata
    modify $ \s -> s { dontCareMetadata = Just dcMetadata }
    -- Create the null pointer metadata
    nullBasePtr <- alloca (ptr i8) Nothing 8
    nullBoundPtr <- alloca (ptr i8) Nothing 8
    nullKeyPtr <- alloca i64 Nothing 8
    nullLockPtr <- alloca (ptr i8) Nothing 8
    store nullBasePtr 8 nullPtr
    store nullBoundPtr 8 nullPtr
    store nullKeyPtr 8 (ConstantOperand $ Const.Int 64 1)
    store nullLockPtr 8 nullPtr
    modify $ \s -> s { nullMetadata = Just (nullBasePtr, nullBoundPtr, nullKeyPtr, nullLockPtr) }
    -- Collect all metadata allocation sites so we can allocate local variables for metadata ahead of time
    metadataAllocationSites <- liftM (nub . sort . concat) $ mapM collectMetadataAllocationSites $ basicBlocks f
    mapM_ createMetadataStackSlots metadataAllocationSites
    emitTerm $ Br firstBlockLabel []
  | otherwise = undefined
  where
      isNonFunctionPointerParam p
        | (Parameter (PointerType (FunctionType {}) _) _ _) <- p = False
        | (Parameter (PointerType {}) _ _) <- p = True
        | otherwise = False

      collectMetadataAllocationSites (BasicBlock _ i t) = do
        instSites <- liftM concat $ mapM examineMetadataAllocationSiteInst i
        termSites <- examineMetadataAllocationSiteTerm t
        return (termSites ++ instSites)

      examineMetadataAllocationSiteInst site
        | (v := o) <- site, (Load _ addr@(LocalReference (PointerType ty _) _) _ _ _) <- o = do
            enable <- gets (CLI.instrumentLoad . options)
            if (enable && (not $ isFunctionType ty) && isPointerType ty)
            then return [addr, LocalReference ty v]
            else return []
        | (v := o) <- site, (Call _ _ _ (Right (ConstantOperand (Const.GlobalReference (PointerType (FunctionType rt _ False) _) fname@(Name {})))) opds _ _) <- o = do
            enable <- gets (CLI.instrumentCall . options)
            ignore <- isIgnoredFunction fname
            if (enable && not ignore)
            then do
              let ptrArgs = filter (not . isFunctionType . pointerReferent . typeOf) $
                            filter isPointerOperand $ map fst opds
              let ptrRet = if (isPointerType rt) then [LocalReference rt v] else []
              return (ptrArgs ++ ptrRet)
            else return []
        | (v := o) <- site, (Phi ty@(PointerType {}) incoming _) <- o = do
            return $ [LocalReference ty v] ++ (map fst $ filter (isLocalReference . fst) incoming)
        | (v := o) <- site, (Alloca ty _ _ _) <- o = do
            enable <- gets (CLI.instrumentStack . options)
            if enable then return [LocalReference (ptr ty) v] else return []
        | (Do o) <- site, (Call _ _ _ (Right (ConstantOperand (Const.GlobalReference (PointerType (FunctionType _ _ False) _) fname@(Name {})))) opds _ _) <- o = do
            enable <- gets (CLI.instrumentCall . options)
            ignore <- isIgnoredFunction fname
            if (enable && not ignore)
            then do
              let ptrArgs = filter (not . isFunctionType . pointerReferent . typeOf) $
                            filter isPointerOperand $ map fst opds
              return ptrArgs
            else return []
        | otherwise = return []

      examineMetadataAllocationSiteTerm site
        | (Do (Ret (Just op@(LocalReference (PointerType _ _) _)) _)) <- site = return [op]
        | otherwise = return []

      createMetadataStackSlots p
        | (LocalReference (PointerType {}) _) <- p = do
            basePtr <- alloca (ptr i8) Nothing 8
            boundPtr <- alloca (ptr i8) Nothing 8
            keyPtr <- alloca (i64) Nothing 8
            lockPtr <- alloca (ptr i8) Nothing 8
            modify $ \s -> s { metadataStorage = Data.Map.insert p (basePtr, boundPtr, keyPtr, lockPtr) $ metadataStorage s }
        | otherwise  = error $ "createMetadataStackSlots: expecting a pointer but saw " ++ (unpack $ ppll p)

-- | Create a local key and lock for entities allocated on the stack inside the current function
emitLocalKeyAndLockCreation :: (MonadState SBCETSState m, MonadIRBuilder m) => m ()
emitLocalKeyAndLockCreation = do
  keyPtr <- alloca i64 Nothing 8
  lockPtr <- alloca (ptr i8) Nothing 8
  _ <- emitRuntimeAPIFunctionCall "__softboundcets_create_stack_key" [lockPtr, keyPtr]
  modify $ \s -> s { localStackFrameKeyPtr = Just keyPtr, localStackFrameLockPtr = Just lockPtr }
  return ()

-- | Invalidate the local key; We do this just prior to returning from the function.
--   Subsequent use of a leaked stack-allocated variable from inside the current function
--   will cause a runtime error with a key mismatch.
emitLocalKeyAndLockDestruction :: (MonadState SBCETSState m, MonadIRBuilder m) => m ()
emitLocalKeyAndLockDestruction = do
  keyPtr <- gets (fromJust . localStackFrameKeyPtr)
  key <- load keyPtr 0
  _ <- emitRuntimeAPIFunctionCall "__softboundcets_destroy_stack_key" [key]
  return ()

-- | Allocate space on the shadow stack for the parameters of an instrumented function we are about to call.
emitShadowStackAllocation :: (MonadState SBCETSState m, MonadIRBuilder m) => Integer -> m ()
emitShadowStackAllocation numArgs = do
  numArgs' <- pure $ int32 numArgs
  _ <- emitRuntimeAPIFunctionCall "__softboundcets_allocate_shadow_stack_space" [numArgs']
  return ()

-- | Deallocate the shadow stack space for the instrumented function which just returned.
emitShadowStackDeallocation :: (MonadState SBCETSState m, MonadIRBuilder m) => m ()
emitShadowStackDeallocation = do
  _ <- emitRuntimeAPIFunctionCall "__softboundcets_deallocate_shadow_stack_space" []
  return ()

-- | Load the metadata for a pointer function parameter from the shadow stack.
emitMetadataLoadFromShadowStack :: (MonadState SBCETSState m, MonadIRBuilder m) => Operand -> Integer -> m ()
emitMetadataLoadFromShadowStack p ix
  | (LocalReference {}) <- p = do
    ix' <- pure $ int32 ix
    base <- emitRuntimeAPIFunctionCall "__softboundcets_load_base_shadow_stack" [ix']
    bound <- emitRuntimeAPIFunctionCall "__softboundcets_load_bound_shadow_stack" [ix']
    key <- emitRuntimeAPIFunctionCall "__softboundcets_load_key_shadow_stack" [ix']
    lock <- emitRuntimeAPIFunctionCall "__softboundcets_load_lock_shadow_stack" [ix']
    newMetadata@(basePtr, boundPtr, keyPtr, lockPtr) <- getOrCreateMetadataStorage p
    store basePtr 8 base
    store boundPtr 8 bound
    store keyPtr 8 key
    store lockPtr 8 lock
    modify $ \s -> s { functionMetadataTable = Data.Map.insert p newMetadata $ functionMetadataTable s }
  | otherwise = undefined

-- | Store the metadata for a pointer on the shadow stack at the specified position.
emitMetadataStoreToShadowStack :: (MonadState SBCETSState m, MonadWriter [String] m, MonadIRBuilder m) => Maybe Name -> Operand -> Integer -> m ()
emitMetadataStoreToShadowStack callee op ix
  | (LocalReference (PointerType {}) _) <- op = do
      haveBlockMetadata <- gets ((Data.Map.member op) . basicBlockMetadataTable)
      haveStackMetadata <- gets ((Data.Map.member op) . functionMetadataTable)
      (basePtr, boundPtr, keyPtr, lockPtr) <- if (haveBlockMetadata || haveStackMetadata)
                                              then
                                                if haveStackMetadata
                                                then gets ((! op) . functionMetadataTable)
                                                else gets ((! op) . basicBlockMetadataTable)
                                              else do
                                                tell ["in function " ++ (unpack $ ppll callee) ++ ": metadata reload for killed pointer " ++ (unpack $ ppll op)]
                                                isAllocated <- gets ((Data.Map.member op) . metadataStorage)
                                                if isAllocated
                                                then do
                                                  newMetadata <- getOrCreateMetadataStorage op
                                                  loadMetadataForAddress op newMetadata
                                                else do
                                                  func <- if isJust callee then return (fromJust callee) else gets (name . fromJust . current)
                                                  error $ "in function " ++ (unpack $ ppll func) ++ ": no metadata storage allocated for pointer " ++ (unpack $ ppll op)
      emitCheck <- gets (CLI.emitChecks . options)
      when emitCheck $ do
        _ <- emitRuntimeAPIFunctionCall "__softboundcets_metadata_check" [basePtr, boundPtr, keyPtr, lockPtr]
        return ()
      ix' <- pure $ int32 ix
      base <- load basePtr 0
      bound <- load boundPtr 0
      key <- load keyPtr 0
      lock <- load lockPtr 0
      _ <- emitRuntimeAPIFunctionCall "__softboundcets_store_base_shadow_stack" [base, ix']
      _ <- emitRuntimeAPIFunctionCall "__softboundcets_store_bound_shadow_stack" [bound, ix']
      _ <- emitRuntimeAPIFunctionCall "__softboundcets_store_key_shadow_stack" [key, ix']
      _ <- emitRuntimeAPIFunctionCall "__softboundcets_store_lock_shadow_stack" [lock, ix']
      return ()
  | otherwise = undefined

-- | Helper predicate.
isConstantOperand :: Operand -> Bool
isConstantOperand (ConstantOperand {}) = True
isConstantOperand _ = False

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

-- | Index into a type with a list of consecutive 'Operand' indices.
typeIndex :: MonadModuleBuilder m => Type -> [Operand] -> m (Maybe Type)
typeIndex ty [] = pure $ Just ty
typeIndex (PointerType ty _) (_:is') = typeIndex ty is'
typeIndex (StructureType _ elTys) (ConstantOperand (Const.Int 32 val):is') =
  typeIndex (head $ drop (fromIntegral val) elTys) is'
typeIndex (StructureType _ _) (i:_) = error $ "Field indices for structure types must be i32 constants, got: " ++ (unpack $ ppll i)
typeIndex (VectorType _ elTy) (_:is') = typeIndex elTy is'
typeIndex (ArrayType _ elTy) (_:is') = typeIndex elTy is'
typeIndex (NamedTypeReference nm) is' = do
  mayTy <- liftModuleState (gets (Data.Map.lookup nm . builderTypeDefs))
  case mayTy of
    Nothing -> pure Nothing
    Just ty -> typeIndex ty is'
typeIndex t (_:_) = error $ "Can't index into type: " ++ (unpack $ ppll t)
