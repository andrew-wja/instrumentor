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

import GHC.Stack (HasCallStack)
import Prelude hiding ((!!))
import Control.Monad.State hiding (void)
import Control.Monad.RWS hiding (void)
import qualified Data.Set
import Data.Map hiding (map, filter, null, foldr, drop, partition)
import Data.Maybe (isJust, fromJust, isNothing)
import Data.String (IsString(..))
import Data.List (nub, sort, intercalate, partition)
import Data.Text.Lazy (unpack)
import LLVM.AST hiding (args, index, Metadata)
import LLVM.AST.Global
import LLVM.AST.Type
import LLVM.AST.Typed (typeOf)
import qualified LLVM.Pretty as PP
import qualified LLVM.AST.Constant as Const
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import Instrumentor
import qualified CLI
import qualified LLVMHelpers as Helpers

-- | SoftboundCETS has two classes of pointer: SAFE and UNSAFE.
data PointerClass = Safe | Unsafe

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
                               , options :: CLI.Options
                               -- ^ The command line options are stored for inspection.
                               , currentFunction :: Maybe Global
                               -- ^ The current function we are processing (needed for decent error reporting).
                               , classifications :: Map Operand PointerClass
                               -- ^ The map of pointers to their class.
                               --   'classifications' needs to be saved and restored around function entry and exit so we don't mistakenly treat
                               --   pointers with the same names in different functions as belonging to the same class. Outside a function, 'classifications' contains global variables.
                               , blacklist :: Data.Set.Set Name
                               -- ^ The set of blacklisted function symbols (these will not be instrumented).
                               , dontCareMetadata :: Maybe Metadata
                               -- ^ Metadata that can never cause any runtime checks to fail.
                               , metadataStorage :: Map Operand Metadata
                               -- ^ A 'Map' from the SSA register names of pointers to the metadata storage allocated to hold their metadata.
                               }

-- | Create an empty 'SBCETSState'
emptySBCETSState :: SBCETSState
emptySBCETSState = SBCETSState Nothing Nothing Nothing
                               Data.Set.empty
                               Data.Map.empty Data.Map.empty
                               CLI.defaultOptions Nothing
                               Data.Map.empty Data.Set.empty
                               Nothing Data.Map.empty

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
  | Helpers.isInfixOfName "__softboundcets" func = return True  -- One of our runtime functions
  | Helpers.isInfixOfName "isoc99" func = return True           -- ISO C99 intrinsic functions
  | Helpers.isInfixOfName "llvm." func = return True            -- LLVM intrinsic functions
  | otherwise = do
      blist <- gets blacklist
      return $ Data.Set.member func blist               -- Function symbols explicitly blacklisted by the user

-- | Decide whether the given function symbol returns a safe pointer
returnsSafePointer :: MonadState SBCETSState m => Name -> m Bool
returnsSafePointer func
  | (mkName "malloc") == func = return True
  | (mkName "calloc") == func = return True
  | (mkName "realloc") == func = return True
  | otherwise = return False

-- | Mark a pointer as belonging to a particular 'PointerClass'.
markPointer :: MonadState SBCETSState m => Operand -> PointerClass -> m ()
markPointer p c = modify $ \s -> s { classifications = Data.Map.insert p c $ classifications s }

-- | Query the 'PointerClass' of a pointer, defaulting to 'Unsafe'.
pointerClass :: MonadState SBCETSState m => Operand -> m PointerClass
pointerClass p = gets (maybe Unsafe id . (Data.Map.lookup p . classifications))

-- | Check if the given function symbol is a function with a runtime wrapper
isWrappedFunction :: MonadState SBCETSState m => Name -> m Bool
isWrappedFunction n = gets (Data.Set.member n . Data.Map.keysSet . stdlibWrapperPrototypes)

-- | 'inspectPointer' is probably the most crucial single function in this code.
--   There are *many* ways to write an expression whose value is a pointer in LLVM IR.
--   'inspectPointer' traverses pointer-type expressions and returns the metadata
--   for the ultimate referent, and the referent type. If 'inspectPointer' doesn't handle
--   some LLVM IR construct, that construct is not instrumented.
inspectPointer :: (HasCallStack, MonadState SBCETSState m, MonadWriter [String] m, MonadModuleBuilder m) => Operand -> m (Maybe (Type, Metadata))
inspectPointer p
  | (LocalReference (PointerType ty _) _) <- p, Helpers.isFunctionType ty = return Nothing
  | (LocalReference (PointerType ty _) _) <- p = do
      pp <- liftM (unpack . PP.render) $ PP.ppOperand p
      fname <- gets (name . fromJust . currentFunction)
      allocated <- gets (Data.Map.member p . metadataStorage)
      if (not allocated)
      then do
        tell ["inspectPointer: in function " ++ (unpack $ PP.ppll fname) ++ ": no storage allocated for metadata for pointer " ++ pp ++ ", instrumentation of " ++ pp ++ " will not be possible"]
        return Nothing
      else gets (Just . (ty,) . (! p) . metadataStorage)
  -- TODO-IMPROVE: Constant expressions of pointer type and global pointers currently just get the don't-care metadata.
  -- This is sufficient for performance testing (since it doesn't alter the amount of work done) but not for real-world use.
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
      fname <- gets (name . fromJust . currentFunction)
      pp <- liftM (unpack . PP.render) $ PP.ppOperand p
      tp <- typeOf p
      case tp of
        (PointerType {}) -> do
          tell ["inspectPointer: in function " ++ (unpack $ PP.ppll fname) ++ ": unsupported pointer " ++ pp ++ ", instrumentation of " ++ pp ++ " will not be possible"]
          return Nothing
        _ -> error $ "inspectPointer: in function " ++ (unpack $ PP.ppll fname) ++ ": argument " ++ pp ++ " is not a pointer"

-- | Allocate local variables to hold the metadata for the given pointer.
allocateLocalMetadataStorage :: (HasCallStack, MonadState SBCETSState m, MonadIRBuilder m, MonadModuleBuilder m) => Operand -> m Metadata
allocateLocalMetadataStorage p = do
  allocated <- gets (Data.Map.member p . metadataStorage)
  if not allocated
  then do
    basePtr <- alloca (ptr i8) Nothing 8
    boundPtr <- alloca (ptr i8) Nothing 8
    keyPtr <- alloca (i64) Nothing 8
    lockPtr <- alloca (ptr i8) Nothing 8
    modify $ \s -> s { metadataStorage = Data.Map.insert p (basePtr, boundPtr, keyPtr, lockPtr) $ metadataStorage s }
    return (basePtr, boundPtr, keyPtr, lockPtr)
  else do
    fname <- gets (unpack . PP.ppll . name . fromJust . currentFunction)
    pp <- liftM (unpack . PP.render) $ PP.ppOperand p
    error $ "allocateLocalMetadataStorage: in function " ++ fname ++ ": storage already allocated for metadata for pointer " ++ pp

-- | Look up the local variables allocated to hold metadata for the given pointer
getLocalMetadataStorage :: (HasCallStack, MonadState SBCETSState m, MonadIRBuilder m, MonadModuleBuilder m) => Operand -> m Metadata
getLocalMetadataStorage p = do
  allocated <- gets ((Data.Map.lookup p) . metadataStorage)
  if isJust allocated
  then gets ((! p) . metadataStorage)
  else do
    pp <- liftM (unpack . PP.render) $ PP.ppOperand p
    error $ "getLocalMetadataStorage: no storage allocated for metadata for pointer " ++ pp

-- | Identify the instructions in the given basic block which require local variables to be allocated to hold metadata.
--   Only LocalReference pointers require local variables allocated to hold metadata. The metadata for global references
--   is held in global variables, and the metadata for constant expressions of pointer type is computed as more constant
--   expressions, and so doesn't require storage. Returns a list of pointer 'Operand's requiring local metadata storage.
identifyLocalMetadataAllocations :: (HasCallStack, MonadState SBCETSState m, MonadWriter [String] m, MonadModuleBuilder m) => BasicBlock -> m [Operand]
identifyLocalMetadataAllocations (BasicBlock _ i t) = do
  isites <- liftM concat $ mapM instAllocations i
  tsites <- termAllocations t
  return (nub $ sort (isites ++ tsites))
  where
    instAllocations inst
      -- Case 1: If a pointer is loaded from memory, local metadata is allocated for the pointer's metadata.
      | (v := o) <- inst, (Load _ addr _ _ _) <- o = do
          enable <- gets (CLI.instrumentLoad . options)
          if enable
          then do
            ta <- typeOf addr
            if (Helpers.isPointerType $ pointerReferent ta) &&
               (not $ Helpers.isFunctionType $ pointerReferent $ pointerReferent ta)
            then return [LocalReference (pointerReferent ta) v]
            else return []
          else return []
      -- Case 2: If a function is called and LocalReference pointer arguments are passed, the metadata for those pointer arguments must be available in local variables.
      -- Additionally, if a pointer is returned, local variables must be allocated to hold the metadata for that pointer.
      | (v := o) <- inst, (Call _ _ _ (Right (ConstantOperand (Const.GlobalReference (PointerType (FunctionType rt _ False) _) fname@(Name {})))) opds _ _) <- o = do
          enable <- gets (CLI.instrumentCall . options)
          ignore <- isIgnoredFunction fname
          if (enable && not ignore)
          then do
            let lrArgs = filter Helpers.isLocalReference $ map fst opds
            let ptrRet = if (Helpers.isPointerType rt) then [LocalReference rt v] else []
            argTys <- mapM typeOf lrArgs
            let ptrArgs = map snd $
                          filter (not . Helpers.isFunctionType . pointerReferent . fst) $
                          filter (Helpers.isPointerType . fst) $
                          zip argTys lrArgs
            return (ptrArgs ++ ptrRet)
          else return []
      -- Case 3: Local metadata must be available for any LocalReference incoming values to a phi instruction of pointer type.
      | (_ := o) <- inst, (Phi (PointerType ty _) incoming _) <- o = do
          if (not $ Helpers.isFunctionType ty)
          then return (map fst $ filter (Helpers.isLocalReference . fst) incoming)
          else return []
      -- Case 4: If we allocate anything on the stack, we get a pointer to it, which needs metadata.
      | (v := o) <- inst, (Alloca ty _ _ _) <- o = do
          enable <- gets (CLI.instrumentStack . options)
          if (enable && (not $ Helpers.isFunctionType ty)) then return [LocalReference (ptr ty) v] else return []
      -- Case 5: Case 2 for void functions (no possibility of returning a pointer here).
      | (Do o) <- inst, (Call _ _ _ (Right (ConstantOperand (Const.GlobalReference (PointerType (FunctionType _ _ False) _) fname@(Name {})))) opds _ _) <- o = do
          enable <- gets (CLI.instrumentCall . options)
          ignore <- isIgnoredFunction fname
          if (enable && not ignore)
          then do
            let lrArgs = filter Helpers.isLocalReference $ map fst opds
            argTys <- mapM typeOf lrArgs
            let ptrArgs = map snd $
                          filter (not . Helpers.isFunctionType . pointerReferent . fst) $
                          filter (Helpers.isPointerType . fst) $
                          zip argTys lrArgs
            return ptrArgs
          else return []
      | otherwise = return []

    termAllocations term
      -- Case 6: If we return a pointer, we need to push the pointer's metadata to the shadow stack, so (for LocalReference pointers) it must be available in local variables.
      | (Do (Ret (Just x) _)) <- term = do
          tx <- typeOf x
          if (Helpers.isLocalReference x) &&
             (Helpers.isPointerType tx) &&
             (not $ Helpers.isFunctionType $ pointerReferent tx)
          then return [x]
          else return []
      | otherwise = return []

-- | Emit the declaration of a runtime API function.
emitRuntimeAPIFunctionDecl :: (HasCallStack, MonadModuleBuilder m) => (Name, Type) -> m ()
emitRuntimeAPIFunctionDecl decl
  | (fname, (FunctionType retType argTypes _)) <- decl = do
      _ <- extern fname argTypes retType
      return ()
  | otherwise = undefined

-- | Emit a call to a runtime API function.
emitRuntimeAPIFunctionCall :: (HasCallStack, MonadIRBuilder m, MonadState SBCETSState m, MonadModuleBuilder m) => String -> [Operand] -> m Operand
emitRuntimeAPIFunctionCall n args = do
  (fname, fproto) <- gets ((!! (mkName n)) . runtimeFunctionPrototypes)
  call (ConstantOperand $ Const.GlobalReference (ptr fproto) fname) $ map (\x -> (x, [])) args

-- | Load the metadata for the given address.
emitRuntimeMetadataLoad :: (HasCallStack, MonadState SBCETSState m, MonadWriter [String] m, MonadIRBuilder m, MonadModuleBuilder m) => Operand -> m Metadata
emitRuntimeMetadataLoad addr
  | (LocalReference (PointerType _ _) _) <- addr = do
      allocated <- gets (Data.Map.member addr . metadataStorage)
      if not allocated
      then do
        pAddr <- liftM (unpack . PP.render) $ PP.ppOperand addr
        tell ["emitRuntimeMetadataLoad: using don't-care metadata for uninstrumented pointer " ++ pAddr]
        gets (fromJust . dontCareMetadata)
      else do
        addr' <- bitcast addr (ptr i8)
        (basePtr, boundPtr, keyPtr, lockPtr) <- getLocalMetadataStorage addr
        _ <- emitRuntimeAPIFunctionCall "__softboundcets_metadata_load" [addr', basePtr, boundPtr, keyPtr, lockPtr]
        emitCheck <- gets (CLI.emitChecks . options)
        when emitCheck $ do
          _ <- emitRuntimeAPIFunctionCall "__softboundcets_metadata_check" [basePtr, boundPtr, keyPtr, lockPtr]
          return ()
        return (basePtr, boundPtr, keyPtr, lockPtr)
  | (ConstantOperand {}) <- addr = do
      -- TODO-IMPROVE: If asked to load the metadata for a constant pointer expression or global variable, we currently just return the don't-care metadata.
      -- I believe we can just call __softboundcets_metadata_load here but we need to make sure that we are actually setting up the metadata
      -- storage for global variables properly (in 'instrumentGlobalVariable' below) first.
      pAddr <- liftM (unpack . PP.render) $ PP.ppOperand addr
      tell ["emitRuntimeMetadataLoad: using don't-care metadata for uninstrumented pointer " ++ pAddr]
      gets (fromJust . dontCareMetadata)
  | otherwise = do
      pAddr <- liftM (unpack . PP.render) $ PP.ppOperand addr
      error $ "emitRuntimeMetadataLoad: expected pointer but saw non-pointer " ++ pAddr

-- | Create a local key and lock for entities allocated in the current stack frame
emitLocalKeyAndLockCreation :: (HasCallStack, MonadState SBCETSState m, MonadIRBuilder m, MonadModuleBuilder m) => m ()
emitLocalKeyAndLockCreation = do
  keyPtr <- alloca i64 Nothing 8
  lockPtr <- alloca (ptr i8) Nothing 8
  _ <- emitRuntimeAPIFunctionCall "__softboundcets_create_stack_key" [lockPtr, keyPtr]
  modify $ \s -> s { localStackFrameKeyPtr = Just keyPtr, localStackFrameLockPtr = Just lockPtr }
  return ()

-- | Invalidate the local key; We do this just prior to returning from a function.
emitLocalKeyAndLockDestruction :: (HasCallStack, MonadState SBCETSState m, MonadIRBuilder m, MonadModuleBuilder m) => m ()
emitLocalKeyAndLockDestruction = do
  keyPtr <- gets (fromJust . localStackFrameKeyPtr)
  key <- load keyPtr 0
  _ <- emitRuntimeAPIFunctionCall "__softboundcets_destroy_stack_key" [key]
  return ()

-- | Allocate space on the shadow stack for the parameters of an instrumented function we are about to call.
emitShadowStackAllocation :: (HasCallStack, MonadState SBCETSState m, MonadIRBuilder m, MonadModuleBuilder m) => Integer -> m ()
emitShadowStackAllocation numArgs = do
  numArgs' <- pure $ int32 numArgs
  _ <- emitRuntimeAPIFunctionCall "__softboundcets_allocate_shadow_stack_space" [numArgs']
  return ()

-- | Deallocate the shadow stack space for the instrumented function which just returned.
emitShadowStackDeallocation :: (HasCallStack, MonadState SBCETSState m, MonadIRBuilder m, MonadModuleBuilder m) => m ()
emitShadowStackDeallocation = do
  _ <- emitRuntimeAPIFunctionCall "__softboundcets_deallocate_shadow_stack_space" []
  return ()

-- | Store the metadata for the given pointer into the local variables allocated to hold it.
emitMetadataStoreToLocalVariables :: (HasCallStack, MonadState SBCETSState m, MonadIRBuilder m, MonadModuleBuilder m) => Operand -> Metadata -> m Metadata
emitMetadataStoreToLocalVariables p (base, bound, key, lock) = do
  meta@(basePtr, boundPtr, keyPtr, lockPtr) <- getLocalMetadataStorage p
  store basePtr 8 base
  store boundPtr 8 bound
  store keyPtr 8 key
  store lockPtr 8 lock
  return meta

-- | Load the metadata for a pointer function parameter from the shadow stack.
emitMetadataLoadFromShadowStack :: (HasCallStack, MonadState SBCETSState m, MonadIRBuilder m, MonadModuleBuilder m) => Operand -> Integer -> m Metadata
emitMetadataLoadFromShadowStack p ix = do
  ix' <- pure $ int32 ix
  base <- emitRuntimeAPIFunctionCall "__softboundcets_load_base_shadow_stack" [ix']
  bound <- emitRuntimeAPIFunctionCall "__softboundcets_load_bound_shadow_stack" [ix']
  key <- emitRuntimeAPIFunctionCall "__softboundcets_load_key_shadow_stack" [ix']
  lock <- emitRuntimeAPIFunctionCall "__softboundcets_load_lock_shadow_stack" [ix']
  emitMetadataStoreToLocalVariables p (base, bound, key, lock)

-- | Store the metadata for a pointer on the shadow stack at the specified position.
emitMetadataStoreToShadowStack :: (HasCallStack, MonadModuleBuilder m, MonadState SBCETSState m, MonadWriter [String] m, MonadIRBuilder m) => Maybe Name -> Operand -> Integer -> m ()
emitMetadataStoreToShadowStack callee p ix = do
  meta <- inspectPointer p
  case meta of
    (Just (_, (basePtr, boundPtr, keyPtr, lockPtr))) -> do
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
    Nothing -> do
      fname <- gets (unpack . PP.ppll . name . fromJust . currentFunction)
      pp <- liftM (unpack . PP.render) $ PP.ppOperand p
      tell ["emitMetadataStoreToShadowStack: in function " ++ fname ++ ": using don't-care metadata for unsupported pointer " ++ pp ++ " passed to function " ++ (unpack $ PP.ppll $ fromJust callee)]
      (basePtr, boundPtr, keyPtr, lockPtr) <- gets (fromJust . dontCareMetadata)
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

-- | Instrument a given module according to the supplied command-line options and list of blacklisted function symbols.
instrument :: HasCallStack => [String] -> CLI.Options -> Module -> IO Module
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
      -- We must handle type definitions first, otherwise we won't be able to resolve named type references
      let (typeDefs, nonTypeDefs) = partition Helpers.isTypeDef $ moduleDefinitions m'
      mapM_ (\(TypeDefinition n t) -> typedef n t) typeDefs
      mapM_ instrumentDefinition nonTypeDefs
      return ()

    instrumentDefinition g
      -- Don't instrument empty functions (i.e. forward declarations)
      | (GlobalDefinition f@(Function {})) <- g, null $ basicBlocks f = emitDefn g
      -- TODO-IMPROVE: Softboundcets does not instrument varargs functions
      | (GlobalDefinition f@(Function {})) <- g, snd $ parameters f = emitDefn g
      | (GlobalDefinition f@(Function {})) <- g = do
          hasWrapper <- isWrappedFunction $ name f
          ignore <- isIgnoredFunction (name f)
          if (not ignore && not hasWrapper) || name f == mkName "main" then do
            instrumentFunction f
          else emitDefn g
      | (GlobalDefinition gv@(GlobalVariable {})) <- g = do
          emitDefn g
          instrumentGlobalVariable gv
      | otherwise = emitDefn g

    instrumentGlobalVariable g
      | (GlobalVariable {}) <- g, name g == (Name $ fromString "llvm.global_ctors") = return () -- https://llvm.org/docs/LangRef.html#the-llvm-global-ctors-global-variable
      | (GlobalVariable {}) <- g, name g == (Name $ fromString "llvm.global_dtors") = return () -- https://llvm.org/docs/LangRef.html#the-llvm-global-dtors-global-variable
      | (GlobalVariable {}) <- g, section g == (Just $ fromString "llvm.metadata")  = return () -- LLVM puts metadata in a specially named section
      | (GlobalVariable {}) <- g, isNothing $ initializer g                         = return () -- Uninitialized globals do not get metadata
      | (GlobalVariable {}) <- g = do
          let gName = name g
          gType' <- typeOf (fromJust $ initializer g)
          let gType = ptr gType'
          -- The address of a global variable is always safe
          markPointer (ConstantOperand $ Const.GlobalReference gType gName) Safe
          -- TODO-IMPROVE: right now all pointers to globals get don't-care metadata from 'inspectPointer' so we can skip creating metadata here for performance testing.
      | otherwise = do
        pg <- liftM (unpack . PP.render) $ PP.ppGlobal g
        error $ "instrumentGlobalVariable: expected global variable, but got: " ++ pg

    instrumentFunction f
      | (Function {}) <- f = do
          let name' = if name f == mkName "main" then mkName "softboundcets_main" else name f
          (_, blocks) <- runIRBuilderT emptyIRBuilder { builderNameSuggestion = Just $ fromString "sbcets" } $ do
            classifications' <- gets classifications
            modify $ \s -> s { globalLockPtr = Nothing
                             , localStackFrameKeyPtr = Nothing
                             , localStackFrameLockPtr = Nothing
                             , currentFunction = Just f
                             , dontCareMetadata = Nothing
                             , metadataStorage = Data.Map.empty
                             }
            let firstBlockLabel = (\(BasicBlock n _ _) -> n) $ head $ basicBlocks f
            -- Create the metadata for any non-function type pointer parameters
            let params = fst $ parameters f
            paramTys <- mapM typeOf params
            let pointerArguments = map (\(_, Parameter t n _) -> (LocalReference t n)) $
                                   filter (not . Helpers.isFunctionType . pointerReferent . fst) $
                                   filter (Helpers.isPointerType . fst) $
                                   zip paramTys params
            let shadowStackIndices :: [Integer] = [1..]
            emitBlockStart (mkName "sbcets_metadata_init")
            mapM_ allocateLocalMetadataStorage pointerArguments
            _ <- zipWithM emitMetadataLoadFromShadowStack pointerArguments shadowStackIndices
            -- Create the don't-care metadata.
            nullPtr <- inttoptr (int64 0) (ptr i8)
            dcMetadata <- allocateLocalMetadataStorage nullPtr
            _ <- emitRuntimeMetadataLoad nullPtr
            modify $ \s -> s { dontCareMetadata = Just dcMetadata }
            -- Collect all metadata allocation sites so we can allocate local variables for metadata ahead of time
            pointersRequiringLocalMetadata <- liftM (nub . sort . concat) $ mapM identifyLocalMetadataAllocations $ basicBlocks f
            mapM_ allocateLocalMetadataStorage $ filter (not . flip elem pointerArguments) pointersRequiringLocalMetadata
            emitTerm $ Br firstBlockLabel []
            -- Traverse and instrument the basic blocks
            instrumentBlocks $ basicBlocks f
            modify $ \s -> s { classifications = classifications' }
          emitDefn $ GlobalDefinition $ f { name = name', basicBlocks = blocks }
          return ()
      | otherwise = undefined

    instrumentBlocks bs
      | [] <- bs = return ()
      | (first:[]) <- bs = instrumentFirstBlock first
      | (first:blocks) <- bs = do
          instrumentFirstBlock first
          mapM_ instrumentBlock blocks

    instrumentFirstBlock (BasicBlock n i t) = do
      emitBlockStart n
      -- Set up a handle to the global lock
      glp <- emitRuntimeAPIFunctionCall "__softboundcets_get_global_lock" []
      modify $ \s -> s { globalLockPtr = Just glp }
      -- Create a lock for local allocations
      emitLocalKeyAndLockCreation
      mapM_ instrumentInst i
      instrumentTerm t

    instrumentBlock (BasicBlock n i t) = do
      emitBlockStart n
      mapM_ instrumentInst i
      instrumentTerm t

    instrumentInst i@(v := o)
      | (Alloca ty count _ _) <- o = do
        -- We emit the alloca first because we reference the result in the instrumentation
        Helpers.emitNamedInst i
        when (not $ Helpers.isFunctionType ty) $ do -- Don't instrument function pointers
          let resultPtr = LocalReference (ptr ty) v
            -- The address of a stack allocation is always safe
          markPointer resultPtr Safe
          enable <- gets (CLI.instrumentStack . options)
          when enable $ do
            eltSize <- sizeof 64 ty
            intCount <- if isJust count
                        then do
                          tc <- typeOf $ fromJust count
                          if not (tc == i64)
                          then sext (fromJust count) i64
                          else pure $ fromJust count
                        else pure $ ConstantOperand $ Const.Int 64 1
            allocSize <- mul eltSize intCount
            base <- bitcast resultPtr (ptr i8)
            intBase <- ptrtoint base i64
            intBound <- add allocSize intBase
            bound <- inttoptr intBound (ptr i8)
            functionKeyPtr <- gets (fromJust . localStackFrameKeyPtr)
            functionLockPtr <- gets (fromJust . localStackFrameLockPtr)
            functionKey <- load functionKeyPtr 0
            functionLock <- load functionLockPtr 0
            _ <- emitMetadataStoreToLocalVariables resultPtr (base, bound, functionKey, functionLock)
            return ()

      | (Load _ addr _ _ _) <- o = do
        enable <- gets (CLI.instrumentLoad . options)
        when enable $ do
          addrClass <- pointerClass addr
          case addrClass of
            Unsafe -> do
              meta <- inspectPointer addr
              (ty, (basePtr, boundPtr, keyPtr, lockPtr)) <- case meta of
                (Just (ty, meta')) -> return (ty, meta')
                Nothing -> do
                  pAddr <- liftM (unpack . PP.render) $ PP.ppOperand addr
                  pFunc <- gets (unpack . PP.ppll . name . fromJust . currentFunction)
                  pInst <- liftM (unpack . PP.render) $ PP.ppNamed PP.ppInstruction i
                  tell ["in function " ++ pFunc ++ ": using don't-care metadata for uninstrumented pointer " ++ pAddr ++ " in " ++ pInst]
                  ty <- liftM pointerReferent $ typeOf addr
                  dc <- gets (fromJust . dontCareMetadata)
                  return (ty, dc)
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
            Safe -> return ()

          Helpers.emitNamedInst i
          -- No matter if we were able to instrument the load or not, if a pointer was loaded, ask the runtime for metadata for the loaded address.
          ta <- typeOf addr
          when ((Helpers.isPointerType $ pointerReferent ta) &&
                (not $ Helpers.isFunctionType $ pointerReferent $ pointerReferent ta)) $ do
            _ <- emitRuntimeMetadataLoad addr
            return ()
          return ()

      -- Instrument a call instruction unless it is calling inline assembly or a computed function pointer.
      | (Call _ _ _ (Right (ConstantOperand (Const.GlobalReference (PointerType (FunctionType rt _ False) _) fname))) opds _ _) <- o = do
        enable <- gets (CLI.instrumentCall . options)
        ignore <- isIgnoredFunction fname
        if (not enable || ignore)
        then Helpers.emitNamedInst i
        else do
          case fname of
            (Name {}) -> do -- Calling a function symbol
              let opds' = map fst opds
              opdTys <- mapM typeOf opds'
              let ptrArgs = map snd $
                            filter (not . Helpers.isFunctionType . pointerReferent . fst) $
                            filter (Helpers.isPointerType . fst) $
                            zip opdTys opds'
              emitShadowStackAllocation (fromIntegral $ 1 + length ptrArgs)
              zipWithM_ (emitMetadataStoreToShadowStack $ Just fname) ptrArgs [1..]
              hasWrapper <- isWrappedFunction fname
              if hasWrapper
              then do
                wrapperFunctionName <- gets (fst . (! fname) . stdlibWrapperPrototypes)
                Helpers.emitNamedInst $ v := (Helpers.rewriteCalledFunctionName wrapperFunctionName o)
              else Helpers.emitNamedInst i
              -- The function could potentially deallocate any pointer it is passed
              mapM_ (flip markPointer $ Unsafe) $ Data.Set.fromList ptrArgs
              -- Read the metadata for the return value off the shadow stack if it is a pointer
              when (Helpers.isPointerType rt && (not $ Helpers.isFunctionType $ pointerReferent rt)) $ do
                _ <- emitMetadataLoadFromShadowStack (LocalReference rt v) 0
                safe <- returnsSafePointer fname
                if safe
                then markPointer (LocalReference rt v) Safe
                else markPointer (LocalReference rt v) Unsafe
              emitShadowStackDeallocation
            (UnName {}) -> do -- TODO-IMPROVE: Calling a computed function pointer. Can we map this to a function symbol?
              Helpers.emitNamedInst i

      | (GetElementPtr _ addr ixs _) <- o = do
        meta <- inspectPointer addr
        (_, meta') <- case meta of
          (Just (ty, meta')) -> return (ty, meta')
          Nothing -> do
            pAddr <- liftM (unpack . PP.render) $ PP.ppOperand addr
            pFunc <- gets (unpack . PP.ppll . name . fromJust . currentFunction)
            pInst <- liftM (unpack . PP.render) $ PP.ppNamed PP.ppInstruction i
            tell ["in function " ++ pFunc ++ ": using don't-care metadata for uninstrumented pointer " ++ pAddr ++ " in " ++ pInst]
            ty <- liftM pointerReferent $ typeOf addr
            dc <- gets (fromJust . dontCareMetadata)
            return (ty, dc)
        ta <- typeOf addr
        ty' <- Helpers.typeIndex ta ixs
        if (isJust ty')
        then do
          let gepResultPtr = LocalReference (ptr $ fromJust ty') v
          -- The pointer created by getelementptr shares metadata storage with the parent pointer
          modify $ \s -> s { metadataStorage = Data.Map.insert gepResultPtr meta' $ metadataStorage s }
          -- TODO-OPTIMIZE: arithmetic derived pointers are considered unconditionally unsafe (even if the parent pointer is safe)
          markPointer gepResultPtr Unsafe
        else do
          -- TODO-IMPROVE: Softboundcets doesn't handle opaque structure types (https://llvm.org/docs/LangRef.html#opaque-structure-types) but we could do so.
          pAddr <- liftM (unpack . PP.render) $ PP.ppOperand addr
          pIxs <- mapM (liftM (unpack . PP.render) . PP.ppOperand) ixs
          tell ["Unable to compute type of getelementptr result: " ++ (unpack $ PP.ppll ta) ++ " " ++ pAddr ++ " [" ++ (intercalate ", " pIxs) ++ "]"]
          return ()
        Helpers.emitNamedInst i

      | (BitCast addr pty@(PointerType {}) _) <- o = do
        enable <- gets (CLI.instrumentBitcast . options)
        when (enable && (not $ Helpers.isFunctionType $ pointerReferent pty)) $ do
          meta <- inspectPointer addr
          (_, meta') <- case meta of
            (Just x) -> return x
            Nothing -> do
              pAddr <- liftM (unpack . PP.render) $ PP.ppOperand addr
              pFunc <- gets (unpack . PP.ppll . name . fromJust . currentFunction)
              pInst <- liftM (unpack . PP.render) $ PP.ppNamed PP.ppInstruction i
              tell ["in function " ++ pFunc ++ ": using don't-care metadata for uninstrumented pointer " ++ pAddr ++ " in " ++ pInst]
              ty' <- liftM pointerReferent $ typeOf addr
              dc <- gets (fromJust . dontCareMetadata)
              return (ty', dc)
          let bitcastResultPtr = LocalReference pty v
          -- The pointer created by bitcast shares metadata storage with the parent pointer
          modify $ \s -> s { metadataStorage = Data.Map.insert bitcastResultPtr meta' $ metadataStorage s }
          -- TODO-OPTIMIZE: cast pointers are considered unconditionally unsafe (even if the parent pointer is safe)
          markPointer bitcastResultPtr Unsafe
        Helpers.emitNamedInst i

      | (Select cond tval fval _) <- o = do
        Helpers.emitNamedInst i
        valTy <- typeOf tval
        when (Helpers.isPointerType valTy && (not $ Helpers.isFunctionType $ pointerReferent valTy)) $ do
          tClass <- pointerClass tval
          tMeta <- inspectPointer tval
          fClass <- pointerClass fval
          fMeta <- inspectPointer fval
          ((ty, tMeta'), (_, fMeta')) <- case (tMeta, fMeta) of
            (Just (tyA, tMeta'), Just (tyB, fMeta')) -> return ((tyA, tMeta'), (tyB, fMeta'))
            (Just (tyA, tMeta'), Nothing) -> do
              pAddr <- liftM (unpack . PP.render) $ PP.ppOperand fval
              pFunc <- gets (unpack . PP.ppll . name . fromJust . currentFunction)
              pInst <- liftM (unpack . PP.render) $ PP.ppNamed PP.ppInstruction i
              tell ["in function " ++ pFunc ++ ": using don't-care metadata for uninstrumented pointer " ++ pAddr ++ " in " ++ pInst]
              tyB <- liftM pointerReferent $ typeOf fval
              fMeta' <- gets (fromJust . dontCareMetadata)
              return ((tyA, tMeta'), (tyB, fMeta'))
            (Nothing, Just (tyB, fMeta')) -> do
              pAddr <- liftM (unpack . PP.render) $ PP.ppOperand tval
              pFunc <- gets (unpack . PP.ppll . name . fromJust . currentFunction)
              pInst <- liftM (unpack . PP.render) $ PP.ppNamed PP.ppInstruction i
              tell ["in function " ++ pFunc ++ ": using don't-care metadata for uninstrumented pointer " ++ pAddr ++ " in " ++ pInst]
              tyA <- liftM pointerReferent $ typeOf tval
              tMeta' <- gets (fromJust . dontCareMetadata)
              return ((tyA, tMeta'), (tyB, fMeta'))
            (Nothing, Nothing) -> do
              pAddr <- liftM (unpack . PP.render) $ PP.ppOperand tval
              pFunc <- gets (unpack . PP.ppll . name . fromJust . currentFunction)
              pInst <- liftM (unpack . PP.render) $ PP.ppNamed PP.ppInstruction i
              tell ["in function " ++ pFunc ++ ": using don't-care metadata for uninstrumented pointer " ++ pAddr ++ " in " ++ pInst]
              tyA <- liftM pointerReferent $ typeOf tval
              tMeta' <- gets (fromJust . dontCareMetadata)
              pAddr' <- liftM (unpack . PP.render) $ PP.ppOperand fval
              tell ["in function " ++ pFunc ++ ": using don't-care metadata for uninstrumented pointer " ++ pAddr' ++ " in " ++ pInst]
              tyB <- liftM pointerReferent $ typeOf fval
              fMeta' <- gets (fromJust . dontCareMetadata)
              return ((tyA, tMeta'), (tyB, fMeta'))
          basePtr <- select cond (getBase tMeta') (getBase fMeta')
          boundPtr <- select cond (getBound tMeta') (getBound fMeta')
          keyPtr <- select cond (getKey tMeta') (getKey fMeta')
          lockPtr <- select cond (getLock tMeta') (getLock fMeta')
          let newPtr = LocalReference (ptr ty) v
          let newMeta = (basePtr, boundPtr, keyPtr, lockPtr)
          case (tClass, fClass) of
            (Safe, Safe) -> markPointer newPtr Safe
            _ -> markPointer newPtr Unsafe
          -- The pointer created by select shares metadata storage with the parent pointer
          modify $ \s -> s { metadataStorage = Data.Map.insert newPtr newMeta $ metadataStorage s }

      | (Phi (PointerType ty _) incoming _) <- o = do
        Helpers.emitNamedInst i

        let phiMeta f (p, n) = do meta <- inspectPointer p
                                  case meta of
                                    (Just (_, meta')) -> return (f meta', n)
                                    Nothing -> do
                                      pAddr <- liftM (unpack . PP.render) $ PP.ppOperand p
                                      pFunc <- gets (unpack . PP.ppll . name . fromJust . currentFunction)
                                      pInst <- liftM (unpack . PP.render) $ PP.ppNamed PP.ppInstruction i
                                      tell ["in function " ++ pFunc ++ ": using don't-care metadata for uninstrumented pointer " ++ pAddr ++ " in " ++ pInst]
                                      meta' <- gets (fromJust . dontCareMetadata)
                                      return (f meta', n)

        when (not $ Helpers.isFunctionType ty) $ do
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
          -- The pointer created by phi shares metadata storage with the parent pointer
          modify $ \s -> s { metadataStorage = Data.Map.insert newPtr newMeta $ metadataStorage s }

      | otherwise = Helpers.emitNamedInst i

    instrumentInst i@(Do o)
      -- This alternative is the non-capturing variant (call ignoring return value, if any).
      -- We don't need to emit checks for the return value here because it is unused.
      | (Call _ _ _ (Right (ConstantOperand (Const.GlobalReference (PointerType (FunctionType _ _ False) _) fname))) opds _ _) <- o = do
        enable <- gets (CLI.instrumentCall . options)
        ignore <- isIgnoredFunction fname
        if (not enable || ignore)
        then Helpers.emitNamedInst i
        else do
          case fname of
            (Name {}) -> do -- Calling a function symbol
              let opds' = map fst opds
              opdTys <- mapM typeOf opds'
              let ptrArgs = map snd $
                            filter (not . Helpers.isFunctionType . pointerReferent . fst) $
                            filter (Helpers.isPointerType . fst) $
                            zip opdTys opds'
              emitShadowStackAllocation (fromIntegral $ 1 + length ptrArgs)
              zipWithM_ (emitMetadataStoreToShadowStack $ Just fname) ptrArgs [1..]
              hasWrapper <- isWrappedFunction fname
              if hasWrapper
              then do
                wrapperFunctionName <- gets (fst . (! fname) . stdlibWrapperPrototypes)
                Helpers.emitNamedInst $ Do $ Helpers.rewriteCalledFunctionName wrapperFunctionName o
              else Helpers.emitNamedInst i
              -- The function could potentially deallocate any pointer it is passed
              mapM_ (flip markPointer $ Unsafe) $ Data.Set.fromList ptrArgs
              emitShadowStackDeallocation
            (UnName {}) -> do -- TODO-IMPROVE: Calling a computed function pointer. Can we map this to a function symbol?
              Helpers.emitNamedInst i

      | (Store _ tgt src _ _ _) <- o = do
        enable <- gets (CLI.instrumentStore . options)
        when enable $ do
          tgtClass <- pointerClass tgt
          case tgtClass of
            Unsafe -> do
              tgtMeta <- inspectPointer tgt
              (ty, (basePtr, boundPtr, keyPtr, lockPtr)) <- case tgtMeta of
                (Just (ty, meta)) -> return (ty, meta)
                Nothing -> do
                  pAddr <- liftM (unpack . PP.render) $ PP.ppOperand tgt
                  pFunc <- gets (unpack . PP.ppll . name . fromJust . currentFunction)
                  pInst <- liftM (unpack . PP.render) $ PP.ppNamed PP.ppInstruction i
                  tell ["in function " ++ pFunc ++ ": using don't-care metadata for uninstrumented pointer " ++ pAddr ++ " in " ++ pInst]
                  ty <- liftM pointerReferent $ typeOf tgt
                  meta <- gets (fromJust . dontCareMetadata)
                  return (ty, meta)
              tgtBase <- load basePtr 0
              tgtBound <- load boundPtr 0
              tgtAddr <- bitcast tgt (ptr i8)
              tySize <- sizeof 64 ty
              -- Check the store is spatially in bounds
              _ <- emitRuntimeAPIFunctionCall "__softboundcets_spatial_store_dereference_check" [tgtBase, tgtBound, tgtAddr, tySize]
              -- Check the store is temporally in bounds
              tgtKey <- load keyPtr 0
              tgtLock <- load lockPtr 0
              _ <- emitRuntimeAPIFunctionCall "__softboundcets_temporal_store_dereference_check" [tgtLock, tgtKey]
              return ()
            Safe -> return ()

        Helpers.emitNamedInst i

        when enable $ do
          ty <- typeOf src
          when (Helpers.isPointerType ty && (not $ Helpers.isFunctionType $ pointerReferent ty)) $ do
            srcMeta <- inspectPointer src
            (_, (basePtr, boundPtr, keyPtr, lockPtr)) <- case srcMeta of
              (Just (ty', meta)) -> return (ty', meta)
              Nothing -> do
                pAddr <- liftM (unpack . PP.render) $ PP.ppOperand src
                pFunc <- gets (unpack . PP.ppll . name . fromJust . currentFunction)
                pInst <- liftM (unpack . PP.render) $ PP.ppNamed PP.ppInstruction i
                tell ["in function " ++ pFunc ++ ": using don't-care metadata for uninstrumented pointer " ++ pAddr ++ " in " ++ pInst]
                ty' <- liftM pointerReferent $ typeOf src
                meta <- gets (fromJust . dontCareMetadata)
                return (ty', meta)
            tgtAddr <- bitcast tgt (ptr i8)
            srcBase <- load basePtr 0
            srcBound <- load boundPtr 0
            srcKey <- load keyPtr 0
            srcLock <- load lockPtr 0
            -- Write the metadata for the stored pointer to the runtime
            _ <- emitRuntimeAPIFunctionCall "__softboundcets_metadata_store" [tgtAddr, srcBase, srcBound, srcKey, srcLock]
            return ()

      | otherwise = Helpers.emitNamedInst i

    instrumentTerm i
      | (Do (Ret (Just op) _)) <- i = do
          ty <- typeOf op
          when (Helpers.isPointerType ty && (not $ Helpers.isFunctionType $ pointerReferent ty)) $ do
            -- If we are returning a pointer, put the metadata on the shadow stack
            emitMetadataStoreToShadowStack Nothing op 0
          -- Invalidate the key for this function's local allocations
          emitLocalKeyAndLockDestruction
          Helpers.emitNamedTerm i
      | (Do (Ret _ _)) <- i = do
          -- Returning a non-pointer, just invalidate the key for this function's local allocations
          emitLocalKeyAndLockDestruction
          Helpers.emitNamedTerm i
      -- Not a return instruction, don't instrument
      | otherwise = Helpers.emitNamedTerm i
