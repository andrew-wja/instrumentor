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
import LLVM.AST hiding (args, index, Metadata)
import LLVM.AST.Global
import LLVM.AST.Type
import LLVM.AST.Typed (typeOf)
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

-- | Metadata is a 4-tuple: the base address, bound address, key value, and lock
--   address associated with some user pointer. Metadata is held in different
--   ways: local variable metadata is held in local variables, while metadata
--   for global variables is held in global variables. Metadata for constant
--   pointers is not really held at all, but computed as constant expressions.

data Metadata = Local    { base :: Operand
                         , bound :: Operand
                         , key :: Operand
                         , lock :: Operand
                         }
              | Constant { base :: Operand
                         , bound :: Operand
                         , key :: Operand
                         , lock :: Operand
                         }

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
                               , nullMetadata :: Maybe Metadata
                               -- ^ Metadata for null pointers.
                               , localStorage :: Map Operand Metadata
                               -- ^ A 'Map' from the SSA register names of pointers in the current function to their metadata.
                               }

-- | Create an empty 'SBCETSState'
emptySBCETSState :: SBCETSState
emptySBCETSState = SBCETSState Nothing Nothing Nothing
                               Data.Set.empty
                               Data.Map.empty Data.Map.empty
                               CLI.defaultOptions Nothing
                               Data.Map.empty Data.Set.empty
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

-- | Mark a pointer as belonging to a particular 'PointerClass'.
mark :: MonadState SBCETSState m => Operand -> PointerClass -> m ()
mark p c = modify $ \s -> s { classifications = Data.Map.insert p c $ classifications s }

-- | Query the 'PointerClass' of a pointer, defaulting to the given 'PointerClass' if the pointer is not tracked.
query :: MonadState SBCETSState m => PointerClass-> Operand -> m PointerClass
query d p = gets (maybe d id . (Data.Map.lookup p . classifications))

-- | Associate local metadata storage with the given pointer.
associate :: MonadState SBCETSState m => Operand -> Metadata -> m ()
associate p m = modify $ \s -> s { localStorage = Data.Map.insert p m $ localStorage s }

-- | 'inspect' traverses pointer-type expressions and returns the metadata.
inspect :: (HasCallStack, MonadState SBCETSState m, MonadWriter [String] m, MonadModuleBuilder m) => Operand -> m (Maybe (Type, Metadata))
inspect p
  | (LocalReference (PointerType ty _) _) <- p = do
      if Helpers.isFunctionType ty
      then return Nothing -- TODO-IMPROVE: Function pointers currently get don't-care metadata but they are supposed to be checked for spatial safety at callsites.
      else do
        loc <- gets (Data.Map.member p . localStorage)
        if loc
        then gets (Just . (ty,) . (! p) . localStorage)
        else do
          pp <- pure $ show p
          fname <- gets (name . fromJust . currentFunction)
          tell ["inspect: in function " ++ (show fname) ++ ": no storage allocated for metadata for pointer " ++ pp]
          return Nothing

  | (ConstantOperand (Const.Null (PointerType ty _))) <- p = gets (Just . (ty,) . fromJust . nullMetadata)
  | (ConstantOperand (Const.Undef (PointerType ty _))) <- p = gets (Just . (ty,) . fromJust . nullMetadata)

  -- TODO-IMPROVE: Some constant expressions of pointer type and global pointers currently just get the don't-care metadata.
  -- This is sufficient for performance testing (since it doesn't alter the amount of work done) but not for real-world use.
  {-
  | (ConstantOperand (Const.GlobalReference (PointerType ty _) n)) <- p = return Nothing
  -}
  | (ConstantOperand (Const.GetElementPtr _ addr _)) <- p = inspect (ConstantOperand addr)
  | (ConstantOperand (Const.IntToPtr _ (PointerType ty _))) <- p = gets (Just . (ty,) . fromJust . dontCareMetadata)
  | (ConstantOperand (Const.BitCast x (PointerType ty _))) <- p = do
      meta <- inspect (ConstantOperand x)
      case meta of
        (Just (_, m)) -> return $ Just (ty, m)
        _ -> return Nothing
  {-
  | (ConstantOperand (Const.AddrSpaceCast _ (PointerType ty _))) <- p = return Nothing
  | (ConstantOperand op@(Const.Select _ _ _)) <- p, (PointerType ty _) <- typeOf op = return Nothing
  | (ConstantOperand (Const.ExtractElement v _)) <- p, (PointerType ty _) <- elementType $ typeOf v = return Nothing
  | (ConstantOperand (Const.ExtractValue agg ixs)) <- p = do
      ty <- typeIndex (typeOf agg) (map (ConstantOperand . Const.Int 32 . fromIntegral) ixs)
      return Nothing
  -}

  | otherwise = do
      fname <- gets (name . fromJust . currentFunction)
      pp <- pure $ show p
      tp <- typeOf p
      case tp of
        (PointerType {}) -> do
          tell ["inspect: in function " ++ (show fname) ++ ": unsupported pointer " ++ pp]
          return Nothing
        _ -> error $ "inspect: in function " ++ (show fname) ++ ": argument " ++ pp ++ " is not a pointer"

-- | Allocate local variables to hold the metadata for the given pointer.
allocateLocalMetadataStorage :: (HasCallStack, MonadState SBCETSState m, MonadIRBuilder m, MonadModuleBuilder m) => Operand -> m Metadata
allocateLocalMetadataStorage p = do
  allocated <- gets (Data.Map.member p . localStorage)
  if not allocated
  then do
    basePtr <- alloca (ptr i8) Nothing 8
    boundPtr <- alloca (ptr i8) Nothing 8
    keyPtr <- alloca (i64) Nothing 8
    lockPtr <- alloca (ptr i8) Nothing 8
    let meta = Local basePtr boundPtr keyPtr lockPtr
    associate p meta
    return meta
  else do
    fname <- gets (show . name . fromJust . currentFunction)
    pp <- pure $ show p
    error $ "allocateLocalMetadataStorage: in function " ++ fname ++ ": storage already allocated for metadata for pointer " ++ pp

-- | Look up the local variables allocated to hold metadata for the given pointer
getLocalMetadataStorage :: (HasCallStack, MonadState SBCETSState m, MonadIRBuilder m, MonadModuleBuilder m) => Operand -> m Metadata
getLocalMetadataStorage p = do
  allocated <- gets ((Data.Map.lookup p) . localStorage)
  if isJust allocated
  then gets ((! p) . localStorage)
  else do
    pp <- pure $ show p
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

-- | Load the metadata for the given memory address from the runtime.
emitRuntimeMetadataLoad :: (HasCallStack, MonadState SBCETSState m, MonadWriter [String] m, MonadIRBuilder m, MonadModuleBuilder m) => Operand -> m Metadata
emitRuntimeMetadataLoad addr
  | (LocalReference (PointerType _ _) _) <- addr = do
      allocated <- gets (Data.Map.member addr . localStorage)
      if not allocated
      then do
        pAddr <- pure $ show addr
        tell ["emitRuntimeMetadataLoad: using don't-care metadata for unsupported pointer " ++ pAddr]
        gets (fromJust . dontCareMetadata)
      else do
        addr' <- bitcast addr (ptr i8)
        meta <- getLocalMetadataStorage addr
        _ <- emitRuntimeAPIFunctionCall "__softboundcets_metadata_load" [addr', (base meta), (bound meta), (key meta), (lock meta)]
        emitCheck <- gets (CLI.emitChecks . options)
        when emitCheck $ do
          _ <- emitRuntimeAPIFunctionCall "__softboundcets_metadata_check" [(base meta), (bound meta), (key meta), (lock meta)]
          return ()
        return meta
  | (ConstantOperand {}) <- addr = do
      -- TODO-IMPROVE: If asked to load the metadata for a constant pointer expression or global variable, we currently just return the don't-care metadata.
      -- I believe we can just call __softboundcets_metadata_load here but we need to make sure that we are actually setting up the metadata
      -- storage for global variables properly (in 'instrumentGlobalVariable' below) first.
      pAddr <- pure $ show addr
      tell ["emitRuntimeMetadataLoad: using don't-care metadata for unsupported pointer " ++ pAddr]
      gets (fromJust . dontCareMetadata)
  | otherwise = do
      pAddr <- pure $ show addr
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
  keyReg <- load keyPtr 0
  _ <- emitRuntimeAPIFunctionCall "__softboundcets_destroy_stack_key" [keyReg]
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

-- | Store the metadata in registers for the given pointer into the local variables allocated to hold it.
emitMetadataStoreToLocalVariables :: (HasCallStack, MonadState SBCETSState m, MonadIRBuilder m, MonadModuleBuilder m) => Operand -> (Operand, Operand, Operand, Operand) -> m Metadata
emitMetadataStoreToLocalVariables p (baseReg, boundReg, keyReg, lockReg) = do
  meta <- getLocalMetadataStorage p
  store (base meta) 8 baseReg
  store (bound meta) 8 boundReg
  store (key meta) 8 keyReg
  store (lock meta) 8 lockReg
  return meta

-- | Load the metadata for a pointer function parameter from the shadow stack.
emitMetadataLoadFromShadowStack :: (HasCallStack, MonadState SBCETSState m, MonadIRBuilder m, MonadModuleBuilder m) => Operand -> Integer -> m Metadata
emitMetadataLoadFromShadowStack p ix = do
  ix' <- pure $ int32 ix
  baseReg <- emitRuntimeAPIFunctionCall "__softboundcets_load_base_shadow_stack" [ix']
  boundReg <- emitRuntimeAPIFunctionCall "__softboundcets_load_bound_shadow_stack" [ix']
  keyReg <- emitRuntimeAPIFunctionCall "__softboundcets_load_key_shadow_stack" [ix']
  lockReg <- emitRuntimeAPIFunctionCall "__softboundcets_load_lock_shadow_stack" [ix']
  emitMetadataStoreToLocalVariables p (baseReg, boundReg, keyReg, lockReg)

-- | Store the metadata for a pointer on the shadow stack at the specified position.
emitMetadataStoreToShadowStack :: (HasCallStack, MonadModuleBuilder m, MonadState SBCETSState m, MonadWriter [String] m, MonadIRBuilder m) => Maybe Name -> Operand -> Integer -> m ()
emitMetadataStoreToShadowStack callee p ix = do
  meta <- inspect p
  if isNothing meta
  then do
    fname <- gets (show . name . fromJust . currentFunction)
    pp <- pure $ show p
    tell ["emitMetadataStoreToShadowStack: in function " ++ fname ++ ": using don't-care metadata for unsupported pointer " ++ pp ++ " passed to function " ++ (show $ fromJust callee)]
    meta' <- gets (fromJust . dontCareMetadata)
    ix' <- pure $ int32 ix
    baseReg <- load (base meta') 0
    boundReg <- load (bound meta') 0
    keyReg <- load (key meta') 0
    lockReg <- load (lock meta') 0
    _ <- emitRuntimeAPIFunctionCall "__softboundcets_store_base_shadow_stack" [baseReg, ix']
    _ <- emitRuntimeAPIFunctionCall "__softboundcets_store_bound_shadow_stack" [boundReg, ix']
    _ <- emitRuntimeAPIFunctionCall "__softboundcets_store_key_shadow_stack" [keyReg, ix']
    _ <- emitRuntimeAPIFunctionCall "__softboundcets_store_lock_shadow_stack" [lockReg, ix']
    return ()
  else do
    case (fromJust meta) of
      (_, meta'@(Local {})) -> do
        ix' <- pure $ int32 ix
        baseReg <- load (base meta') 0
        boundReg <- load (bound meta') 0
        keyReg <- load (key meta') 0
        lockReg <- load (lock meta') 0
        _ <- emitRuntimeAPIFunctionCall "__softboundcets_store_base_shadow_stack" [baseReg, ix']
        _ <- emitRuntimeAPIFunctionCall "__softboundcets_store_bound_shadow_stack" [boundReg, ix']
        _ <- emitRuntimeAPIFunctionCall "__softboundcets_store_key_shadow_stack" [keyReg, ix']
        _ <- emitRuntimeAPIFunctionCall "__softboundcets_store_lock_shadow_stack" [lockReg, ix']
        return ()
      (_, meta'@(Constant {})) -> do
        ix' <- pure $ int32 ix
        lockReg <- load (lock meta') 0
        _ <- emitRuntimeAPIFunctionCall "__softboundcets_store_base_shadow_stack" [base meta', ix']
        _ <- emitRuntimeAPIFunctionCall "__softboundcets_store_bound_shadow_stack" [bound meta', ix']
        _ <- emitRuntimeAPIFunctionCall "__softboundcets_store_key_shadow_stack" [key meta', ix']
        _ <- emitRuntimeAPIFunctionCall "__softboundcets_store_lock_shadow_stack" [lockReg, ix']
        return ()

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

-- | Check if the given function symbol is a function with a runtime wrapper
isWrappedFunction :: MonadState SBCETSState m => Name -> m Bool
isWrappedFunction n = gets (Data.Set.member n . Data.Map.keysSet . stdlibWrapperPrototypes)

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
          gType <- liftM ptr $ typeOf (fromJust $ initializer g)
          let gPtr = (ConstantOperand $ Const.GlobalReference gType gName)
          -- The address of a global variable is always safe
          mark gPtr Safe
          -- TODO-IMPROVE: We don't yet handle global variables properly. Pointers to globals currently cause 'inspect' to return 'Nothing'.
      | otherwise = do
        pg <- pure $ show g
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
                             , nullMetadata = Nothing
                             , localStorage = Data.Map.empty
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
            -- Create the null pointer metadata
            let nullMetaBase = ConstantOperand $ Const.IntToPtr (Const.Int 64 0) (ptr i8)
            let nullMetaBound = ConstantOperand $ Const.IntToPtr (Const.Int 64 0) (ptr i8)
            let nullMetaKey = ConstantOperand $ Const.Int 64 0
            let nullMetaLock = ConstantOperand $ Const.IntToPtr (Const.Int 64 0) (ptr i8)
            modify $ \s -> s { nullMetadata = Just $ Constant nullMetaBase nullMetaBound nullMetaKey nullMetaLock }
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
        when (not $ Helpers.isFunctionType ty) $ do -- TODO-IMPROVE: We don't currently instrument function pointers
          let resultPtr = LocalReference (ptr ty) v
            -- The address of a stack allocation is always safe
          mark resultPtr Safe
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
            baseReg <- bitcast resultPtr (ptr i8)
            intBase <- ptrtoint baseReg i64
            intBound <- add allocSize intBase
            boundReg <- inttoptr intBound (ptr i8)
            functionKeyPtr <- gets (fromJust . localStackFrameKeyPtr)
            functionLockPtr <- gets (fromJust . localStackFrameLockPtr)
            keyReg <- load functionKeyPtr 0
            lockReg <- load functionLockPtr 0
            _ <- emitMetadataStoreToLocalVariables resultPtr (baseReg, boundReg, keyReg, lockReg)
            return ()

      | (Load _ addr _ _ _) <- o = do
        enable <- gets (CLI.instrumentLoad . options)
        when enable $ do
          aClass <- query Unsafe addr
          case aClass of
            Unsafe -> do
              meta <- inspect addr
              if isNothing meta
              then do
                pAddr <- pure $ show addr
                pFunc <- gets (show . name . fromJust . currentFunction)
                pInst <- pure $ show i
                tell ["in function " ++ pFunc ++ ": using don't-care metadata for unsupported pointer " ++ pAddr ++ " in " ++ pInst]
                refTy <- liftM pointerReferent $ typeOf addr
                dcMeta <- gets (fromJust . dontCareMetadata)
                baseReg <- load (base dcMeta) 0
                boundReg <- load (bound dcMeta) 0
                addr' <- bitcast addr (ptr i8)
                tySize <- sizeof 64 refTy
                -- Check the load is spatially in bounds
                _ <- emitRuntimeAPIFunctionCall "__softboundcets_spatial_load_dereference_check" [baseReg, boundReg, addr', tySize]
                -- Check the load is temporally in bounds
                keyReg <- load (key dcMeta) 0
                lockReg <- load (lock dcMeta) 0
                _ <- emitRuntimeAPIFunctionCall "__softboundcets_temporal_load_dereference_check" [lockReg, keyReg]
                return ()
              else do
                case (fromJust meta) of
                  (refTy, meta'@(Local {})) -> do -- Metadata in local variables
                    baseReg <- load (base meta') 0
                    boundReg <- load (bound meta') 0
                    addr' <- bitcast addr (ptr i8)
                    tySize <- sizeof 64 refTy
                    -- Check the load is spatially in bounds
                    _ <- emitRuntimeAPIFunctionCall "__softboundcets_spatial_load_dereference_check" [baseReg, boundReg, addr', tySize]
                    -- Check the load is temporally in bounds
                    keyReg <- load (key meta') 0
                    lockReg <- load (lock meta') 0
                    _ <- emitRuntimeAPIFunctionCall "__softboundcets_temporal_load_dereference_check" [lockReg, keyReg]
                    return ()
                  (refTy, meta'@(Constant {})) -> do -- Metadata in constant expressions
                    addr' <- bitcast addr (ptr i8)
                    tySize <- sizeof 64 refTy
                    -- Check the load is spatially in bounds
                    _ <- emitRuntimeAPIFunctionCall "__softboundcets_spatial_load_dereference_check" [base meta', bound meta', addr', tySize]
                    -- Check the load is temporally in bounds
                    lockReg <- load (lock meta') 0
                    _ <- emitRuntimeAPIFunctionCall "__softboundcets_temporal_load_dereference_check" [lockReg, key meta']
                    return ()
            Safe -> return ()

        Helpers.emitNamedInst i

        when enable $ do -- No matter if we were able to instrument the load or not, if a pointer was loaded, ask the runtime for metadata for the loaded address.
          ta <- typeOf addr
          when ((Helpers.isPointerType $ pointerReferent ta) &&
                (not $ Helpers.isFunctionType $ pointerReferent $ pointerReferent ta)) $ do -- TODO-IMPROVE: We don't currently instrument function pointers
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
              -- TODO-OPTIMIZE: The function could potentially deallocate any pointer it is passed
              mapM_ (flip mark $ Unsafe) $ Data.Set.fromList ptrArgs
              -- Read the metadata for the return value off the shadow stack if it is a pointer
              when (Helpers.isPointerType rt && (not $ Helpers.isFunctionType $ pointerReferent rt)) $ do
                _ <- emitMetadataLoadFromShadowStack (LocalReference rt v) 0
                safe <- returnsSafePointer fname
                if safe
                then mark (LocalReference rt v) Safe
                else mark (LocalReference rt v) Unsafe
              emitShadowStackDeallocation
            (UnName {}) -> do -- TODO-IMPROVE: Calling a computed function pointer. Can we map this to a function symbol?
              Helpers.emitNamedInst i

      | (GetElementPtr _ addr ixs _) <- o = do
        ta <- typeOf addr
        refTy <- Helpers.typeIndex ta ixs
        if isNothing refTy
        then do
          -- TODO-IMPROVE: Softboundcets doesn't handle opaque structure types (https://llvm.org/docs/LangRef.html#opaque-structure-types) but we could do so.
          pAddr <- pure $ show addr
          pIxs <- pure $ map show ixs
          tell ["Unable to compute type of getelementptr result: " ++ (show ta) ++ " " ++ pAddr ++ " [" ++ (intercalate ", " pIxs) ++ "]"]
          return ()
        else do
          let gepResultPtr = LocalReference (ptr $ fromJust refTy) v
          meta <- inspect addr
          if isNothing meta
          then do
            pAddr <- pure $ show addr
            pFunc <- gets (show . name . fromJust . currentFunction)
            pInst <- pure $ show i
            tell ["in function " ++ pFunc ++ ": using don't-care metadata for unsupported pointer " ++ pAddr ++ " in " ++ pInst]
            dcMeta <- gets (fromJust . dontCareMetadata)
            associate gepResultPtr dcMeta -- The pointer created by getelementptr shares metadata storage with the parent pointer
            mark gepResultPtr Unsafe -- TODO-OPTIMIZE: arithmetic derived pointers are considered unconditionally unsafe (even if the parent pointer is safe)
          else do
            let (_, meta') = fromJust meta
            associate gepResultPtr meta' -- The pointer created by getelementptr shares metadata storage with the parent pointer
            mark gepResultPtr Unsafe -- TODO-OPTIMIZE: arithmetic derived pointers are considered unconditionally unsafe (even if the parent pointer is safe)
        Helpers.emitNamedInst i

      | (BitCast addr pty@(PointerType {}) _) <- o = do
        enable <- gets (CLI.instrumentBitcast . options)
        when (enable && (not $ Helpers.isFunctionType $ pointerReferent pty)) $ do -- TODO-IMPROVE: We don't currently instrument function pointers
          let bitcastResultPtr = LocalReference pty v
          meta <- inspect addr
          if isNothing meta
          then do
            pAddr <- pure $ show addr
            pFunc <- gets (show . name . fromJust . currentFunction)
            pInst <- pure $ show i
            tell ["in function " ++ pFunc ++ ": using don't-care metadata for unsupported pointer " ++ pAddr ++ " in " ++ pInst]
            dcMeta <- gets (fromJust . dontCareMetadata)
            associate bitcastResultPtr dcMeta -- The pointer created by bitcast shares metadata storage with the parent pointer
            mark bitcastResultPtr Unsafe -- TODO-OPTIMIZE: cast pointers are considered unconditionally unsafe (even if the parent pointer is safe)
          else do
            let (_, meta') = fromJust meta
            associate bitcastResultPtr meta' -- The pointer created by bitcast shares metadata storage with the parent pointer
            mark bitcastResultPtr Unsafe -- TODO-OPTIMIZE: cast pointers are considered unconditionally unsafe (even if the parent pointer is safe)
        Helpers.emitNamedInst i

      | (Select cond tval fval _) <- o = do
        Helpers.emitNamedInst i
        selTy <- typeOf tval
        when (Helpers.isPointerType selTy && (not $ Helpers.isFunctionType $ pointerReferent selTy)) $ do -- TODO-IMPROVE: We don't currently instrument function pointers
          let resultPtr = LocalReference selTy v
          tClass <- query Unsafe tval
          fClass <- query Unsafe fval
          case (tClass, fClass) of
            (Safe, Safe) -> mark resultPtr Safe
            _ -> mark resultPtr Unsafe

          tMeta <- inspect tval
          tMeta' <-
            if isJust tMeta
            then return $ snd $ fromJust tMeta
            else do
              pAddr <- pure $ show tval
              pFunc <- gets (show . name . fromJust . currentFunction)
              pInst <- pure $ show i
              tell ["in function " ++ pFunc ++ ": using don't-care metadata for uninstrumented pointer " ++ pAddr ++ " in " ++ pInst]
              dcMeta <- gets (fromJust . dontCareMetadata)
              return dcMeta

          fMeta <- inspect fval
          fMeta' <-
            if isJust fMeta
            then return $ snd $ fromJust fMeta
            else do
              pAddr <- pure $ show fval
              pFunc <- gets (show . name . fromJust . currentFunction)
              pInst <- pure $ show i
              tell ["in function " ++ pFunc ++ ": using don't-care metadata for uninstrumented pointer " ++ pAddr ++ " in " ++ pInst]
              dcMeta <- gets (fromJust . dontCareMetadata)
              return dcMeta

          basePtr <- select cond (base tMeta') (base fMeta')
          boundPtr <- select cond (bound tMeta') (bound fMeta')
          keyPtr <- select cond (key tMeta') (key fMeta')
          lockPtr <- select cond (lock tMeta') (lock fMeta')
          let meta = Local basePtr boundPtr keyPtr lockPtr
          associate resultPtr meta -- The pointer created by select shares metadata storage with the parent pointer

      | (Phi (PointerType ty _) incoming _) <- o = do
        Helpers.emitNamedInst i

        let phiMeta (p, n) = do meta <- inspect p
                                if isNothing meta
                                then do
                                  pAddr <- pure $ show p
                                  pFunc <- gets (show . name . fromJust . currentFunction)
                                  pInst <- pure $ show i
                                  tell ["in function " ++ pFunc ++ ": using don't-care metadata for uninstrumented pointer " ++ pAddr ++ " in " ++ pInst]
                                  dcMeta <- gets (fromJust . dontCareMetadata)
                                  return (dcMeta, n)
                                else do
                                 let x = snd $ fromJust meta
                                 return (x, n)

        when (not $ Helpers.isFunctionType ty) $ do -- TODO-IMPROVE: We don't currently instrument function pointers
          incomingMeta <- forM incoming phiMeta
          basePtr <- phi $ map (\(x, n) -> (base x, n)) incomingMeta
          boundPtr <- phi $ map (\(x, n) -> (bound x, n)) incomingMeta
          keyPtr <- phi $ map (\(x, n) -> (key x, n)) incomingMeta
          lockPtr <- phi $ map (\(x, n) -> (lock x, n)) incomingMeta
          let resultPtr = LocalReference (ptr ty) v
          let meta = Local basePtr boundPtr keyPtr lockPtr
          associate resultPtr meta -- The pointer created by phi shares metadata storage with the parent pointer

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
              mapM_ (flip mark $ Unsafe) $ Data.Set.fromList ptrArgs
              emitShadowStackDeallocation
            (UnName {}) -> do -- TODO-IMPROVE: Calling a computed function pointer. Can we map this to a function symbol?
              Helpers.emitNamedInst i

      | (Store _ tgt src _ _ _) <- o = do
        enable <- gets (CLI.instrumentStore . options)
        when enable $ do
          tgtClass <- query Unsafe tgt
          case tgtClass of
            Unsafe -> do
              meta <- inspect tgt
              if isNothing meta
              then do
                pAddr <- pure $ show tgt
                pFunc <- gets (show . name . fromJust . currentFunction)
                pInst <- pure $ show i
                tell ["in function " ++ pFunc ++ ": using don't-care metadata for uninstrumented pointer " ++ pAddr ++ " in " ++ pInst]
                refTy <- liftM pointerReferent $ typeOf tgt
                dcMeta <- gets (fromJust . dontCareMetadata)
                baseReg <- load (base dcMeta) 0
                boundReg <- load (bound dcMeta) 0
                tgt' <- bitcast tgt (ptr i8)
                tySize <- sizeof 64 refTy
                -- Check the store is spatially in bounds
                _ <- emitRuntimeAPIFunctionCall "__softboundcets_spatial_store_dereference_check" [baseReg, boundReg, tgt', tySize]
                -- Check the store is temporally in bounds
                keyReg <- load (key dcMeta) 0
                lockReg <- load (lock dcMeta) 0
                _ <- emitRuntimeAPIFunctionCall "__softboundcets_temporal_store_dereference_check" [lockReg, keyReg]
                return ()
              else do
                case (fromJust meta) of
                  (refTy, meta'@(Local {})) -> do -- Metadata in local variables
                    baseReg <- load (base meta') 0
                    boundReg <- load (bound meta') 0
                    tgt' <- bitcast tgt (ptr i8)
                    tySize <- sizeof 64 refTy
                    -- Check the store is spatially in bounds
                    _ <- emitRuntimeAPIFunctionCall "__softboundcets_spatial_store_dereference_check" [baseReg, boundReg, tgt', tySize]
                    -- Check the store is temporally in bounds
                    keyReg <- load (key meta') 0
                    lockReg <- load (lock meta') 0
                    _ <- emitRuntimeAPIFunctionCall "__softboundcets_temporal_store_dereference_check" [lockReg, keyReg]
                    return ()
                  (refTy, meta'@(Constant {})) -> do -- Metadata in constant expressions
                    tgt' <- bitcast tgt (ptr i8)
                    tySize <- sizeof 64 refTy
                    -- Check the store is spatially in bounds
                    _ <- emitRuntimeAPIFunctionCall "__softboundcets_spatial_store_dereference_check" [base meta', bound meta', tgt', tySize]
                    -- Check the store is temporally in bounds
                    lockReg <- load (lock meta') 0
                    _ <- emitRuntimeAPIFunctionCall "__softboundcets_temporal_store_dereference_check" [lockReg, key meta']
                    return ()
            Safe -> return ()

        Helpers.emitNamedInst i

        when enable $ do
          ty <- typeOf src
          when (Helpers.isPointerType ty && (not $ Helpers.isFunctionType $ pointerReferent ty)) $ do
            meta <- inspect src
            if isNothing meta
            then do
              pAddr <- pure $ show src
              pFunc <- gets (show . name . fromJust . currentFunction)
              pInst <- pure $ show i
              tell ["in function " ++ pFunc ++ ": using don't-care metadata for uninstrumented pointer " ++ pAddr ++ " in " ++ pInst]
              dcMeta <- gets (fromJust . dontCareMetadata)
              tgt' <- bitcast tgt (ptr i8)
              baseReg <- load (base dcMeta) 0
              boundReg <- load (bound dcMeta) 0
              keyReg <- load (key dcMeta) 0
              lockReg <- load (lock dcMeta) 0
              -- Write the metadata for the stored pointer to the runtime
              _ <- emitRuntimeAPIFunctionCall "__softboundcets_metadata_store" [tgt', baseReg, boundReg, keyReg, lockReg]
              return ()
            else do
              case (fromJust meta) of
                (_, meta'@(Local {})) -> do -- Metadata in local variables
                  tgt' <- bitcast tgt (ptr i8)
                  baseReg <- load (base meta') 0
                  boundReg <- load (bound meta') 0
                  keyReg <- load (key meta') 0
                  lockReg <- load (lock meta') 0
                  -- Write the metadata for the stored pointer to the runtime
                  _ <- emitRuntimeAPIFunctionCall "__softboundcets_metadata_store" [tgt', baseReg, boundReg, keyReg, lockReg]
                  return ()
                (_, meta'@(Constant {})) -> do -- Metadata in constant expressions
                  tgt' <- bitcast tgt (ptr i8)
                  lockReg <- load (lock meta') 0
                  -- Write the metadata for the stored pointer to the runtime
                  _ <- emitRuntimeAPIFunctionCall "__softboundcets_metadata_store" [tgt', base meta', bound meta', key meta', lockReg]
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
