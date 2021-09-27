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
import Data.Either
import LLVM.AST hiding (args, index, type')
import LLVM.AST.Global
import LLVM.AST.Type
import LLVM.AST.Typed (typeOf, indexTypeByOperands)
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

data VariableMetadata = Variable { vBase :: Operand
                                 , vBound :: Operand
                                 , vKey :: Operand
                                 , vLock :: Operand
                                 }

data ConstantMetadata = Constant { cBase :: Const.Constant
                                 , cBound :: Const.Constant
                                 , cKey :: Const.Constant
                                 , cLock :: Const.Constant
                                 }
                      | Global   { gBase :: Const.Constant
                                 , gBound :: Const.Constant
                                 , gKey :: Const.Constant
                                 } -- Globals use the runtime-initialized global lock

baseOf :: (Either VariableMetadata ConstantMetadata) -> Operand
baseOf (Left l) = vBase l
baseOf (Right r@(Constant {})) = ConstantOperand $ cBase r
baseOf (Right r@(Global {})) = ConstantOperand $ gBase r

boundOf :: (Either VariableMetadata ConstantMetadata) -> Operand
boundOf (Left l) = vBound l
boundOf (Right r@(Constant {})) = ConstantOperand $ cBound r
boundOf (Right r@(Global {})) = ConstantOperand $ gBound r

keyOf :: (Either VariableMetadata ConstantMetadata) -> Operand
keyOf (Left l) = vKey l
keyOf (Right r@(Constant {})) = ConstantOperand $ cKey r
keyOf (Right r@(Global {})) = ConstantOperand $ gKey r

lockOf :: (HasCallStack, MonadState SBCETSPassState m) => (Either VariableMetadata ConstantMetadata) -> m Operand
lockOf (Left l) = pure $ vLock l
lockOf (Right r@(Constant {})) = pure $ ConstantOperand $ cLock r
lockOf (Right (Global {})) = gets (fromJust . globalLockPtr)

type SoftboundCETSPass a = InstrumentorPass () [String] SBCETSPassState a

data SBCETSPassState = SBCETSPassState { globalLockPtr :: Maybe Operand
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
                                       , currentFunctionGlobals :: Data.Set.Set Operand
                                       -- ^ The set of global references in the current function. These need local metadata when they are used, which is populated from their constant metadata.
                                       , classifications :: Map Operand PointerClass
                                       -- ^ The map of pointers to their class.
                                       --   'classifications' needs to be saved and restored around function entry and exit so we don't mistakenly treat
                                       --   pointers with the same names in different functions as belonging to the same class. Outside a function, 'classifications' contains global variables.
                                       , blacklist :: Data.Set.Set Name
                                       -- ^ The set of blacklisted function symbols (these will not be instrumented).
                                       , localVariableMetadata :: Map Operand VariableMetadata
                                       -- ^ A 'Map' from pointer 'Operand's in the current function to their variable metadata.
                                       , localConstantMetadata :: Map Operand ConstantMetadata
                                       -- ^ A 'Map' from pointer 'Operand's in the current function to their constant metadata.
                                       , baseRegisterMetadata :: Map Operand Operand
                                       -- ^ A 'Map' from pointer 'Operand's in the current function to their base metadata register.
                                       , boundRegisterMetadata :: Map Operand Operand
                                       -- ^ A 'Map' from pointer 'Operand's in the current function to their bound metadata register.
                                       , keyRegisterMetadata :: Map Operand Operand
                                       -- ^ A 'Map' from pointer 'Operand's in the current function to their key metadata register.
                                       , lockRegisterMetadata :: Map Operand Operand
                                       -- ^ A 'Map' from pointer 'Operand's in the current function to their lock metadata register.
                                       , globalMetadata :: Map Operand ConstantMetadata
                                       -- ^ A 'Map' from pointer 'Operand's in the global scope to their metadata.
                                       , localDCMetadata :: Maybe VariableMetadata
                                       -- ^ We need need the DC metadata in local variables for select and phi instructions
                                       , localNullMetadata :: Maybe VariableMetadata
                                       -- ^ We need the null metadata in local variables for select and phi instructions
                                       }

-- | Create an empty 'SBCETSPassState'
emptySBCETSPassState :: SBCETSPassState
emptySBCETSPassState = SBCETSPassState Nothing Nothing Nothing
                               Data.Set.empty
                               Data.Map.empty Data.Map.empty
                               CLI.defaultOptions Nothing Data.Set.empty
                               Data.Map.empty Data.Set.empty
                               Data.Map.empty Data.Map.empty Data.Map.empty Data.Map.empty
                               Data.Map.empty Data.Map.empty Data.Map.empty
                               Nothing Nothing

-- | The initial 'SBCETSPassState' has 'stdlibWrapperPrototypes' and 'runtimeFunctionPrototypes' populated since these are fixed at build time.
initSBCETSPassState :: SBCETSPassState
initSBCETSPassState = emptySBCETSPassState
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
    (mkName "__softboundcets_memcopy_check",                     FunctionType void [ptr i8, ptr i8, i64, ptr i8, ptr i8, ptr i8, ptr i8, i64, ptr i8, i64, ptr i8] False),
    (mkName "__softboundcets_memset_check",                      FunctionType void [ptr i8, i64, ptr i8, ptr i8, i64, ptr i8] False),
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
mark :: MonadState SBCETSPassState m => Operand -> PointerClass -> m ()
mark p c = modify $ \s -> s { classifications = Data.Map.insert p c $ classifications s }

-- | Query the 'PointerClass' of a pointer, defaulting to the given 'PointerClass' if the pointer is not tracked.
query :: MonadState SBCETSPassState m => PointerClass-> Operand -> m PointerClass
query d p = gets (maybe d id . (Data.Map.lookup p . classifications))

-- | Associate local metadata storage with the given pointer.
associate :: MonadState SBCETSPassState m => Operand -> (Either VariableMetadata ConstantMetadata) -> m ()
associate p (Left m) = modify $ \s -> s { localVariableMetadata = Data.Map.insert p m $ localVariableMetadata s }
associate p (Right m) = modify $ \s -> s { localConstantMetadata = Data.Map.insert p m $ localConstantMetadata s }

-- | Mark the registers holding the metadata values for the given pointer as live
gen :: MonadState SBCETSPassState m => Operand -> (Operand, Operand, Operand, Operand) -> m ()
gen p (baseReg, boundReg, keyReg, lockReg) = do
  useRegMeta <- gets (CLI.reuseRegisters . options)
  if useRegMeta
  then do
    modify $ \s -> s { baseRegisterMetadata = Data.Map.insert p baseReg $ baseRegisterMetadata s }
    modify $ \s -> s { boundRegisterMetadata = Data.Map.insert p boundReg $ boundRegisterMetadata s }
    modify $ \s -> s { keyRegisterMetadata = Data.Map.insert p keyReg $ keyRegisterMetadata s }
    modify $ \s -> s { lockRegisterMetadata = Data.Map.insert p lockReg $ lockRegisterMetadata s }
  else return ()

-- | Mark the registers holding the metadata values for the given pointer as dead
kill :: MonadState SBCETSPassState m => Operand -> m ()
kill p =  do
  useRegMeta <- gets (CLI.reuseRegisters . options)
  if useRegMeta
  then do
    modify $ \s -> s { baseRegisterMetadata = Data.Map.delete p $ baseRegisterMetadata s }
    modify $ \s -> s { boundRegisterMetadata = Data.Map.delete p $ boundRegisterMetadata s }
    modify $ \s -> s { keyRegisterMetadata = Data.Map.delete p $ keyRegisterMetadata s }
    modify $ \s -> s { lockRegisterMetadata = Data.Map.delete p $ lockRegisterMetadata s }
  else return ()

-- | Mark only the lock register for the given pointer as dead
killLock :: MonadState SBCETSPassState m => Operand -> m ()
killLock p =  do
  useRegMeta <- gets (CLI.reuseRegisters . options)
  if useRegMeta
  then do
    modify $ \s -> s { lockRegisterMetadata = Data.Map.delete p $ lockRegisterMetadata s }
  else return ()

-- | Reload the metadata value from the stack into a register if necessary
reload :: (MonadIRBuilder m, MonadModuleBuilder m, MonadState SBCETSPassState m) => (SBCETSPassState -> Map Operand Operand) -> Operand -> Operand -> m Operand
reload registerMeta localVarMeta p = do
  isLive <- gets (Data.Map.member p . registerMeta)
  if isLive
  then gets ((! p) . registerMeta)
  else do
    reg <- load localVarMeta 0
    return reg

-- | Decide whether the given function symbol is a function that should not be instrumented.
isIgnoredFunction :: MonadState SBCETSPassState m => Name -> m Bool
isIgnoredFunction func
  | Helpers.isInfixOfName "__softboundcets" func = return True  -- One of our runtime functions
  | Helpers.isInfixOfName "isoc99" func = return True           -- ISO C99 intrinsic functions
  | Helpers.isInfixOfName "llvm." func = return True            -- LLVM intrinsic functions
  | otherwise = do
      blist <- gets blacklist
      return $ Data.Set.member func blist               -- Function symbols explicitly blacklisted by the user

-- | Decide whether the given function symbol returns a safe pointer
returnsSafePointer :: MonadState SBCETSPassState m => Name -> m Bool
returnsSafePointer func
  | (mkName "malloc") == func = return True
  | (mkName "calloc") == func = return True
  | (mkName "realloc") == func = return True
  | otherwise = return False

-- | Check if the given function symbol is a function with a runtime wrapper
isWrappedFunction :: MonadState SBCETSPassState m => Name -> m Bool
isWrappedFunction n = gets (Data.Set.member n . Data.Map.keysSet . stdlibWrapperPrototypes)

-- | Generate the don't-care metadata. We set base to zero and bound to 2^(pointer bits). We set key and lock to zero.
--   We cannot use the global lock here, because we may free things with don't-care metadata, and it would look like we were trying to free a global.
generateDCMetadata :: (HasCallStack, MonadState SBCETSPassState m, MonadIRBuilder m, MonadModuleBuilder m) => m ConstantMetadata
generateDCMetadata = do
  let dcMetaBase = Const.IntToPtr (Const.Int 64 0) (ptr i8)
  pointerBits <- gets (CLI.pointerWidth . options)
  let dcMetaBound = Const.IntToPtr (Const.Int 64 ((2 ^ pointerBits) - 1)) (ptr i8)
  let dcMetaKey = Const.Int 64 0
  let dcMetaLock = Const.IntToPtr (Const.Int 64 0) (ptr i8)
  return $ Constant dcMetaBase dcMetaBound dcMetaKey dcMetaLock

-- | The null metadata must always cause a spatial error, but cannot cause a temporal error.
--   To generate a compulsory spatial error, we can set base = bound. For sanity, we set base = bound = 0 here.
generateNullMetadata :: (HasCallStack, MonadState SBCETSPassState m, MonadIRBuilder m, MonadModuleBuilder m) => m ConstantMetadata
generateNullMetadata = do
  let nullMetaBase = Const.IntToPtr (Const.Int 64 0) (ptr i8)
  let nullMetaBound = Const.IntToPtr (Const.Int 64 0) (ptr i8)
  let nullMetaKey = Const.Int 64 0
  let nullMetaLock = Const.IntToPtr (Const.Int 64 0) (ptr i8)
  return $ Constant nullMetaBase nullMetaBound nullMetaKey nullMetaLock

-- | 'inspect' traverses pointer-type expressions and returns the metadata. If there is no metadata for a pointer, it is not instrumented.
inspect :: (HasCallStack, MonadState SBCETSPassState m, MonadWriter [String] m, MonadModuleBuilder m, MonadIRBuilder m) => Operand -> m (Maybe (Type, Either VariableMetadata ConstantMetadata))
inspect p
  | (LocalReference (PointerType ty _) _) <- p = do
      if Helpers.isFunctionType ty
      then return Nothing -- TODO-IMPROVE: Function pointers should be checked for spatial safety at callsites.
      else do
        haveVariableMeta <- gets (Data.Map.member p . localVariableMetadata)
        haveConstantMeta <- gets (Data.Map.member p . localConstantMetadata)
        if haveVariableMeta
        then gets (Just . (ty,) . Left . (! p) . localVariableMetadata)
        else if haveConstantMeta
        then gets (Just . (ty,) . Right . (! p) . localConstantMetadata)
        else do
          pp <- pure $ show p
          fname <- gets (name . fromJust . currentFunction)
          tell ["inspect: in function " ++ (show fname) ++ ": uninstrumented pointer " ++ pp]
          return Nothing

  | (ConstantOperand (Const.Null (PointerType ty _))) <- p = do
      benchmarking <- gets (CLI.benchmarkMode . options)
      if benchmarking -- Constant null pointers are not instrumented in benchmarking mode
      then return Nothing
      else do
        nullMeta <- generateNullMetadata
        return $ Just (ty, Right nullMeta)
  | (ConstantOperand (Const.Undef (PointerType ty _))) <- p = do
      benchmarking <- gets (CLI.benchmarkMode . options)
      if benchmarking -- Undef pointers are not instrumented in benchmarking mode
      then return Nothing
      else do
        nullMeta <- generateNullMetadata
        return $ Just (ty, Right nullMeta)
  | (ConstantOperand (Const.GlobalReference (PointerType ty _) _)) <- p = do
      present <- gets (Data.Map.member p . globalMetadata)
      if present
      then gets (Just . (ty,) . Right . (! p) . globalMetadata)
      else do
        pp <- pure $ show p
        fname <- gets (name . fromJust . currentFunction)
        tell ["inspect: in function " ++ (show fname) ++ ": uninstrumented global pointer " ++ pp]
        return Nothing

  | (ConstantOperand (Const.GetElementPtr _ addr _)) <- p = inspect (ConstantOperand addr)
  | (ConstantOperand (Const.IntToPtr _ (PointerType {}))) <- p = return Nothing
  | (ConstantOperand (Const.BitCast x (PointerType ty _))) <- p = do
      meta <- inspect (ConstantOperand x)
      case meta of
        (Just (_, m)) -> return $ Just (ty, m)
        _ -> return Nothing

  -- TODO-IMPROVE: Some constant expressions of pointer type are currently uninstrumented.
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
        (Left s) -> error s
        (Right (PointerType {})) -> do
          tell ["inspect: in function " ++ (show fname) ++ ": unsupported pointer " ++ pp]
          return Nothing
        (Right _) -> error $ "inspect: in function " ++ (show fname) ++ ": argument " ++ pp ++ " is not a pointer"

-- | Allocate local variables to hold the metadata for the given pointer.
allocateLocalVariableMetadataStorage :: (HasCallStack, MonadState SBCETSPassState m, MonadIRBuilder m, MonadModuleBuilder m) => Operand -> m VariableMetadata
allocateLocalVariableMetadataStorage p = do
  let p' = Helpers.walk p
  allocated <- gets (Data.Map.member p' . localVariableMetadata)
  if not allocated
  then do
    basePtr <- (alloca (ptr i8) Nothing 8) `named` (fromString "sbcets.base")
    boundPtr <- (alloca (ptr i8) Nothing 8) `named` (fromString "sbcets.bound")
    keyPtr <- (alloca (i64) Nothing 8) `named` (fromString "sbcets.key")
    lockPtr <- (alloca (ptr i8) Nothing 8) `named` (fromString "sbcets.lock")
    let meta = Variable basePtr boundPtr keyPtr lockPtr
    associate p' (Left meta)
    return meta
  else do
    fname <- gets (show . name . fromJust . currentFunction)
    pp <- pure $ show p'
    error $ "allocateLocalVariableMetadataStorage: in function " ++ fname ++ ": storage already allocated for metadata for pointer " ++ pp

-- | Check if a pointer has local variable metadata storage associated with it in the pass state
hasLocalVariableMetadata :: (HasCallStack, MonadState SBCETSPassState m, MonadWriter [String] m, MonadIRBuilder m, MonadModuleBuilder m) => Operand -> m Bool
hasLocalVariableMetadata pointer
  | (LocalReference (PointerType _ _) _) <- pointer = do
      gets (Data.Map.member pointer . localVariableMetadata)
  | otherwise = do
      pPointer <- pure $ show pointer
      error $ "hasLocalVariableMetadata: expected local variable pointer but saw " ++ pPointer

-- | Look up the local variables allocated to hold metadata for the given pointer
getLocalVariableMetadata :: (HasCallStack, MonadState SBCETSPassState m, MonadIRBuilder m, MonadModuleBuilder m) => Operand -> m VariableMetadata
getLocalVariableMetadata p = do
  let p' = Helpers.walk p
  allocated <- gets ((Data.Map.lookup p') . localVariableMetadata)
  if isJust allocated
  then gets ((! p') . localVariableMetadata)
  else do
    pp <- pure $ show p'
    error $ "getLocalVariableMetadata: no storage allocated for metadata for pointer " ++ pp

-- | Check if a pointer has constant metadata storage associated with it in the pass state
hasConstantMetadata :: (HasCallStack, MonadState SBCETSPassState m, MonadWriter [String] m, MonadIRBuilder m, MonadModuleBuilder m) => Operand -> m Bool
hasConstantMetadata pointer
  | (LocalReference (PointerType _ _) _) <- pointer = do
      gets (Data.Map.member pointer . localConstantMetadata)
  | otherwise = do
      pPointer <- pure $ show pointer
      error $ "hasConstantMetadata: expected local variable pointer but saw " ++ pPointer

-- | Identify the instructions in the given basic block which require local variables to be allocated to hold metadata.
--   Only LocalReference pointers require local variables allocated to hold metadata. The metadata for global references
--   and for other constant expressions of pointer type is computed as constant expressions, and so doesn't require storage.
--   Returns a list of pointer 'Operand's requiring local metadata storage.
identifyLocalMetadataAllocations :: (HasCallStack, MonadState SBCETSPassState m, MonadWriter [String] m, MonadModuleBuilder m) => BasicBlock -> m [Operand]
identifyLocalMetadataAllocations (BasicBlock _ i t) = do
  isites <- liftM concat $ mapM instAllocations i
  tsites <- termAllocations t
  return (nub $ sort (isites ++ tsites))
  where
    instAllocations inst
      -- Case 1: If a pointer is loaded from memory, local metadata storage is required to hold the pointer's metadata. Local metadata also needs to be allocated for the load address if it is not a constant (or global).
      | (v := o) <- inst, (Load _ addr _ _ _) <- o = do
          enable <- gets (CLI.instrumentLoad . options)
          if enable
          then do
            ta <- typeOf addr
            case ta of
              (Left s) -> error $ "identifyLocalMetadataAllocations: failed to compute type of argument to instruction " ++ show o ++ " (" ++ s ++ ")"
              (Right ta') -> do
                if (Helpers.isPointerType $ pointerReferent ta') -- We are loading a pointer
                then do
                  if (not $ Helpers.isFunctionType $ pointerReferent $ pointerReferent ta') -- TODO-IMPROVE: We don't currently instrument function pointers
                  then do
                    if (Helpers.isConstantOperand addr) -- Are we loading from a constant address?
                    then do
                      if (Helpers.isGlobalReference $ Helpers.walk addr) -- Are we loading from a global address?
                      then do
                        let addr' = Helpers.walk addr
                        modify $ \s -> s { currentFunctionGlobals = Data.Set.insert addr' $ currentFunctionGlobals s }
                        return [LocalReference (pointerReferent ta') v] -- Load address is a global pointer (with constant metadata) so only need to allocate metadata storage for the loaded pointer
                      else return [LocalReference (pointerReferent ta') v] -- Load address is a constant pointer (with constant metadata) so only need to allocate metadata storage for the loaded pointer
                    else return [addr, LocalReference (pointerReferent ta') v] -- Loading from a local address so we need metadata for both address and result
                  else return [] -- TODO-IMPROVE: We don't currently instrument function pointers
                else do -- Not loading a pointer, but we still need to check the load itself
                  if (Helpers.isConstantOperand addr)
                  then return [] -- Loading from a constant address, with constant metadata
                  else return [addr] -- Loading from a local address, local metadata required
          else return []

      -- Case 2: If a function is called and pointer arguments are passed, the metadata for those pointer arguments must be available in local variables if they are locals.
      -- Additionally, if a pointer is returned, it is by definition local, and so local variables must be allocated to hold the metadata for that pointer.
      | (v := o) <- inst, (Call _ _ _ (Right (ConstantOperand (Const.GlobalReference (PointerType (FunctionType rt _ False) _) fname@(Name {})))) opds _ _) <- o = do
          enable <- gets (CLI.instrumentCall . options)
          ignore <- isIgnoredFunction fname
          if (enable && not ignore)
          then do
            let lrArgs = map fst opds
            let ptrRet = if (Helpers.isPointerType rt) then [LocalReference rt v] else []
            argTys <- mapM typeOf lrArgs
            if all isRight argTys
            then do
              let ptrArgs = map snd $
                            filter (not . Helpers.isFunctionType . pointerReferent . fst) $ -- TODO-IMPROVE: We don't currently instrument function pointers
                            filter (Helpers.isPointerType . fst) $
                            zip (rights argTys) lrArgs
              let globals = filter Helpers.isGlobalReference $ map Helpers.walk $ filter Helpers.isConstantOperand ptrArgs
              let locals = filter (not . Helpers.isConstantOperand) ptrArgs
              modify $ \s -> s { currentFunctionGlobals = Data.Set.union (Data.Set.fromList globals) $ currentFunctionGlobals s }
              return (locals ++ ptrRet)
            else do
              let s = head $ lefts argTys
              error $ "identifyLocalMetadataAllocations: failed to compute type of function parameter (" ++ s ++ ")"
          else do
            tell ["identifyLocalMetadataAllocations: ignoring function " ++ show fname ]
            return []

      -- Case 3a: Local metadata must be available for all local incoming values to a phi instruction of pointer type.
      -- It is not enough to leave the metadata for arguments in constants, because we have to generate phi instructions to
      -- resolve the metadata, and the types of incoming values must match for the IR to be well-formed.
      | (_ := o) <- inst, (Phi (PointerType ty _) incoming _) <- o = do
          if (not $ Helpers.isFunctionType ty) -- TODO-IMPROVE: We don't currently instrument function pointers
          then do
            let incomingV = map fst incoming
            let incomingG = filter Helpers.isGlobalReference $ map Helpers.walk $ filter Helpers.isConstantOperand incomingV
            modify $ \s -> s { currentFunctionGlobals = Data.Set.union (Data.Set.fromList incomingG) $ currentFunctionGlobals s }
            return incomingV
          else return []

      -- Case 3b: Local metadata must be available for all local incoming values to a select instruction of pointer type.
      -- It is not enough to leave the metadata for arguments in constants, because we have to generate select instructions to
      -- resolve the metadata, and the types of incoming values must match for the IR to be well-formed.
      | (_ := o) <- inst, (Select _ tv fv _) <- o = do
          selTy <- typeOf tv
          case selTy of
            (Left s) -> do
              error $ "identifyLocalMetadataAllocations: failed to compute type of select instruction argument (" ++ s ++ ")"
            (Right selTy') -> do
              if (Helpers.isPointerType selTy' && (not $ Helpers.isFunctionType $ pointerReferent selTy')) -- TODO-IMPROVE: We don't currently instrument function pointers
              then do
                let incomingV = [tv, fv]
                let incomingG = filter Helpers.isGlobalReference $ map Helpers.walk $ filter Helpers.isConstantOperand incomingV
                modify $ \s -> s { currentFunctionGlobals = Data.Set.union (Data.Set.fromList incomingG) $ currentFunctionGlobals s }
                return incomingV
              else return []

      -- Case 4: If we allocate anything on the stack, the result is a local pointer to it, which needs metadata.
      | (v := o) <- inst, (Alloca ty _ _ _) <- o = do
          enable <- gets (CLI.instrumentStack . options)
          if (enable && (not $ Helpers.isFunctionType ty)) then return [LocalReference (ptr ty) v] else return [] -- TODO-IMPROVE: We don't currently instrument function pointers

      -- Case 5: Case 2 for void functions (no possibility of returning a pointer here).
      | (Do o) <- inst, (Call _ _ _ (Right (ConstantOperand (Const.GlobalReference (PointerType (FunctionType _ _ False) _) fname@(Name {})))) opds _ _) <- o = do
          enable <- gets (CLI.instrumentCall . options)
          ignore <- isIgnoredFunction fname
          if (enable && not ignore)
          then do
            let lrArgs = map fst opds
            argTys <- mapM typeOf lrArgs
            if all isRight argTys
            then do
              let ptrArgs = map snd $
                            filter (not . Helpers.isFunctionType . pointerReferent . fst) $ -- TODO-IMPROVE: We don't currently instrument function pointers
                            filter (Helpers.isPointerType . fst) $
                            zip (rights argTys) lrArgs
              let globals = filter Helpers.isGlobalReference $ map Helpers.walk $ filter Helpers.isConstantOperand ptrArgs
              let locals = filter (not . Helpers.isConstantOperand) ptrArgs
              modify $ \s -> s { currentFunctionGlobals = Data.Set.union (Data.Set.fromList globals) $ currentFunctionGlobals s }
              return locals
            else do
              let s = head $ lefts argTys
              error $ "identifyLocalMetadataAllocations: failed to compute type of function parameter (" ++ s ++ ")"
          else do
            tell ["identifyLocalMetadataAllocations: ignoring function " ++ show fname ]
            return []

      -- Case 7: Local metadata needs to be available for the target of a store instruction (and the source if it is a local pointer)
      | (Do o) <- inst, (Store _ tgt src _ _ _) <- o = do
          enable <- gets (CLI.instrumentStore . options)
          if enable
          then do
            ta <- typeOf tgt
            case ta of
              (Left s) -> error $ "identifyLocalMetadataAllocations: failed to compute type of argument to instruction " ++ show o ++ " (" ++ s ++ ")"
              (Right ta') -> do
                if (Helpers.isPointerType $ pointerReferent ta') -- We are storing a pointer
                then do
                  if (not $ Helpers.isFunctionType $ pointerReferent $ pointerReferent ta') -- TODO-IMPROVE: We don't currently instrument function pointers
                  then do
                    if (Helpers.isConstantOperand src) -- We are storing a constant pointer
                    then do
                      if (Helpers.isGlobalReference $ Helpers.walk src) -- We are storing a global pointer
                      then do
                        let src' = Helpers.walk src
                        modify $ \s -> s { currentFunctionGlobals = Data.Set.insert src' $ currentFunctionGlobals s }
                        return [tgt] -- Stored pointer is a global (with constant metadata) so only need to allocate metadata storage for the store target pointer
                      else return [tgt] -- Stored pointer is a constant (with constant metadata) so only need to allocate metadata storage for the store target pointer
                    else do -- We are storing a local pointer. Are we storing it to a local or a global?
                      if (Helpers.isConstantOperand tgt)
                      then return [src]
                      else return [src, tgt]
                  else return [] -- TODO-IMPROVE: We don't currently instrument function pointers
                else do -- Not storing a pointer, but we still require metadata to check the store itself
                  if (Helpers.isConstantOperand tgt) -- Are we storing to a global address?
                  then return [] -- Yes: we have constant metadata and so we don't need to allocate anything
                  else return [tgt] -- No: we need to have local metadata for the store target address
          else return []
      | otherwise = return []

    termAllocations term
      -- Case 6: If we return a pointer, we need to push the pointer's metadata to the shadow stack, so it must be available in local variables if the pointer is a local pointer.
      | (Do (Ret (Just x) _)) <- term = do
          tx <- typeOf x
          case tx of
            (Left s) -> error $ "identifyLocalMetadataAllocations: failed to compute type of return argument (" ++ s ++ ")"
            (Right tx') -> do
              if (Helpers.isPointerType tx') -- We are returning a pointer
              then do
                if (not $ Helpers.isFunctionType $ pointerReferent tx') -- TODO-IMPROVE: We don't currently instrument function pointers
                then do
                  if (Helpers.isConstantOperand x)
                  then do
                    if (Helpers.isGlobalReference $ Helpers.walk x)
                    then do
                      let x' = Helpers.walk x
                      modify $ \s -> s { currentFunctionGlobals = Data.Set.insert x' $ currentFunctionGlobals s }
                      return [] -- We are returning a global pointer which has constant metadata so we need no local metadata allocation
                    else return [] -- We are returning a constant pointer which has constant metadata so we need no local metadata allocation
                  else return [x] -- We are returning a local pointer, we must have local metadata allocated
                else return [] -- TODO-IMPROVE: We don't currently instrument function pointers
              else return [] -- We are not returning a pointer
      | otherwise = return []

-- | Emit the declaration of a runtime API function.
emitRuntimeAPIFunctionDecl :: (HasCallStack, MonadModuleBuilder m) => (Name, Type) -> m ()
emitRuntimeAPIFunctionDecl decl
  | (fname, (FunctionType retType argTypes _)) <- decl = do
      _ <- extern fname argTypes retType
      return ()
  | otherwise = undefined

-- | Emit a call to a runtime API function.
emitRuntimeAPIFunctionCall :: (HasCallStack, MonadIRBuilder m, MonadState SBCETSPassState m, MonadModuleBuilder m) => String -> [Operand] -> m Operand
emitRuntimeAPIFunctionCall n args = do
  (fname, fproto) <- gets ((!! (mkName n)) . runtimeFunctionPrototypes)
  call (ConstantOperand $ Const.GlobalReference (ptr fproto) fname) $ map (\x -> (x, [])) args

-- | Load the metadata for the pointer stored at the given memory address (first argument) from the runtime, if necessary. Place the metadata into the storage associated with the given pointer (second argument).
writeMetadataForAddressToLocalVariables :: (HasCallStack, MonadState SBCETSPassState m, MonadWriter [String] m, MonadIRBuilder m, MonadModuleBuilder m) => Operand -> Operand -> m ()
writeMetadataForAddressToLocalVariables addr loadedPtr
  | (LocalReference {}) <- addr = do -- Loading a pointer from memory
      haveStorage <- hasLocalVariableMetadata loadedPtr
      if haveStorage
      then do
        addr' <- bitcast addr (ptr i8)
        meta <- getLocalVariableMetadata loadedPtr
        _ <- emitRuntimeAPIFunctionCall "__softboundcets_metadata_load" [addr', (vBase meta), (vBound meta), (vKey meta), (vLock meta)]
        emitCheck <- gets (CLI.emitChecks . options)
        when emitCheck $ do
          _ <- emitRuntimeAPIFunctionCall "__softboundcets_metadata_check" [(vBase meta), (vBound meta), (vKey meta), (vLock meta)]
          return ()
        return ()
      else return ()

  | (ConstantOperand {}) <- addr = do -- Loading a global variable pointer or explicit constant address
      haveStorage <- hasLocalVariableMetadata loadedPtr
      if haveStorage
      then do
        meta <- inspect addr
        if isJust meta
        then do
          case (snd $ fromJust meta) of
            (Right meta'@(Constant {})) -> do
              baseReg <- pure $ ConstantOperand $ cBase meta'
              boundReg <- pure $ ConstantOperand $ cBound meta'
              keyReg <- pure $ ConstantOperand $ cKey meta'
              lockReg <- pure $ ConstantOperand $ cLock meta'
              _ <- emitMetadataStoreToLocalVariables loadedPtr (baseReg, boundReg, keyReg, lockReg)
              return ()
            (Right meta'@(Global {})) -> do
              baseReg <- pure $ ConstantOperand $ gBase meta'
              boundReg <- pure $ ConstantOperand $ gBound meta'
              keyReg <- pure $ ConstantOperand $ gKey meta'
              glp <- gets (fromJust . globalLockPtr)
              lockReg <- load glp 0
              _ <- emitMetadataStoreToLocalVariables loadedPtr (baseReg, boundReg, keyReg, lockReg)
              return ()
            (Left _) -> error $ "writeMetadataForAddressToLocalVariables: encountered constant pointer with non-constant metadata"
        else do
          pAddr <- pure $ show addr
          tell ["writeMetadataForAddressToLocalVariables: not instrumenting unsupported pointer " ++ pAddr]
          return ()
      else return ()

  | (MetadataOperand {}) <- addr = do -- Loading a metadata pointer, not handled
      pAddr <- pure $ show addr
      tell ["writeMetadataForAddressToLocalVariables: not instrumenting unsupported pointer " ++ pAddr]
      return ()

-- | Create a local key and lock for entities allocated in the current stack frame
emitLocalKeyAndLockCreation :: (HasCallStack, MonadState SBCETSPassState m, MonadIRBuilder m, MonadModuleBuilder m) => m ()
emitLocalKeyAndLockCreation = do
  keyPtr <- (alloca i64 Nothing 8) `named` (fromString "sbcets.stack_frame_key")
  lockPtr <- (alloca (ptr i8) Nothing 8) `named` (fromString "sbcets.stack_frame_lock")
  _ <- emitRuntimeAPIFunctionCall "__softboundcets_create_stack_key" [lockPtr, keyPtr]
  modify $ \s -> s { localStackFrameKeyPtr = Just keyPtr, localStackFrameLockPtr = Just lockPtr }
  return ()

-- | Invalidate the local key; We do this just prior to returning from a function.
emitLocalKeyAndLockDestruction :: (HasCallStack, MonadState SBCETSPassState m, MonadIRBuilder m, MonadModuleBuilder m) => m ()
emitLocalKeyAndLockDestruction = do
  keyPtr <- gets (fromJust . localStackFrameKeyPtr)
  keyReg <- load keyPtr 0
  _ <- emitRuntimeAPIFunctionCall "__softboundcets_destroy_stack_key" [keyReg]
  return ()

-- | Allocate space on the shadow stack for the parameters of an instrumented function we are about to call.
emitShadowStackAllocation :: (HasCallStack, MonadState SBCETSPassState m, MonadIRBuilder m, MonadModuleBuilder m) => Integer -> m ()
emitShadowStackAllocation numArgs = do
  numArgs' <- pure $ int32 numArgs
  _ <- emitRuntimeAPIFunctionCall "__softboundcets_allocate_shadow_stack_space" [numArgs']
  return ()

-- | Deallocate the shadow stack space for the instrumented function which just returned.
emitShadowStackDeallocation :: (HasCallStack, MonadState SBCETSPassState m, MonadIRBuilder m, MonadModuleBuilder m) => m ()
emitShadowStackDeallocation = do
  _ <- emitRuntimeAPIFunctionCall "__softboundcets_deallocate_shadow_stack_space" []
  return ()

-- | Load the metadata for a pointer function parameter from the shadow stack.
emitMetadataLoadFromShadowStack :: (HasCallStack, MonadState SBCETSPassState m, MonadIRBuilder m, MonadModuleBuilder m) => Operand -> Integer -> m VariableMetadata
emitMetadataLoadFromShadowStack p ix = do
  ix' <- pure $ int32 ix
  baseReg <- emitRuntimeAPIFunctionCall "__softboundcets_load_base_shadow_stack" [ix']
  boundReg <- emitRuntimeAPIFunctionCall "__softboundcets_load_bound_shadow_stack" [ix']
  keyReg <- emitRuntimeAPIFunctionCall "__softboundcets_load_key_shadow_stack" [ix']
  lockReg <- emitRuntimeAPIFunctionCall "__softboundcets_load_lock_shadow_stack" [ix']
  emitMetadataStoreToLocalVariables p (baseReg, boundReg, keyReg, lockReg)

-- | Store the metadata in registers for the given pointer into the local variables allocated to hold it.
emitMetadataStoreToLocalVariables :: (HasCallStack, MonadState SBCETSPassState m, MonadIRBuilder m, MonadModuleBuilder m) => Operand -> (Operand, Operand, Operand, Operand) -> m VariableMetadata
emitMetadataStoreToLocalVariables p (baseReg, boundReg, keyReg, lockReg) = do
  meta <- getLocalVariableMetadata p
  store (vBase meta) 8 baseReg
  store (vBound meta) 8 boundReg
  store (vKey meta) 8 keyReg
  store (vLock meta) 8 lockReg
  gen p (baseReg, boundReg, keyReg, lockReg) -- We just generated these registers
  return meta

-- | Store the metadata for a pointer on the shadow stack at the specified position.
emitMetadataStoreToShadowStack :: (HasCallStack, MonadModuleBuilder m, MonadState SBCETSPassState m, MonadWriter [String] m, MonadIRBuilder m) => Maybe Name -> Operand -> Integer -> m ()
emitMetadataStoreToShadowStack callee p ix = do
  meta <- inspect p
  if isNothing meta
  then do
    fname <- gets (show . name . fromJust . currentFunction)
    pp <- pure $ show p
    if isJust callee
    then tell ["emitMetadataStoreToShadowStack: in function " ++ fname ++ ": using don't-care metadata for uninstrumented pointer " ++ pp ++ " passed to instrumented function " ++ (show $ fromJust callee)]
    else tell ["emitMetadataStoreToShadowStack: in function " ++ fname ++ ": using don't-care metadata for uninstrumented pointer " ++ pp ++ " being returned from instrumented function"]
    meta' <- generateDCMetadata
    ix' <- pure $ int32 ix
    baseReg <- pure $ ConstantOperand $ cBase meta'
    boundReg <- pure $ ConstantOperand $ cBound meta'
    keyReg <- pure $ ConstantOperand $ cKey meta'
    lockReg <- pure $ ConstantOperand $ cLock meta'
    _ <- emitRuntimeAPIFunctionCall "__softboundcets_store_base_shadow_stack" [baseReg, ix']
    _ <- emitRuntimeAPIFunctionCall "__softboundcets_store_bound_shadow_stack" [boundReg, ix']
    _ <- emitRuntimeAPIFunctionCall "__softboundcets_store_key_shadow_stack" [keyReg, ix']
    _ <- emitRuntimeAPIFunctionCall "__softboundcets_store_lock_shadow_stack" [lockReg, ix']
    return ()
  else do
    let meta' = snd $ fromJust meta
    case meta' of
      Left variableMeta -> do
        ix' <- pure $ int32 ix
        baseReg <- reload baseRegisterMetadata (vBase variableMeta) p
        boundReg <- reload boundRegisterMetadata (vBound variableMeta) p
        keyReg <- reload keyRegisterMetadata (vKey variableMeta) p
        lockReg <- reload lockRegisterMetadata (vLock variableMeta) p
        gen p (baseReg, boundReg, keyReg, lockReg) -- We just generated these registers
        _ <- emitRuntimeAPIFunctionCall "__softboundcets_store_base_shadow_stack" [baseReg, ix']
        _ <- emitRuntimeAPIFunctionCall "__softboundcets_store_bound_shadow_stack" [boundReg, ix']
        _ <- emitRuntimeAPIFunctionCall "__softboundcets_store_key_shadow_stack" [keyReg, ix']
        _ <- emitRuntimeAPIFunctionCall "__softboundcets_store_lock_shadow_stack" [lockReg, ix']
        return ()
      Right constMeta@(Constant {}) -> do
        ix' <- pure $ int32 ix
        baseReg <- pure $ ConstantOperand $ cBase constMeta
        boundReg <- pure $ ConstantOperand $ cBound constMeta
        keyReg <- pure $ ConstantOperand $ cKey constMeta
        lockReg <- pure $ ConstantOperand $ cLock constMeta
        _ <- emitRuntimeAPIFunctionCall "__softboundcets_store_base_shadow_stack" [baseReg, ix']
        _ <- emitRuntimeAPIFunctionCall "__softboundcets_store_bound_shadow_stack" [boundReg, ix']
        _ <- emitRuntimeAPIFunctionCall "__softboundcets_store_key_shadow_stack" [keyReg, ix']
        _ <- emitRuntimeAPIFunctionCall "__softboundcets_store_lock_shadow_stack" [lockReg, ix']
        return ()
      Right constMeta@(Global {}) -> do
        ix' <- pure $ int32 ix
        baseReg <- pure $ ConstantOperand $ gBase constMeta
        boundReg <- pure $ ConstantOperand $ gBound constMeta
        keyReg <- pure $ ConstantOperand $ gKey constMeta
        glp <- gets (fromJust . globalLockPtr)
        lockReg <- load glp 0
        _ <- emitRuntimeAPIFunctionCall "__softboundcets_store_base_shadow_stack" [baseReg, ix']
        _ <- emitRuntimeAPIFunctionCall "__softboundcets_store_bound_shadow_stack" [boundReg, ix']
        _ <- emitRuntimeAPIFunctionCall "__softboundcets_store_key_shadow_stack" [keyReg, ix']
        _ <- emitRuntimeAPIFunctionCall "__softboundcets_store_lock_shadow_stack" [lockReg, ix']
        return ()

-- | Helper function for select instruction incoming value metadata access
selectMeta :: (HasCallStack, MonadModuleBuilder m, MonadState SBCETSPassState m, MonadWriter [String] m, MonadIRBuilder m) => Named Instruction -> Operand -> m (Either VariableMetadata ConstantMetadata)
selectMeta i p = do
  meta <- inspect p
  if isNothing meta
  then do
    pAddr <- pure $ show p
    pFunc <- gets (show . name . fromJust . currentFunction)
    pInst <- pure $ show i
    tell ["instrumentInst: in function " ++ pFunc ++ ": uninstrumented (using don't-care metadata) incoming value " ++ pAddr ++ " in " ++ pInst]
    meta' <- gets (fromJust . localDCMetadata) -- Select metadata must reside in local variables
    return $ Left meta'
  else do
    let meta' = snd $ fromJust meta
    case meta' of
      (Left _) -> return meta'
      (Right _) -> do
        pAddr <- pure $ show p
        pFunc <- gets (show . name . fromJust . currentFunction)
        pInst <- pure $ show i
        error $ "instrumentInst: in function " ++ pFunc ++ ": variable metadata not allocated for instrumented incoming value " ++ pAddr ++ " in " ++ pInst

-- | Helper function for phi-node incoming value metadata access
phiMeta :: (HasCallStack, MonadModuleBuilder m, MonadState SBCETSPassState m, MonadWriter [String] m, MonadIRBuilder m) => Named Instruction -> (Operand, Name) -> m (Either VariableMetadata ConstantMetadata, Name)
phiMeta i (p, n) = do
  meta <- inspect p
  if isNothing meta
  then do
    pAddr <- pure $ show p
    pFunc <- gets (show . name . fromJust . currentFunction)
    pInst <- pure $ show i
    tell ["instrumentInst: in function " ++ pFunc ++ ": uninstrumented (using don't-care metadata) incoming value " ++ pAddr ++ " in " ++ pInst]
    meta' <- gets (fromJust . localDCMetadata) -- Phi metadata must reside in local variables
    return (Left meta', n)
  else do
    let meta' = snd $ fromJust meta
    case meta' of
      (Left _) -> return (meta', n)
      (Right _) -> do
        pAddr <- pure $ show p
        pFunc <- gets (show . name . fromJust . currentFunction)
        pInst <- pure $ show i
        error $ "instrumentInst: in function " ++ pFunc ++ ": variable metadata not allocated for instrumented incoming value " ++ pAddr ++ " in " ++ pInst

-- | Instrument a given module according to the supplied command-line options and list of blacklisted function symbols.
instrument :: HasCallStack => [String] -> CLI.Options -> Module -> IO Module
instrument blacklist' opts m = do
  let sbcetsState = initSBCETSPassState { options = opts, blacklist = Data.Set.fromList $ map mkName blacklist' }
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

    instrumentDefinition :: (HasCallStack, MonadIRBuilder m, MonadState SBCETSPassState m, MonadWriter [String] m, MonadModuleBuilder m) => Definition -> m ()
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
          else do
            tell ["instrumentDefinition: ignoring function " ++ (show $ name f) ]
            emitDefn g
      | (GlobalDefinition gv@(GlobalVariable {})) <- g = do
          emitDefn g
          instrumentGlobalVariable gv
      | otherwise = emitDefn g

    instrumentGlobalVariable :: (HasCallStack, MonadIRBuilder m, MonadState SBCETSPassState m, MonadWriter [String] m, MonadModuleBuilder m) => Global -> m ()
    instrumentGlobalVariable g
      | (GlobalVariable {}) <- g, name g == (Name $ fromString "llvm.global_ctors") = return () -- https://llvm.org/docs/LangRef.html#the-llvm-global-ctors-global-variable
      | (GlobalVariable {}) <- g, name g == (Name $ fromString "llvm.global_dtors") = return () -- https://llvm.org/docs/LangRef.html#the-llvm-global-dtors-global-variable
      | (GlobalVariable {}) <- g, section g == (Just $ fromString "llvm.metadata")  = return () -- LLVM puts metadata in a specially named section
      | (GlobalVariable {}) <- g, isNothing $ initializer g                         = return () -- Uninitialized globals do not get metadata
      | (GlobalVariable {}) <- g = do
          let gName = name g
          gType <- typeOf (fromJust $ initializer g)
          case gType of
            (Left s) -> error $ "instrumentGlobalVariable: could not compute type of initializer (" ++ s ++ ")"
            (Right gType') -> do
              let gConst = Const.GlobalReference (ptr gType') gName
              shouldUseDontCareMetadata <- gets (CLI.benchmarkMode . options)
              if shouldUseDontCareMetadata
              then do
                dcConstMeta <- generateDCMetadata
                let gMeta = Global (cBase dcConstMeta) (cBound dcConstMeta) (cKey dcConstMeta)
                let gPtr = ConstantOperand gConst
                modify $ \s -> s { globalMetadata = Data.Map.insert gPtr gMeta $ globalMetadata s }
                mark gPtr Safe -- The address of a global variable is always safe
              else do
                let baseConst = if Helpers.isPointerType gType'
                                then Const.BitCast (Const.GetElementPtr False (fromJust $ initializer g) [Const.Int 64 0]) (ptr i8)
                                else Const.BitCast (Const.GetElementPtr False gConst [Const.Int 64 0]) (ptr i8)
                let boundConst = if Helpers.isPointerType gType'
                                then Const.BitCast (Const.GetElementPtr False (fromJust $ initializer g) [Const.Int 64 1]) (ptr i8)
                                else Const.BitCast (Const.GetElementPtr False gConst [Const.Int 64 1]) (ptr i8)
                let keyConst = Const.Int 64 1
                let gMeta = Global baseConst boundConst keyConst
                let gPtr = ConstantOperand gConst
                modify $ \s -> s { globalMetadata = Data.Map.insert gPtr gMeta $ globalMetadata s }
                mark gPtr Safe -- The address of a global variable is always safe
      | otherwise = do
        pg <- pure $ show g
        error $ "instrumentGlobalVariable: expected global variable, but got: " ++ pg

    instrumentFunction :: (HasCallStack, MonadIRBuilder m, MonadState SBCETSPassState m, MonadWriter [String] m, MonadModuleBuilder m) => Global -> m ()
    instrumentFunction f
      | (Function {}) <- f = do
          let name' = if name f == mkName "main" then mkName "softboundcets_main" else name f
          (_, blocks) <- runIRBuilderT emptyIRBuilder { builderNameSuggestion = Just $ fromString "sbcets" } $ do
            classifications' <- gets classifications
            modify $ \s -> s { globalLockPtr = Nothing
                             , localStackFrameKeyPtr = Nothing
                             , localStackFrameLockPtr = Nothing
                             , currentFunction = Just f
                             , currentFunctionGlobals = Data.Set.empty
                             , localVariableMetadata = Data.Map.empty
                             , baseRegisterMetadata = Data.Map.empty
                             , boundRegisterMetadata = Data.Map.empty
                             , keyRegisterMetadata = Data.Map.empty
                             , lockRegisterMetadata = Data.Map.empty
                             }
            let firstBlockLabel = (\(BasicBlock n _ _) -> n) $ head $ basicBlocks f
            -- Create the metadata for any non-function type pointer parameters
            let params = fst $ parameters f
            paramTys <- mapM typeOf params
            if any isLeft paramTys
            then do
              let s = head $ lefts paramTys
              error $ "instrumentFunction: could not compute type of function parameter (" ++ s ++ ")"
            else do
              let pointerArguments = map (\(_, Parameter t n _) -> (LocalReference t n)) $
                                     filter (not . Helpers.isFunctionType . pointerReferent . fst) $
                                     filter (Helpers.isPointerType . fst) $
                                     zip (rights paramTys) params
              let shadowStackIndices :: [Integer] = [1..]
              emitBlockStart (mkName "sbcets_metadata_init")
              mapM_ allocateLocalVariableMetadataStorage pointerArguments
              _ <- zipWithM emitMetadataLoadFromShadowStack pointerArguments shadowStackIndices
              -- Collect all metadata allocation sites so we can allocate local variables for metadata ahead of time
              pointersRequiringLocalMetadata <- liftM (Data.Set.fromList . map Helpers.walk . concat) $ mapM identifyLocalMetadataAllocations $ basicBlocks f
              mapM_ allocateLocalVariableMetadataStorage $ filter (not . flip elem pointerArguments) $ Data.Set.toList pointersRequiringLocalMetadata
              -- FIXME: Here we need to write the metadata to local variables for all those globals which require local variable metadata due to being passed to select and phi instructions (the local variable storage was just allocated in allocateLocalVariableMetadataStorage)
              -- Otherwise, the local metadata for those global variables contains whatever garbage is left there by alloca
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
      -- Set up a handle to the global lock pointer
      gl <- emitRuntimeAPIFunctionCall "__softboundcets_get_global_lock" []
      glp <- (alloca (ptr i8) Nothing 8) `named` (fromString "sbcets.global_lock")
      store glp 8 gl
      modify $ \s -> s { globalLockPtr = Just glp }
      -- Create a lock for local allocations
      emitLocalKeyAndLockCreation
      -- Create local variable don't-care metadata
      dcBasePtr <- (alloca (ptr i8) Nothing 8) `named` (fromString "sbcets.dc.base")
      dcBoundPtr <- (alloca (ptr i8) Nothing 8) `named` (fromString "sbcets.dc.bound")
      dcKeyPtr <- (alloca (i64) Nothing 8) `named` (fromString "sbcets.dc.key")
      dcLockPtr <- (alloca (ptr i8) Nothing 8) `named` (fromString "sbcets.dc.lock")
      dcConstMeta <- generateDCMetadata
      store dcBasePtr 8 (ConstantOperand $ cBase dcConstMeta)
      store dcBoundPtr 8 (ConstantOperand $ cBound dcConstMeta)
      store dcKeyPtr 8 (ConstantOperand $ cKey dcConstMeta)
      store dcLockPtr 8 (ConstantOperand $ cLock dcConstMeta)
      let dcVarMeta = Variable dcBasePtr dcBoundPtr dcKeyPtr dcLockPtr
      modify $ \s -> s { localDCMetadata = Just dcVarMeta }
      -- Create local variable null metadata
      nullBasePtr <- (alloca (ptr i8) Nothing 8) `named` (fromString "sbcets.null.base")
      nullBoundPtr <- (alloca (ptr i8) Nothing 8) `named` (fromString "sbcets.null.bound")
      nullKeyPtr <- (alloca (i64) Nothing 8) `named` (fromString "sbcets.null.key")
      nullLockPtr <- (alloca (ptr i8) Nothing 8) `named` (fromString "sbcets.null.lock")
      nullConstMeta <- generateNullMetadata
      store nullBasePtr 8 (ConstantOperand $ cBase nullConstMeta)
      store nullBoundPtr 8 (ConstantOperand $ cBound nullConstMeta)
      store nullKeyPtr 8 (ConstantOperand $ cKey nullConstMeta)
      store nullLockPtr 8 (ConstantOperand $ cLock nullConstMeta)
      let nullVarMeta = Variable nullBasePtr nullBoundPtr nullKeyPtr nullLockPtr
      modify $ \s -> s { localNullMetadata = Just nullVarMeta }
      -- Instrument the rest of the block
      mapM_ instrumentInst i
      instrumentTerm t

    instrumentBlock (BasicBlock n i t) = do
      emitBlockStart n
      baseRegisterMetadata' <- gets baseRegisterMetadata
      boundRegisterMetadata' <- gets boundRegisterMetadata
      keyRegisterMetadata' <- gets keyRegisterMetadata
      lockRegisterMetadata' <- gets lockRegisterMetadata
      mapM_ instrumentInst i
      instrumentTerm t
      modify $ \s -> s { baseRegisterMetadata = baseRegisterMetadata',
                         boundRegisterMetadata = boundRegisterMetadata',
                         keyRegisterMetadata = keyRegisterMetadata',
                         lockRegisterMetadata = lockRegisterMetadata' }

    instrumentInst i@(v := o)
      | (Alloca ty count _ _) <- o = do
        -- We emit the alloca first because we reference the result in the instrumentation
        Helpers.emitNamedInstStripMeta ["tbaa"] i
        when (not $ Helpers.isFunctionType ty) $ do -- TODO-IMPROVE: We don't currently instrument function pointers
          let resultPtr = LocalReference (ptr ty) v
            -- The address of a stack allocation is always safe
          mark resultPtr Safe
          enable <- gets (CLI.instrumentStack . options)
          when enable $ do
            shouldUseDontCareMetadata <- gets (CLI.benchmarkMode . options)
            if shouldUseDontCareMetadata
            then do
              meta' <- generateDCMetadata
              baseReg <- pure $ ConstantOperand $ cBase meta'
              boundReg <- pure $ ConstantOperand $ cBound meta'
              keyReg <- pure $ ConstantOperand $ cKey meta'
              lockReg <- pure $ ConstantOperand $ cLock meta'
              _ <- emitMetadataStoreToLocalVariables resultPtr (baseReg, boundReg, keyReg, lockReg)
              return ()
            else do
              eltSize <- sizeof 64 ty
              intCount <- if isJust count
                          then do
                            tc <- typeOf $ fromJust count
                            case tc of
                              (Left s) -> do
                                pFunc <- gets (show . name . fromJust . currentFunction)
                                pInst <- pure $ show i
                                error $ "instrumentInst: in function "++ pFunc ++ ": could not compute type of alloca count parameter in " ++ pInst ++ " (" ++ s ++ ")"
                              (Right tc') -> do
                                if not (tc' == i64)
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
                tell ["in function " ++ pFunc ++ ": not instrumenting load through uninstrumented pointer " ++ pAddr ++ " in " ++ pInst]
                return ()
              else do
                let (refTy, meta') = fromJust meta
                addr' <- bitcast addr (ptr i8)
                tySize <- sizeof 64 refTy
                case meta' of
                  Left variableMeta -> do
                    baseReg <- reload baseRegisterMetadata (vBase variableMeta) addr
                    boundReg <- reload boundRegisterMetadata (vBound variableMeta) addr
                    -- Check the load is spatially in bounds
                    _ <- emitRuntimeAPIFunctionCall "__softboundcets_spatial_load_dereference_check" [baseReg, boundReg, addr', tySize]
                    -- Check the load is temporally in bounds
                    keyReg <- reload keyRegisterMetadata (vKey variableMeta) addr
                    lockReg <- reload lockRegisterMetadata (vLock variableMeta) addr
                    _ <- emitRuntimeAPIFunctionCall "__softboundcets_temporal_load_dereference_check" [lockReg, keyReg]
                    gen addr (baseReg, boundReg, keyReg, lockReg) -- We just generated these registers
                    return ()
                  Right constMeta@(Constant {}) -> do
                    baseReg <- pure $ ConstantOperand $ cBase constMeta
                    boundReg <- pure $ ConstantOperand $ cBound constMeta
                    -- Check the load is spatially in bounds
                    _ <- emitRuntimeAPIFunctionCall "__softboundcets_spatial_load_dereference_check" [baseReg, boundReg, addr', tySize]
                    -- No temporal check because constants cannot be deallocated
                    return ()
                  Right constMeta@(Global {}) -> do
                    baseReg <- pure $ ConstantOperand $ gBase constMeta
                    boundReg <- pure $ ConstantOperand $ gBound constMeta
                    -- Check the load is spatially in bounds
                    _ <- emitRuntimeAPIFunctionCall "__softboundcets_spatial_load_dereference_check" [baseReg, boundReg, addr', tySize]
                    -- No temporal check because globals cannot be deallocated
                    return ()
            Safe -> return ()

        Helpers.emitNamedInstStripMeta ["tbaa"] i

        when enable $ do -- No matter if we were able to instrument the load or not, if a pointer is being loaded, ask the runtime for metadata for the loaded address.
          ta <- typeOf addr
          case ta of
            (Left s) -> do
              pFunc <- gets (show . name . fromJust . currentFunction)
              pInst <- pure $ show i
              error $ "instrumentInst: in function " ++ pFunc ++ ": could not compute type of loaded value in " ++ pInst ++ " (" ++ s ++ ")"
            (Right ta') -> do
              when ((Helpers.isPointerType $ pointerReferent ta') &&
                    (not $ Helpers.isFunctionType $ pointerReferent $ pointerReferent ta')) $ do -- TODO-IMPROVE: We don't currently instrument function pointers
                let resultPtr = LocalReference (pointerReferent ta') v
                writeMetadataForAddressToLocalVariables addr resultPtr
              return ()

      -- Instrument a call instruction unless it is calling inline assembly or a computed function pointer.
      | (Call _ _ _ (Right (ConstantOperand (Const.GlobalReference (PointerType (FunctionType rt _ False) _) fname))) opds _ _) <- o = do
        enable <- gets (CLI.instrumentCall . options)
        ignore <- isIgnoredFunction fname
        if (not enable || ignore)
        then do
          tell ["instrumentInst: ignoring function " ++ show fname ]
          Helpers.emitNamedInstStripMeta ["tbaa"] i
        else do
          case fname of
            (Name {}) -> do -- Calling a function symbol
              let opds' = map fst opds
              opdTys <- mapM typeOf opds'
              if all isRight opdTys
              then do
                let ptrArgs = map snd $
                              filter (not . Helpers.isFunctionType . pointerReferent . fst) $
                              filter (Helpers.isPointerType . fst) $
                              zip (rights opdTys) opds'
                emitShadowStackAllocation (fromIntegral $ 1 + length ptrArgs)
                zipWithM_ (emitMetadataStoreToShadowStack $ Just fname) ptrArgs [1..]
                hasWrapper <- isWrappedFunction fname
                if hasWrapper
                then do
                  wrapperFunctionName <- gets (fst . (! fname) . stdlibWrapperPrototypes)
                  Helpers.emitNamedInstStripMeta ["tbaa"] $ v := (Helpers.rewriteCalledFunctionName wrapperFunctionName o)
                else Helpers.emitNamedInstStripMeta ["tbaa"] i
                -- TODO-OPTIMIZE: The function could potentially deallocate any pointer it is passed
                mapM_ (flip mark $ Unsafe) $ Data.Set.fromList ptrArgs
                mapM_ killLock $ Data.Set.fromList ptrArgs
                -- Read the metadata for the return value off the shadow stack if it is a pointer
                when (Helpers.isPointerType rt && (not $ Helpers.isFunctionType $ pointerReferent rt)) $ do
                  _ <- emitMetadataLoadFromShadowStack (LocalReference rt v) 0
                  safe <- returnsSafePointer fname
                  if safe
                  then mark (LocalReference rt v) Safe
                  else mark (LocalReference rt v) Unsafe
                emitShadowStackDeallocation
              else do
                let s = head $ lefts opdTys
                pFunc <- gets (show . name . fromJust . currentFunction)
                pInst <- pure $ show i
                error $ "instrumentInst: in function " ++ pFunc ++ ": could not compute type of function argument in " ++ pInst ++ " (" ++ s ++ ")"
            (UnName {}) -> do -- TODO-IMPROVE: Calling a computed function pointer. Can we map this to a function symbol?
              Helpers.emitNamedInstStripMeta ["tbaa"] i

      | (GetElementPtr _ addr ixs _) <- o = do
        ta <- typeOf addr
        case ta of
          (Left s) -> do
            pFunc <- gets (show . name . fromJust . currentFunction)
            pInst <- pure $ show i
            error $ "instrumentInst: in function " ++ pFunc ++ ": could not compute type of argument to getelementptr instruction in " ++ pInst ++ " (" ++ s ++ ")"
          (Right ta') -> do
            refTy <- indexTypeByOperands ta' ixs
            case refTy of
              (Left s) -> do
                -- TODO-IMPROVE: Softboundcets doesn't handle opaque structure types (https://llvm.org/docs/LangRef.html#opaque-structure-types) but we could do so.
                pAddr <- pure $ show addr
                pIxs <- pure $ map show ixs
                tell ["Unable to compute type of getelementptr result: " ++ (show ta') ++ " " ++ pAddr ++ " [" ++ (intercalate ", " pIxs) ++ "]"]
                tell [s]
                return ()
              (Right ptrTy) -> do
                let gepResultPtr = LocalReference ptrTy v
                meta <- inspect addr
                if isNothing meta
                then do
                  pAddr <- pure $ show addr
                  pFunc <- gets (show . name . fromJust . currentFunction)
                  pInst <- pure $ show i
                  tell ["in function " ++ pFunc ++ ": not instrumenting arithmetic derivation from uninstrumented pointer " ++ pAddr ++ " in " ++ pInst]
                  return ()
                else do
                  let meta' = snd $ fromJust meta
                  case meta' of
                    Left variableMeta -> do
                      associate gepResultPtr meta' -- The pointer created by getelementptr shares metadata storage with the parent pointer
                      baseReg <- reload baseRegisterMetadata (vBase variableMeta) addr
                      boundReg <- reload boundRegisterMetadata (vBound variableMeta) addr
                      keyReg <- reload keyRegisterMetadata (vKey variableMeta) addr
                      lockReg <- reload lockRegisterMetadata (vLock variableMeta) addr
                      gen gepResultPtr (baseReg, boundReg, keyReg, lockReg) -- We just generated these registers
                      mark gepResultPtr Unsafe -- TODO-OPTIMIZE: arithmetic derived pointers are considered unconditionally unsafe (even if the parent pointer is safe)
                    Right _ -> do
                      associate gepResultPtr meta'
        Helpers.emitNamedInstStripMeta ["tbaa"] i

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
            tell ["in function " ++ pFunc ++ ": not instrumenting type derivation from uninstrumented pointer " ++ pAddr ++ " in " ++ pInst]
            return ()
          else do
            let meta' = snd $ fromJust meta
            case meta' of
              Left variableMeta -> do
                associate bitcastResultPtr meta' -- The pointer created by bitcast shares metadata storage with the parent pointer
                baseReg <- reload baseRegisterMetadata (vBase variableMeta) addr
                boundReg <- reload boundRegisterMetadata (vBound variableMeta) addr
                keyReg <- reload keyRegisterMetadata (vKey variableMeta) addr
                lockReg <- reload lockRegisterMetadata (vLock variableMeta) addr
                gen bitcastResultPtr (baseReg, boundReg, keyReg, lockReg) -- We just generated these registers
                mark bitcastResultPtr Unsafe -- TODO-OPTIMIZE: cast pointers are considered unconditionally unsafe (even if the parent pointer is safe)
              Right _ -> do
                associate bitcastResultPtr meta'
        Helpers.emitNamedInstStripMeta ["tbaa"] i

      | (Select cond tval fval _) <- o = do
        Helpers.emitNamedInstStripMeta ["tbaa"] i
        selTy <- typeOf tval
        case selTy of
          (Left s) -> do
            pFunc <- gets (show . name . fromJust . currentFunction)
            pInst <- pure $ show i
            error $ "instrumentInst: in function " ++ pFunc ++ ": could not compute type of select instruction argument in " ++ pInst ++ " (" ++ s ++ ")"
          (Right selTy') -> do
            when (Helpers.isPointerType selTy' && (not $ Helpers.isFunctionType $ pointerReferent selTy')) $ do -- TODO-IMPROVE: We don't currently instrument function pointers
              let resultPtr = LocalReference selTy' v
              tClass <- query Unsafe tval
              fClass <- query Unsafe fval
              case (tClass, fClass) of
                (Safe, Safe) -> mark resultPtr Safe
                _ -> mark resultPtr Unsafe
              tMeta <- selectMeta i tval
              fMeta <- selectMeta i fval
              base' <- select cond (baseOf tMeta) (baseOf fMeta)
              bound' <- select cond (boundOf tMeta) (boundOf fMeta)
              key' <- select cond (keyOf tMeta) (keyOf fMeta)
              tMetaLock <- lockOf tMeta
              fMetaLock <- lockOf fMeta
              lock' <- select cond tMetaLock fMetaLock
              associate resultPtr $ Left $ Variable base' bound' key' lock'
              kill resultPtr
              return ()

      | (Phi (PointerType ty _) incoming _) <- o = do
        Helpers.emitNamedInstStripMeta ["tbaa"] i
        when (not $ Helpers.isFunctionType ty) $ do -- TODO-IMPROVE: We don't currently instrument function pointers
          incomingMeta <- forM incoming (phiMeta i)
          base'  <- phi $ map (\(x, n) -> (baseOf x, n)) incomingMeta
          bound' <- phi $ map (\(x, n) -> (boundOf x, n)) incomingMeta
          key'   <- phi $ map (\(x, n) -> (keyOf x, n)) incomingMeta
          incomingLocksAndBlockNames <- mapM (\(x, n) -> do {l <- lockOf x; return (l, n)}) incomingMeta
          lock'  <- phi incomingLocksAndBlockNames
          let resultPtr = LocalReference (ptr ty) v
          associate resultPtr $ Left $ Variable base' bound' key' lock'
          kill resultPtr
          return ()

      | otherwise = Helpers.emitNamedInstStripMeta ["tbaa"] i

    instrumentInst i@(Do o)
      -- This alternative is the non-capturing variant (call ignoring return value, if any).
      -- We don't need to emit checks for the return value here because it is unused.
      | (Call _ _ _ (Right (ConstantOperand (Const.GlobalReference (PointerType (FunctionType _ _ False) _) fname))) opds _ _) <- o = do
        enable <- gets (CLI.instrumentCall . options)
        ignore <- isIgnoredFunction fname
        if (not enable || ignore)
        then do
          tell ["instrumentInst: ignoring function " ++ show fname ]
          Helpers.emitNamedInstStripMeta ["tbaa"] i
        else do
          case fname of
            (Name {}) -> do -- Calling a function symbol
              let opds' = map fst opds
              opdTys <- mapM typeOf opds'
              if all isRight opdTys
              then do
                let ptrArgs = map snd $
                              filter (not . Helpers.isFunctionType . pointerReferent . fst) $
                              filter (Helpers.isPointerType . fst) $
                              zip (rights opdTys) opds'
                emitShadowStackAllocation (fromIntegral $ 1 + length ptrArgs)
                zipWithM_ (emitMetadataStoreToShadowStack $ Just fname) ptrArgs [1..]
                hasWrapper <- isWrappedFunction fname
                if hasWrapper
                then do
                  wrapperFunctionName <- gets (fst . (! fname) . stdlibWrapperPrototypes)
                  Helpers.emitNamedInstStripMeta ["tbaa"] $ Do $ Helpers.rewriteCalledFunctionName wrapperFunctionName o
                else Helpers.emitNamedInstStripMeta ["tbaa"] i
                -- The function could potentially deallocate any pointer it is passed
                mapM_ (flip mark $ Unsafe) $ Data.Set.fromList ptrArgs
                mapM_ killLock $ Data.Set.fromList ptrArgs
                emitShadowStackDeallocation
              else do
                let s = head $ lefts opdTys
                pFunc <- gets (show . name . fromJust . currentFunction)
                pInst <- pure $ show i
                error $ "instrumentInst: in function " ++ pFunc ++ ": could not compute type of function argument in " ++ pInst ++ " (" ++ s ++ ")"
            (UnName {}) -> do -- TODO-IMPROVE: Calling a computed function pointer. Can we map this to a function symbol?
              Helpers.emitNamedInstStripMeta ["tbaa"] i

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
                tell ["in function " ++ pFunc ++ ": not instrumenting store through uninstrumented pointer " ++ pAddr ++ " in " ++ pInst]
                return ()
              else do
                let (refTy, meta') = fromJust meta
                tgt' <- bitcast tgt (ptr i8)
                tySize <- sizeof 64 refTy
                case meta' of
                  Left variableMeta -> do
                    baseReg <- reload baseRegisterMetadata (vBase variableMeta) tgt
                    boundReg <- reload boundRegisterMetadata (vBound variableMeta) tgt
                    -- Check the store is spatially in bounds
                    _ <- emitRuntimeAPIFunctionCall "__softboundcets_spatial_store_dereference_check" [baseReg, boundReg, tgt', tySize]
                    -- Check the store is temporally in bounds
                    keyReg <- reload keyRegisterMetadata (vKey variableMeta) tgt
                    lockReg <- reload lockRegisterMetadata (vLock variableMeta) tgt
                    _ <- emitRuntimeAPIFunctionCall "__softboundcets_temporal_store_dereference_check" [lockReg, keyReg]
                    gen tgt (baseReg, boundReg, keyReg, lockReg) -- We just generated these registers
                    return ()
                  Right constMeta@(Constant {}) -> do
                    baseReg <- pure $ ConstantOperand $ cBase constMeta
                    boundReg <- pure $ ConstantOperand $ cBound constMeta
                    -- Check the store is spatially in bounds
                    _ <- emitRuntimeAPIFunctionCall "__softboundcets_spatial_store_dereference_check" [baseReg, boundReg, tgt', tySize]
                    -- No temporal check because constants cannot be deallocated
                    return ()
                  Right constMeta@(Global {}) -> do
                    baseReg <- pure $ ConstantOperand $ gBase constMeta
                    boundReg <- pure $ ConstantOperand $ gBound constMeta
                    -- Check the store is spatially in bounds
                    _ <- emitRuntimeAPIFunctionCall "__softboundcets_spatial_store_dereference_check" [baseReg, boundReg, tgt', tySize]
                    -- No temporal check because globals cannot be deallocated
                    return ()
            Safe -> return ()

        Helpers.emitNamedInstStripMeta ["tbaa"] i

        when enable $ do
          ty <- typeOf src
          case ty of
            (Left s) -> do
              pFunc <- gets (show . name . fromJust . currentFunction)
              pInst <- pure $ show i
              error $ "instrumentInst: in function " ++ pFunc ++ ": could not compute type of stored value in " ++ pInst ++ " (" ++ s ++ ")"
            (Right ty') -> do
              when (Helpers.isPointerType ty' && (not $ Helpers.isFunctionType $ pointerReferent ty')) $ do -- TODO-IMPROVE: We don't currently instrument function pointers
                meta <- inspect src
                if isNothing meta
                then do
                  pAddr <- pure $ show src
                  pFunc <- gets (show . name . fromJust . currentFunction)
                  pInst <- pure $ show i
                  tell ["in function " ++ pFunc ++ ": not instrumenting store of uninstrumented pointer " ++ pAddr ++ " in " ++ pInst]
                  return ()
                else do
                  let meta' = snd $ fromJust meta
                  tgt' <- bitcast tgt (ptr i8)
                  case meta' of
                    Left variableMeta -> do
                      baseReg <- reload baseRegisterMetadata (vBase variableMeta) src
                      boundReg <- reload boundRegisterMetadata (vBound variableMeta) src
                      keyReg <- reload keyRegisterMetadata (vKey variableMeta) src
                      lockReg <- reload lockRegisterMetadata (vLock variableMeta) src
                      -- Write the metadata for the stored pointer to the runtime
                      _ <- emitRuntimeAPIFunctionCall "__softboundcets_metadata_store" [tgt', baseReg, boundReg, keyReg, lockReg]
                      gen src (baseReg, boundReg, keyReg, lockReg) -- We just generated these registers
                      return ()
                    Right constMeta@(Constant {}) -> do
                      baseReg <- pure $ ConstantOperand $ cBase constMeta
                      boundReg <- pure $ ConstantOperand $ cBound constMeta
                      keyReg <- pure $ ConstantOperand $ cKey constMeta
                      lockReg <- pure $ ConstantOperand $ cLock constMeta
                      -- Write the metadata for the stored pointer to the runtime
                      _ <- emitRuntimeAPIFunctionCall "__softboundcets_metadata_store" [tgt', baseReg, boundReg, keyReg, lockReg]
                      return ()
                    Right constMeta@(Global {}) -> do
                      baseReg <- pure $ ConstantOperand $ gBase constMeta
                      boundReg <- pure $ ConstantOperand $ gBound constMeta
                      keyReg <- pure $ ConstantOperand $ gKey constMeta
                      glp <- gets (fromJust . globalLockPtr)
                      lockReg <- load glp 0
                      -- Write the metadata for the stored pointer to the runtime
                      _ <- emitRuntimeAPIFunctionCall "__softboundcets_metadata_store" [tgt', baseReg, boundReg, keyReg, lockReg]
                      return ()

      | otherwise = Helpers.emitNamedInstStripMeta ["tbaa"] i

    instrumentTerm i
      | (Do (Ret (Just op) _)) <- i = do
          ty <- typeOf op
          case ty of
            (Left s) -> do
              pFunc <- gets (show . name . fromJust . currentFunction)
              pInst <- pure $ show i
              error $ "instrumentInst: in function " ++ pFunc ++ ": could not compute type of argument to return instruction in " ++ pInst ++ " (" ++ s ++ ")"
            (Right ty') -> do
              when (Helpers.isPointerType ty' && (not $ Helpers.isFunctionType $ pointerReferent ty')) $ do
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
