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

data LocalMetadata = Local    { base :: Operand
                              , bound :: Operand
                              , key :: Operand
                              , lock :: Operand
                              }

data GlobalMetadata = Global  { gbase :: Const.Constant
                              , gbound :: Const.Constant
                              , gkey :: Const.Constant
                              } -- Globals use the runtime-initialized global lock

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
                               , currentFunctionGlobals :: Data.Set.Set Operand
                               -- ^ The set of global references in the current function. These need local metadata when they are used, which is populated from their constant metadata.
                               , classifications :: Map Operand PointerClass
                               -- ^ The map of pointers to their class.
                               --   'classifications' needs to be saved and restored around function entry and exit so we don't mistakenly treat
                               --   pointers with the same names in different functions as belonging to the same class. Outside a function, 'classifications' contains global variables.
                               , blacklist :: Data.Set.Set Name
                               -- ^ The set of blacklisted function symbols (these will not be instrumented).
                               , dontCareMetadata :: Maybe LocalMetadata
                               -- ^ LocalMetadata that can never cause any runtime checks to fail.
                               , nullMetadata :: Maybe LocalMetadata
                               -- ^ LocalMetadata for null pointers.
                               , dontCareMetadataRegs :: Maybe LocalMetadata
                               -- ^ LocalMetadata that can never cause any runtime checks to fail (in registers).
                               , localMetadata :: Map Operand LocalMetadata
                               -- ^ A 'Map' from pointer 'Operand's in the current function to their metadata.
                               , registerMetadata :: Map Operand LocalMetadata
                               -- ^ A 'Map' from pointer 'Operand's in the current function to their metadata in registers.
                               , globalMetadata :: Map Operand GlobalMetadata
                               -- ^ A 'Map' from pointer 'Operand's in the global scope to their metadata.
                               }

-- | Create an empty 'SBCETSState'
emptySBCETSState :: SBCETSState
emptySBCETSState = SBCETSState Nothing Nothing Nothing
                               Data.Set.empty
                               Data.Map.empty Data.Map.empty
                               CLI.defaultOptions Nothing Data.Set.empty
                               Data.Map.empty Data.Set.empty
                               Nothing Nothing Nothing
                               Data.Map.empty Data.Map.empty Data.Map.empty

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
associate :: MonadState SBCETSState m => Operand -> LocalMetadata -> m ()
associate p m = modify $ \s -> s { localMetadata = Data.Map.insert p m $ localMetadata s }

-- | Mark the registers holding the metadata values for the given pointer as live
gen :: MonadState SBCETSState m => Operand -> LocalMetadata -> m ()
gen p m = do
  useRegMeta <- gets (CLI.reuseRegisters . options)
  if useRegMeta
  then modify $ \s -> s { registerMetadata = Data.Map.insert p m $ registerMetadata s }
  else return ()

-- | Mark the registers holding the metadata values for the given pointer as dead
kill :: MonadState SBCETSState m => Operand -> m ()
kill p =  do
  useRegMeta <- gets (CLI.reuseRegisters . options)
  if useRegMeta
  then modify $ \s -> s { registerMetadata = Data.Map.delete p $ registerMetadata s }
  else return ()

-- | Check if live register metadata exists for the given pointer
live :: MonadState SBCETSState m => Operand -> m Bool
live p =  do
  useRegMeta <- gets (CLI.reuseRegisters . options)
  if useRegMeta
  then gets (Data.Map.member p . registerMetadata)
  else return False

-- | 'inspect' traverses pointer-type expressions and returns the metadata.
inspect :: (HasCallStack, MonadState SBCETSState m, MonadWriter [String] m, MonadModuleBuilder m) => Operand -> m (Maybe (Type, LocalMetadata))
inspect p
  | (LocalReference (PointerType ty _) _) <- p = do
      if Helpers.isFunctionType ty
      then return Nothing -- TODO-IMPROVE: Function pointers currently get don't-care metadata but they are supposed to be checked for spatial safety at callsites.
      else do
        present <- gets (Data.Map.member p . localMetadata)
        if present
        then gets (Just . (ty,) . (! p) . localMetadata)
        else do
          pp <- pure $ show p
          fname <- gets (name . fromJust . currentFunction)
          tell ["inspect: in function " ++ (show fname) ++ ": no local storage allocated for metadata for pointer " ++ pp]
          return Nothing

  | (ConstantOperand (Const.Null (PointerType ty _))) <- p = gets (Just . (ty,) . fromJust . nullMetadata)
  | (ConstantOperand (Const.Undef (PointerType ty _))) <- p = gets (Just . (ty,) . fromJust . nullMetadata)
  | (ConstantOperand (Const.GlobalReference (PointerType ty _) _)) <- p = do
      present <- gets (Data.Map.member p . localMetadata)
      if present
      then gets (Just . (ty,) . (! p) . localMetadata)
      else do
        pp <- pure $ show p
        fname <- gets (name . fromJust . currentFunction)
        tell ["inspect: in function " ++ (show fname) ++ ": no local storage allocated for metadata for global variable " ++ pp]
        return Nothing

  | (ConstantOperand (Const.GetElementPtr _ addr _)) <- p = inspect (ConstantOperand addr)
  | (ConstantOperand (Const.IntToPtr _ (PointerType ty _))) <- p = gets (Just . (ty,) . fromJust . dontCareMetadata)
  | (ConstantOperand (Const.BitCast x (PointerType ty _))) <- p = do
      meta <- inspect (ConstantOperand x)
      case meta of
        (Just (_, m)) -> return $ Just (ty, m)
        _ -> return Nothing

  -- TODO-IMPROVE: Some constant expressions of pointer type currently just get the don't-care metadata.
  -- This is sufficient for performance testing (since it doesn't alter the amount of work done) but not for real-world use.
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
allocateLocalMetadataStorage :: (HasCallStack, MonadState SBCETSState m, MonadIRBuilder m, MonadModuleBuilder m) => Operand -> m LocalMetadata
allocateLocalMetadataStorage p = do
  let p' = Helpers.walk p
  allocated <- gets (Data.Map.member p' . localMetadata)
  if not allocated
  then do
    basePtr <- (alloca (ptr i8) Nothing 8) `named` (fromString "sbcets.base")
    boundPtr <- (alloca (ptr i8) Nothing 8) `named` (fromString "sbcets.bound")
    keyPtr <- (alloca (i64) Nothing 8) `named` (fromString "sbcets.key")
    lockPtr <- (alloca (ptr i8) Nothing 8) `named` (fromString "sbcets.lock")
    let meta = Local basePtr boundPtr keyPtr lockPtr
    associate p' meta
    return meta
  else do
    fname <- gets (show . name . fromJust . currentFunction)
    pp <- pure $ show p'
    error $ "allocateLocalMetadataStorage: in function " ++ fname ++ ": storage already allocated for metadata for pointer " ++ pp

-- | Look up the local variables allocated to hold metadata for the given pointer
getLocalMetadataStorage :: (HasCallStack, MonadState SBCETSState m, MonadIRBuilder m, MonadModuleBuilder m) => Operand -> m LocalMetadata
getLocalMetadataStorage p = do
  let p' = Helpers.walk p
  allocated <- gets ((Data.Map.lookup p') . localMetadata)
  if isJust allocated
  then gets ((! p') . localMetadata)
  else do
    pp <- pure $ show p'
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
      -- Case 1: If a pointer is loaded from memory, local metadata is allocated for the pointer's metadata. Local metadata also needs allocating for the address if it is a global variable.
      | (v := o) <- inst, (Load _ addr _ _ _) <- o = do
          enable <- gets (CLI.instrumentLoad . options)
          if enable
          then do
            ta <- typeOf addr
            case ta of
              (Left s) -> error $ "identifyLocalMetadataAllocations: failed to compute type of argument to instruction " ++ show o ++ " (" ++ s ++ ")"
              (Right ta') -> do
                if (Helpers.isPointerType $ pointerReferent ta') &&
                  (not $ Helpers.isFunctionType $ pointerReferent $ pointerReferent ta') -- TODO-IMPROVE: We don't currently instrument function pointers
                then do
                  if Helpers.isConstantOperand addr
                  then do
                    let addr' = Helpers.walk addr
                    modify $ \s -> s { currentFunctionGlobals = Data.Set.insert addr' $ currentFunctionGlobals s }
                    return [addr', LocalReference (pointerReferent ta') v]
                  else return [LocalReference (pointerReferent ta') v]
                else return []
          else return []
      -- Case 2: If a function is called and pointer arguments are passed, the metadata for those pointer arguments must be available in local variables.
      -- Additionally, if a pointer is returned, local variables must be allocated to hold the metadata for that pointer.
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
              let globals = map Helpers.walk $ filter Helpers.isConstantOperand ptrArgs
              modify $ \s -> s { currentFunctionGlobals = Data.Set.union (Data.Set.fromList globals) $ currentFunctionGlobals s }
              return (ptrArgs ++ ptrRet)
            else do
              let s = head $ lefts argTys
              error $ "identifyLocalMetadataAllocations: failed to compute type of function parameter (" ++ s ++ ")"
          else return []
      -- Case 3a: Local metadata must be available for all incoming values to a phi instruction of pointer type.
      | (_ := o) <- inst, (Phi (PointerType ty _) incoming _) <- o = do
          if (not $ Helpers.isFunctionType ty) -- TODO-IMPROVE: We don't currently instrument function pointers
          then do
            let incomingV = map fst incoming
            let incomingG = map Helpers.walk $ filter Helpers.isConstantOperand incomingV
            modify $ \s -> s { currentFunctionGlobals = Data.Set.union (Data.Set.fromList incomingG) $ currentFunctionGlobals s }
            return incomingV
          else return []
      -- Case 3b: Local metadata must be available for all incoming values to a select instruction of pointer type.
      | (_ := o) <- inst, (Select _ tv fv _) <- o = do
          selTy <- typeOf tv
          case selTy of
            (Left s) -> do
              error $ "identifyLocalMetadataAllocations: failed to compute type of select instruction argument (" ++ s ++ ")"
            (Right selTy') -> do
              if (Helpers.isPointerType selTy' && (not $ Helpers.isFunctionType $ pointerReferent selTy')) -- TODO-IMPROVE: We don't currently instrument function pointers
              then do
                let incomingV = [tv, fv]
                let incomingG = map Helpers.walk $ filter Helpers.isConstantOperand incomingV
                modify $ \s -> s { currentFunctionGlobals = Data.Set.union (Data.Set.fromList incomingG) $ currentFunctionGlobals s }
                return incomingV
              else return []
      -- Case 4: If we allocate anything on the stack, we get a pointer to it, which needs metadata.
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
              let globals = map Helpers.walk $ filter Helpers.isConstantOperand ptrArgs
              modify $ \s -> s { currentFunctionGlobals = Data.Set.union (Data.Set.fromList globals) $ currentFunctionGlobals s }
              return ptrArgs
            else do
              let s = head $ lefts argTys
              error $ "identifyLocalMetadataAllocations: failed to compute type of function parameter (" ++ s ++ ")"
          else return []
      | otherwise = return []

    termAllocations term
      -- Case 6: If we return a pointer, we need to push the pointer's metadata to the shadow stack, so it must be available in local variables.
      | (Do (Ret (Just x) _)) <- term = do
          tx <- typeOf x
          case tx of
            (Left s) -> error $ "identifyLocalMetadataAllocations: failed to compute type of return argument (" ++ s ++ ")"
            (Right tx') -> do
              if (Helpers.isPointerType tx') &&
                 (not $ Helpers.isFunctionType $ pointerReferent tx') -- TODO-IMPROVE: We don't currently instrument function pointers
              then do
                if Helpers.isConstantOperand x
                then do
                  let x' = Helpers.walk x
                  modify $ \s -> s { currentFunctionGlobals = Data.Set.insert x' $ currentFunctionGlobals s }
                  return [x']
                else return [x]
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

-- | Load the metadata for the pointer stored at the given memory address (first argument) from the runtime. Associate the loaded metadata with the given pointer (second argument)
emitRuntimeMetadataLoad :: (HasCallStack, MonadState SBCETSState m, MonadWriter [String] m, MonadIRBuilder m, MonadModuleBuilder m) => Operand -> Operand -> m LocalMetadata
emitRuntimeMetadataLoad addr loadedPtr
  | (LocalReference (PointerType _ _) _) <- loadedPtr = do
      allocated <- gets (Data.Map.member loadedPtr . localMetadata)
      if not allocated
      then do
        pLoadedPtr <- pure $ show loadedPtr
        tell ["emitRuntimeMetadataLoad: using don't-care metadata for unsupported pointer " ++ pLoadedPtr]
        gets (fromJust . dontCareMetadata)
      else do
        case addr of
          (LocalReference {}) -> do -- Loading a pointer from memory
            addr' <- bitcast addr (ptr i8)
            meta <- getLocalMetadataStorage loadedPtr
            _ <- emitRuntimeAPIFunctionCall "__softboundcets_metadata_load" [addr', (base meta), (bound meta), (key meta), (lock meta)]
            emitCheck <- gets (CLI.emitChecks . options)
            when emitCheck $ do
              _ <- emitRuntimeAPIFunctionCall "__softboundcets_metadata_check" [(base meta), (bound meta), (key meta), (lock meta)]
              return ()
            return meta

          (ConstantOperand {}) -> do -- Loading a global variable pointer or explicit constant address
            meta <- inspect addr
            meta' <- if isJust meta
                     then return $ snd $ fromJust meta
                     else do
                       pAddr <- pure $ show addr
                       tell ["emitRuntimeMetadataLoad: using don't-care metadata for unsupported pointer " ++ pAddr]
                       dcMeta <- gets (fromJust . dontCareMetadata)
                       return dcMeta
            base' <- load (base meta') 0
            bound' <- load (bound meta') 0
            key' <- load (key meta') 0
            lock' <- load (lock meta') 0
            emitMetadataStoreToLocalVariables loadedPtr (base', bound', key', lock')

          (MetadataOperand {}) -> do
            pAddr <- pure $ show addr
            tell ["emitRuntimeMetadataLoad: using don't-care metadata for unsupported pointer " ++ pAddr]
            gets (fromJust . dontCareMetadata)

  | otherwise = do
      pLoadedPtr <- pure $ show loadedPtr
      error $ "emitRuntimeMetadataLoad: expected local variable pointer but saw " ++ pLoadedPtr

-- | Create local metadata variables in a function to hold the metadata for the given global variable
populateLocalMetadataForGlobal :: (HasCallStack, MonadState SBCETSState m, MonadWriter [String] m, MonadIRBuilder m, MonadModuleBuilder m) => Operand -> m ()
populateLocalMetadataForGlobal g = do
  let g' = Helpers.walk g
  localMeta <- getLocalMetadataStorage g'
  present <- gets (Data.Map.member g' . globalMetadata)
  if present
  then do
    globalMeta <- gets ((! g') . globalMetadata)
    glp <- gets (fromJust . globalLockPtr)
    gl <- load glp 0
    let base' = ConstantOperand $ gbase globalMeta
    let bound' = ConstantOperand $ gbound globalMeta
    let key' = ConstantOperand $ gkey globalMeta
    let lock' = gl
    store (base localMeta) 8 base'
    store (bound localMeta) 8 bound'
    store (key localMeta) 8 key'
    store (lock localMeta) 8 lock'
    gen g' (Local base' bound' key' lock') -- We just generated these registers
    return ()
  else do
    tell ["populateLocalMetadataForGlobal: using don't-care metadata for unsupported global variable " ++ show g']
    dcMeta <- gets (fromJust . dontCareMetadata)
    base' <- load (base dcMeta) 0
    bound' <- load (bound dcMeta) 0
    key' <- load (key dcMeta) 0
    lock' <- load (lock dcMeta) 0
    store (base localMeta) 8 base'
    store (bound localMeta) 8 bound'
    store (key localMeta) 8 key'
    store (lock localMeta) 8 lock'
    gen g' (Local base' bound' key' lock') -- We just generated these registers
    return ()

-- | Create a local key and lock for entities allocated in the current stack frame
emitLocalKeyAndLockCreation :: (HasCallStack, MonadState SBCETSState m, MonadIRBuilder m, MonadModuleBuilder m) => m ()
emitLocalKeyAndLockCreation = do
  keyPtr <- (alloca i64 Nothing 8) `named` (fromString "sbcets.stack_frame_key")
  lockPtr <- (alloca (ptr i8) Nothing 8) `named` (fromString "sbcets.stack_frame_lock")
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
emitMetadataStoreToLocalVariables :: (HasCallStack, MonadState SBCETSState m, MonadIRBuilder m, MonadModuleBuilder m) => Operand -> (Operand, Operand, Operand, Operand) -> m LocalMetadata
emitMetadataStoreToLocalVariables p (baseReg, boundReg, keyReg, lockReg) = do
  meta <- getLocalMetadataStorage p
  store (base meta) 8 baseReg
  store (bound meta) 8 boundReg
  store (key meta) 8 keyReg
  store (lock meta) 8 lockReg
  gen p (Local baseReg boundReg keyReg lockReg) -- We just generated these registers
  return meta

-- | Load the metadata for a pointer function parameter from the shadow stack.
emitMetadataLoadFromShadowStack :: (HasCallStack, MonadState SBCETSState m, MonadIRBuilder m, MonadModuleBuilder m) => Operand -> Integer -> m LocalMetadata
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
    if isJust callee
    then tell ["emitMetadataStoreToShadowStack: in function " ++ fname ++ ": using don't-care metadata for unsupported pointer " ++ pp ++ " passed to function " ++ (show $ fromJust callee)]
    else tell ["emitMetadataStoreToShadowStack: in function " ++ fname ++ ": using don't-care metadata for unsupported pointer " ++ pp ++ " being returned"]
    meta' <- gets (fromJust . dontCareMetadata)
    ix' <- pure $ int32 ix
    useRegMeta <- gets (CLI.reuseRegisters . options)
    if useRegMeta
    then do
      (Local baseReg boundReg keyReg lockReg) <- gets (fromJust . dontCareMetadataRegs)
      _ <- emitRuntimeAPIFunctionCall "__softboundcets_store_base_shadow_stack" [baseReg, ix']
      _ <- emitRuntimeAPIFunctionCall "__softboundcets_store_bound_shadow_stack" [boundReg, ix']
      _ <- emitRuntimeAPIFunctionCall "__softboundcets_store_key_shadow_stack" [keyReg, ix']
      _ <- emitRuntimeAPIFunctionCall "__softboundcets_store_lock_shadow_stack" [lockReg, ix']
      return ()
    else do
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
    let meta' = snd $ fromJust meta
    ix' <- pure $ int32 ix
    haveRegMeta <- live p
    if haveRegMeta
    then do
      (Local baseReg boundReg keyReg lockReg) <- gets ((! p) . registerMetadata)
      _ <- emitRuntimeAPIFunctionCall "__softboundcets_store_base_shadow_stack" [baseReg, ix']
      _ <- emitRuntimeAPIFunctionCall "__softboundcets_store_bound_shadow_stack" [boundReg, ix']
      _ <- emitRuntimeAPIFunctionCall "__softboundcets_store_key_shadow_stack" [keyReg, ix']
      _ <- emitRuntimeAPIFunctionCall "__softboundcets_store_lock_shadow_stack" [lockReg, ix']
      return ()
    else do
      baseReg <- load (base meta') 0
      boundReg <- load (bound meta') 0
      keyReg <- load (key meta') 0
      lockReg <- load (lock meta') 0
      gen p (Local baseReg boundReg keyReg lockReg) -- We just generated these registers
      _ <- emitRuntimeAPIFunctionCall "__softboundcets_store_base_shadow_stack" [baseReg, ix']
      _ <- emitRuntimeAPIFunctionCall "__softboundcets_store_bound_shadow_stack" [boundReg, ix']
      _ <- emitRuntimeAPIFunctionCall "__softboundcets_store_key_shadow_stack" [keyReg, ix']
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

    instrumentDefinition :: (HasCallStack, MonadIRBuilder m, MonadState SBCETSState m, MonadWriter [String] m, MonadModuleBuilder m) => Definition -> m ()
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

    instrumentGlobalVariable :: (HasCallStack, MonadIRBuilder m, MonadState SBCETSState m, MonadWriter [String] m, MonadModuleBuilder m) => Global -> m ()
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

    instrumentFunction :: (HasCallStack, MonadIRBuilder m, MonadState SBCETSState m, MonadWriter [String] m, MonadModuleBuilder m) => Global -> m ()
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
                             , dontCareMetadata = Nothing
                             , nullMetadata = Nothing
                             , dontCareMetadataRegs = Nothing
                             , localMetadata = Data.Map.empty
                             , registerMetadata = Data.Map.empty
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
              mapM_ allocateLocalMetadataStorage pointerArguments
              _ <- zipWithM emitMetadataLoadFromShadowStack pointerArguments shadowStackIndices
              -- Create the don't-care metadata. The runtime returns this metadata for address 0.
              -- The don't-care metadata is platform-dependent, which is why the runtime does this.
              nullPtr <- inttoptr (int64 0) (ptr i8)
              dcMetadata <- allocateLocalMetadataStorage nullPtr
              _ <- emitRuntimeMetadataLoad nullPtr nullPtr
              modify $ \s -> s { dontCareMetadata = Just dcMetadata }
              -- Load the don't-care metadata into registers for reuse if reuseRegisters is set
              useRegMeta <- gets (CLI.reuseRegisters . options)
              when useRegMeta $ do
                dcMetaBase <- load (base dcMetadata) 0
                dcMetaBound <- load (bound dcMetadata) 0
                dcMetaKey <- load (key dcMetadata) 0
                dcMetaLock <- load (lock dcMetadata) 0
                modify $ \s -> s { dontCareMetadataRegs = Just $ Local dcMetaBase dcMetaBound dcMetaKey dcMetaLock }
              -- Create the null pointer metadata. The null pointer metadata is not platform dependent.
              nullMetaBasePtr <- (alloca (ptr i8) Nothing 8) `named` (fromString "sbcets.null.base")
              nullMetaBoundPtr <- (alloca (ptr i8) Nothing 8) `named` (fromString "sbcets.null.bound")
              nullMetaKeyPtr <- (alloca (i64) Nothing 8) `named` (fromString "sbcets.null.key")
              nullMetaLockPtr <- (alloca (ptr i8) Nothing 8) `named` (fromString "sbcets.null.lock")
              modify $ \s -> s { nullMetadata = Just $ Local nullMetaBasePtr nullMetaBoundPtr nullMetaKeyPtr nullMetaLockPtr }
              -- Initialize the null pointer metadata
              nullMeta <- gets (fromJust . nullMetadata)
              let nullMetaBase = ConstantOperand $ Const.IntToPtr (Const.Int 64 0) (ptr i8)
              store (base nullMeta) 8 nullMetaBase
              let nullMetaBound = ConstantOperand $ Const.IntToPtr (Const.Int 64 0) (ptr i8)
              store (bound nullMeta) 8 nullMetaBound
              let nullMetaKey = ConstantOperand $ Const.Int 64 0
              store (key nullMeta) 8 nullMetaKey
              let nullMetaLock = ConstantOperand $ Const.IntToPtr (Const.Int 64 0) (ptr i8)
              store (lock nullMeta) 8 nullMetaLock
              -- Collect all metadata allocation sites so we can allocate local variables for metadata ahead of time
              pointersRequiringLocalMetadata <- liftM (Data.Set.fromList . map Helpers.walk . concat) $ mapM identifyLocalMetadataAllocations $ basicBlocks f
              mapM_ allocateLocalMetadataStorage $ filter (not . flip elem pointerArguments) $ Data.Set.toList pointersRequiringLocalMetadata
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
      -- Populate the local metadata for global variables used in the function
      globals <- gets currentFunctionGlobals
      mapM_ populateLocalMetadataForGlobal globals
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
                tell ["in function " ++ pFunc ++ ": using don't-care metadata for unsupported pointer " ++ pAddr ++ " in " ++ pInst]
                ta <- typeOf addr
                case ta of
                  (Left s) -> do
                    error $ "instrumentInst: in function " ++ pFunc ++ ": could not compute type of argument to load instruction in " ++ pInst ++ " (" ++ s ++ ")"
                  (Right ta') -> do
                    let refTy = pointerReferent ta'
                    addr' <- bitcast addr (ptr i8)
                    tySize <- sizeof 64 refTy
                    dcMeta <- gets (fromJust . dontCareMetadata)
                    useRegMeta <- gets (CLI.reuseRegisters . options)
                    if useRegMeta
                    then do
                      (Local baseReg boundReg keyReg lockReg) <- gets (fromJust . dontCareMetadataRegs)
                      -- Check the load is spatially in bounds
                      _ <- emitRuntimeAPIFunctionCall "__softboundcets_spatial_load_dereference_check" [baseReg, boundReg, addr', tySize]
                      -- Check the load is temporally in bounds
                      _ <- emitRuntimeAPIFunctionCall "__softboundcets_temporal_load_dereference_check" [lockReg, keyReg]
                      return ()
                    else do
                      baseReg <- load (base dcMeta) 0
                      boundReg <- load (bound dcMeta) 0
                      -- Check the load is spatially in bounds
                      _ <- emitRuntimeAPIFunctionCall "__softboundcets_spatial_load_dereference_check" [baseReg, boundReg, addr', tySize]
                      -- Check the load is temporally in bounds
                      keyReg <- load (key dcMeta) 0
                      lockReg <- load (lock dcMeta) 0
                      _ <- emitRuntimeAPIFunctionCall "__softboundcets_temporal_load_dereference_check" [lockReg, keyReg]
                      return ()
              else do
                let (refTy, meta') = fromJust meta
                addr' <- bitcast addr (ptr i8)
                tySize <- sizeof 64 refTy
                haveRegMeta <- live addr
                if haveRegMeta
                then do
                  (Local baseReg boundReg keyReg lockReg) <- gets ((! addr) . registerMetadata)
                  -- Check the load is spatially in bounds
                  _ <- emitRuntimeAPIFunctionCall "__softboundcets_spatial_load_dereference_check" [baseReg, boundReg, addr', tySize]
                  -- Check the load is temporally in bounds
                  _ <- emitRuntimeAPIFunctionCall "__softboundcets_temporal_load_dereference_check" [lockReg, keyReg]
                  return ()
                else do
                  baseReg <- load (base meta') 0
                  boundReg <- load (bound meta') 0
                  -- Check the load is spatially in bounds
                  _ <- emitRuntimeAPIFunctionCall "__softboundcets_spatial_load_dereference_check" [baseReg, boundReg, addr', tySize]
                  -- Check the load is temporally in bounds
                  keyReg <- load (key meta') 0
                  lockReg <- load (lock meta') 0
                  _ <- emitRuntimeAPIFunctionCall "__softboundcets_temporal_load_dereference_check" [lockReg, keyReg]
                  gen addr (Local baseReg boundReg keyReg lockReg) -- We just generated these registers
                  return ()
            Safe -> return ()

        Helpers.emitNamedInst i

        when enable $ do -- No matter if we were able to instrument the load or not, if a pointer was loaded, ask the runtime for metadata for the loaded address.
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
                _ <- emitRuntimeMetadataLoad addr resultPtr
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
                  Helpers.emitNamedInst $ v := (Helpers.rewriteCalledFunctionName wrapperFunctionName o)
                else Helpers.emitNamedInst i
                -- TODO-OPTIMIZE: The function could potentially deallocate any pointer it is passed
                mapM_ (flip mark $ Unsafe) $ Data.Set.fromList ptrArgs
                mapM_ kill $ Data.Set.fromList ptrArgs
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
              Helpers.emitNamedInst i

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
                  tell ["in function " ++ pFunc ++ ": using don't-care metadata for unsupported pointer " ++ pAddr ++ " in " ++ pInst]
                  dcMeta <- gets (fromJust . dontCareMetadata)
                  associate gepResultPtr dcMeta -- The pointer created by getelementptr shares metadata storage with the parent pointer
                  mark gepResultPtr Unsafe -- TODO-OPTIMIZE: arithmetic derived pointers are considered unconditionally unsafe (even if the parent pointer is safe)
                else do
                  let meta' = snd $ fromJust meta
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
            let meta' = snd $ fromJust meta
            associate bitcastResultPtr meta' -- The pointer created by bitcast shares metadata storage with the parent pointer
            mark bitcastResultPtr Unsafe -- TODO-OPTIMIZE: cast pointers are considered unconditionally unsafe (even if the parent pointer is safe)
        Helpers.emitNamedInst i

      | (Select cond tval fval _) <- o = do
        Helpers.emitNamedInst i
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

              base' <- select cond (base tMeta') (base fMeta')
              bound' <- select cond (bound tMeta') (bound fMeta')
              key' <- select cond (key tMeta') (key fMeta')
              lock' <- select cond (lock tMeta') (lock fMeta')
              associate resultPtr $ Local base' bound' key' lock'
              kill resultPtr
              return ()

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
                                  let meta' = snd $ fromJust meta
                                  return (meta', n)

        when (not $ Helpers.isFunctionType ty) $ do -- TODO-IMPROVE: We don't currently instrument function pointers
          incomingMeta <- forM incoming phiMeta
          base'  <- phi $ map (\(x, n) -> (base x, n)) incomingMeta
          bound' <- phi $ map (\(x, n) -> (bound x, n)) incomingMeta
          key'   <- phi $ map (\(x, n) -> (key x, n)) incomingMeta
          lock'  <- phi $ map (\(x, n) -> (lock x, n)) incomingMeta
          let resultPtr = LocalReference (ptr ty) v
          associate resultPtr $ Local base' bound' key' lock'
          kill resultPtr
          return ()

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
                  Helpers.emitNamedInst $ Do $ Helpers.rewriteCalledFunctionName wrapperFunctionName o
                else Helpers.emitNamedInst i
                -- The function could potentially deallocate any pointer it is passed
                mapM_ (flip mark $ Unsafe) $ Data.Set.fromList ptrArgs
                mapM_ kill $ Data.Set.fromList ptrArgs
                emitShadowStackDeallocation
              else do
                let s = head $ lefts opdTys
                pFunc <- gets (show . name . fromJust . currentFunction)
                pInst <- pure $ show i
                error $ "instrumentInst: in function " ++ pFunc ++ ": could not compute type of function argument in " ++ pInst ++ " (" ++ s ++ ")"
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
                ta <- typeOf tgt
                case ta of
                  (Left s) -> error $ "instrumentInst: could not compute type of argument to store instruction (" ++ s ++ ")"
                  (Right ta') -> do
                    let refTy = pointerReferent ta'
                    tgt' <- bitcast tgt (ptr i8)
                    tySize <- sizeof 64 refTy
                    dcMeta <- gets (fromJust . dontCareMetadata)
                    useRegMeta <- gets (CLI.reuseRegisters . options)
                    if useRegMeta
                    then do
                      (Local baseReg boundReg keyReg lockReg) <- gets (fromJust . dontCareMetadataRegs)
                      -- Check the store is spatially in bounds
                      _ <- emitRuntimeAPIFunctionCall "__softboundcets_spatial_store_dereference_check" [baseReg, boundReg, tgt', tySize]
                      -- Check the store is temporally in bounds
                      _ <- emitRuntimeAPIFunctionCall "__softboundcets_temporal_store_dereference_check" [lockReg, keyReg]
                      return ()
                    else do
                      baseReg <- load (base dcMeta) 0
                      boundReg <- load (bound dcMeta) 0
                      -- Check the store is spatially in bounds
                      _ <- emitRuntimeAPIFunctionCall "__softboundcets_spatial_store_dereference_check" [baseReg, boundReg, tgt', tySize]
                      -- Check the store is temporally in bounds
                      keyReg <- load (key dcMeta) 0
                      lockReg <- load (lock dcMeta) 0
                      _ <- emitRuntimeAPIFunctionCall "__softboundcets_temporal_store_dereference_check" [lockReg, keyReg]
                      return ()
              else do
                let (refTy, meta') = fromJust meta
                tgt' <- bitcast tgt (ptr i8)
                tySize <- sizeof 64 refTy
                haveRegMeta <- live tgt
                if haveRegMeta
                then do
                  (Local baseReg boundReg keyReg lockReg) <- gets ((! tgt) . registerMetadata)
                  -- Check the store is spatially in bounds
                  _ <- emitRuntimeAPIFunctionCall "__softboundcets_spatial_store_dereference_check" [baseReg, boundReg, tgt', tySize]
                  -- Check the store is temporally in bounds
                  _ <- emitRuntimeAPIFunctionCall "__softboundcets_temporal_store_dereference_check" [lockReg, keyReg]
                  return ()
                else do
                  baseReg <- load (base meta') 0
                  boundReg <- load (bound meta') 0
                  -- Check the store is spatially in bounds
                  _ <- emitRuntimeAPIFunctionCall "__softboundcets_spatial_store_dereference_check" [baseReg, boundReg, tgt', tySize]
                  -- Check the store is temporally in bounds
                  keyReg <- load (key meta') 0
                  lockReg <- load (lock meta') 0
                  _ <- emitRuntimeAPIFunctionCall "__softboundcets_temporal_store_dereference_check" [lockReg, keyReg]
                  gen tgt (Local baseReg boundReg keyReg lockReg) -- We just generated these registers
                  return ()
            Safe -> return ()

        Helpers.emitNamedInst i

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
                  tell ["in function " ++ pFunc ++ ": using don't-care metadata for uninstrumented pointer " ++ pAddr ++ " in " ++ pInst]
                  dcMeta <- gets (fromJust . dontCareMetadata)
                  tgt' <- bitcast tgt (ptr i8)
                  useRegMeta <- gets (CLI.reuseRegisters . options)
                  if useRegMeta
                  then do
                    (Local baseReg boundReg keyReg lockReg) <- gets (fromJust . dontCareMetadataRegs)
                    -- Write the metadata for the stored pointer to the runtime
                    _ <- emitRuntimeAPIFunctionCall "__softboundcets_metadata_store" [tgt', baseReg, boundReg, keyReg, lockReg]
                    return ()
                  else do
                    baseReg <- load (base dcMeta) 0
                    boundReg <- load (bound dcMeta) 0
                    keyReg <- load (key dcMeta) 0
                    lockReg <- load (lock dcMeta) 0
                    -- Write the metadata for the stored pointer to the runtime
                    _ <- emitRuntimeAPIFunctionCall "__softboundcets_metadata_store" [tgt', baseReg, boundReg, keyReg, lockReg]
                    return ()
                else do
                  let meta' = snd $ fromJust meta
                  tgt' <- bitcast tgt (ptr i8)
                  haveRegMeta <- live src
                  if haveRegMeta
                  then do
                    (Local baseReg boundReg keyReg lockReg) <- gets ((! src) . registerMetadata)
                    -- Write the metadata for the stored pointer to the runtime
                    _ <- emitRuntimeAPIFunctionCall "__softboundcets_metadata_store" [tgt', baseReg, boundReg, keyReg, lockReg]
                    return ()
                  else do
                    baseReg <- load (base meta') 0
                    boundReg <- load (bound meta') 0
                    keyReg <- load (key meta') 0
                    lockReg <- load (lock meta') 0
                    -- Write the metadata for the stored pointer to the runtime
                    _ <- emitRuntimeAPIFunctionCall "__softboundcets_metadata_store" [tgt', baseReg, boundReg, keyReg, lockReg]
                    gen src (Local baseReg boundReg keyReg lockReg) -- We just generated these registers
                    return ()

      | otherwise = Helpers.emitNamedInst i

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
