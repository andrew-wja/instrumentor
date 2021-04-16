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
import LLVM.AST.Constant
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Internal.SnocList
import SoftboundCETSDefinitions
import Lib

data SBCETSState = SBCETSState { globalLockPtr :: Maybe Operand
                               , metadataTable :: Map Operand (Operand, Operand, Operand, Operand)
                               , runtimeFunctionPrototypes :: Map String Type
                               }

emptySBCETSState :: SBCETSState
emptySBCETSState = SBCETSState Nothing Data.Map.empty $ Data.Map.fromList [
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
  ("__softboundcets_deallocate_shadow_stack_space", FunctionType void [] False)
  ]

instrument :: Module -> IO Module
instrument m = do
  let sbcModule = buildModule (fromString "softboundcets") sbcetsModule
  return $ sbcModule { moduleName = moduleName m,
                       moduleSourceFileName = moduleSourceFileName m,
                       moduleDataLayout = moduleDataLayout m,
                       moduleTargetTriple = moduleTargetTriple m,
                       moduleDefinitions = moduleDefinitions sbcModule ++
                                           (doInstrumentation $ moduleDefinitions m) }
  where
    doInstrumentation :: [Definition] -> [Definition]
    doInstrumentation defs =
      let ifs = functionsToInstrument defs
      in map (instrumentFunction ifs) defs

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

    -- We ignore functions not in the set of functions to instrument, except for "main" which we handle as a special case.

    instrumentFunction ifs g@(GlobalDefinition f@(Function {})) =
      if (Data.Set.member (name f) ifs || (name f) == (mkName "main")) && (not $ null $ basicBlocks f) then
        let firstBlockLabel = bbName $ head $ basicBlocks f
            builderState = emptyIRBuilder { builderNameSuggestion = Just $ fromString "sbcets" }
            builder :: StateT SBCETSState IRBuilder () = do
              -- We do not currently instrument pointer parameters to varargs functions
              when (not $ snd $ parameters f) (instrumentPointerArgs firstBlockLabel $ fst $ parameters f)
              instrumentBlocks $ basicBlocks f
        in GlobalDefinition $ f { name = if (name f) == (mkName "main") then mkName "softboundcets_main" else name f
                                , basicBlocks = execIRBuilder builderState $
                                                flip evalStateT emptySBCETSState $
                                                builder }
      else g

    instrumentFunction _ x = x

    bbName (BasicBlock n _ _) = n

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
      base <- call (ConstantOperand $ GlobalReference (ptr baseProto) $ mkName baseName) [(ix', [])]
      (boundName, boundProto) <- gets((!! "__softboundcets_load_bound_shadow_stack") . runtimeFunctionPrototypes)
      bound <- call (ConstantOperand $ GlobalReference (ptr boundProto) $ mkName boundName) [(ix', [])]
      (keyName, keyProto) <- gets((!! "__softboundcets_load_key_shadow_stack") . runtimeFunctionPrototypes)
      key <- call (ConstantOperand $ GlobalReference (ptr keyProto) $ mkName keyName) [(ix', [])]
      (lockName, lockProto) <- gets((!! "__softboundcets_load_lock_shadow_stack") . runtimeFunctionPrototypes)
      lock <- call (ConstantOperand $ GlobalReference (ptr lockProto) $ mkName lockName) [(ix', [])]
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
      glp <- call (ConstantOperand $ GlobalReference (ptr fproto) $ mkName fname) []
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
          baseCast <- bitcast base (ptr i8)
          (baseName, baseProto) <- gets ((!! "__softboundcets_store_base_shadow_stack") .runtimeFunctionPrototypes)
          _ <- call (ConstantOperand $ GlobalReference (ptr baseProto) $ mkName baseName)
                    [(baseCast, []), (ix', [])]
          boundCast <- bitcast bound (ptr i8)
          (boundName, boundProto) <- gets ((!! "__softboundcets_store_bound_shadow_stack") .runtimeFunctionPrototypes)
          _ <- call (ConstantOperand $ GlobalReference (ptr boundProto) $ mkName boundName)
                    [(boundCast, []), (ix', [])]
          (keyName, keyProto) <- gets ((!! "__softboundcets_store_key_shadow_stack") .runtimeFunctionPrototypes)
          _ <- call (ConstantOperand $ GlobalReference (ptr keyProto) $ mkName keyName)
                    [(key, []), (ix', [])]
          lockCast <- bitcast lock (ptr i8)
          (lockName, lockProto) <- gets ((!! "__softboundcets_store_lock_shadow_stack") .runtimeFunctionPrototypes)
          _ <- call (ConstantOperand $ GlobalReference (ptr lockProto) $ mkName lockName)
                    [(lockCast, []), (ix', [])]
          return ()
        Nothing -> return ()

    emitPointerOperandMetadataStore (ConstantOperand _) _ = undefined
    emitPointerOperandMetadataStore (MetadataOperand _) _ = undefined

    emitPointerOperandMetadataLoad (LocalReference argType argName) ix = do
      ix' <- pure $ int32 ix
      (baseName, baseProto) <- gets((!! "__softboundcets_load_base_shadow_stack") . runtimeFunctionPrototypes)
      base <- call (ConstantOperand $ GlobalReference (ptr baseProto) $ mkName baseName) [(ix', [])]
      (boundName, boundProto) <- gets((!! "__softboundcets_load_bound_shadow_stack") . runtimeFunctionPrototypes)
      bound <- call (ConstantOperand $ GlobalReference (ptr boundProto) $ mkName boundName) [(ix', [])]
      (keyName, keyProto) <- gets((!! "__softboundcets_load_key_shadow_stack") . runtimeFunctionPrototypes)
      key <- call (ConstantOperand $ GlobalReference (ptr keyProto) $ mkName keyName) [(ix', [])]
      (lockName, lockProto) <- gets((!! "__softboundcets_load_lock_shadow_stack") . runtimeFunctionPrototypes)
      lock <- call (ConstantOperand $ GlobalReference (ptr lockProto) $ mkName lockName) [(ix', [])]
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
      -- We get the metadata for that pointer by calling __softboundcets_metadata_load()
      | (Load _ addr@(LocalReference (PointerType (PointerType _ _) _) _) _ _ _) <- o = do
        base <- alloca (ptr i8) Nothing 8
        bound <- alloca (ptr i8) Nothing 8
        key <- alloca (i64) Nothing 8
        lock <- alloca (ptr i8) Nothing 8
        addr' <- bitcast addr (ptr i8)
        (fname, fproto) <- gets ((!! "__softboundcets_metadata_load") . runtimeFunctionPrototypes)
        _ <- call (ConstantOperand $ GlobalReference (ptr fproto) $ mkName fname)
                  [(addr', []), (base, []), (bound, []), (key, []), (lock, [])]
        modify $ \s -> s { metadataTable = Data.Map.insert addr' (base, bound, key, lock) $ metadataTable s }
        emitNamedInst i

      | (Call _ _ _ (Right (ConstantOperand (GlobalReference (FunctionType rt _ False) _))) opds _ _) <- o = do
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

      | otherwise = emitNamedInst i

    instrumentInst i = emitNamedInst i

    isPointerOperand (LocalReference (PointerType _ _) _) = True
    isPointerOperand _ = False

    isPointerType (PointerType _ _) = True
    isPointerType _ = False

    emitShadowStackAllocation numArgs = do
      numArgs' <- pure $ int32 numArgs
      (fname, fproto) <- gets ((!! "__softboundcets_allocate_shadow_stack_space") . runtimeFunctionPrototypes)
      _ <- call (ConstantOperand $ GlobalReference (ptr fproto) $ mkName fname)
                [(numArgs', [])]
      return ()

    emitShadowStackDeallocation = do
      (fname, fproto) <- gets ((!! "__softboundcets_deallocate_shadow_stack_space") . runtimeFunctionPrototypes)
      _ <- call (ConstantOperand $ GlobalReference (ptr fproto) $ mkName fname) []
      return ()

    emitNamedInst (n := i) = do
      modifyBlock $ \bb -> bb
        { partialBlockInstrs = partialBlockInstrs bb `snoc` (n := i) }

    emitNamedInst (Do i) = do
      emitInstrVoid i
