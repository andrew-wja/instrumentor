{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module SoftboundCETS (instrument) where

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
  ("__softboundcets_store_lock_shadow_stack", FunctionType void [ptr i8, i32] False)
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
      if (Data.Set.member (name f) ifs) && (not $ null $ basicBlocks f) then
        let firstBlockLabel = bbName $ head $ basicBlocks f
            builderState = emptyIRBuilder { builderNameSuggestion = Just $ fromString "sbcets" }
            builder :: StateT SBCETSState IRBuilder () = do
              -- We do not currently instrument pointer parameters to varargs functions
              when (not $ snd $ parameters f) (instrumentPointerArgs firstBlockLabel $ fst $ parameters f)
              instrumentBlocks $ basicBlocks f
        in GlobalDefinition $ f { basicBlocks = execIRBuilder builderState $
                                                flip evalStateT emptySBCETSState $
                                                builder }
      else if (name f) == (mkName "main") then
        GlobalDefinition $ f { name = mkName "softboundcets_main" }

      else g

    instrumentFunction _ x = x

    bbName (BasicBlock n _ _) = n

    -- Set up the instrumentation of any pointer arguments to the function, and
    -- then branch unconditionally to the first block in the function body.

    instrumentPointerArgs fblabel pms = do
      let pointerArgs = filter isPointerArg pms
      let shadowStackIndices :: [Integer] = [1..]
      emitBlockStart (mkName "sbcets_parameter_metadata_init")
      mapM_ emitPointerArgumentMetadataLoad $ zip pointerArgs shadowStackIndices
      emitTerm $ Br fblabel []
      where
        isPointerArg (Parameter (PointerType _ _) _ _) = True
        isPointerArg _ = False

    -- Load the metadata for a pointer argument from the shadow stack.
    -- The location of that metadata in the shadow stack is given by the position
    -- of the pointer argument in the list of pointer arguments (starting from zero).

    emitPointerArgumentMetadataLoad ((Parameter argType argName _), ix) = do
      ix' <- pure $ int32 ix
      let baseName = mkName "__softboundcets_load_base_shadow_stack"
      baseProto <- gets((! "__softboundcets_load_base_shadow_stack") . runtimeFunctionPrototypes)
      base <- call (ConstantOperand $ GlobalReference (ptr baseProto) baseName) [(ix', [])]
      let boundName = mkName "__softboundcets_load_bound_shadow_stack"
      boundProto <- gets((! "__softboundcets_load_bound_shadow_stack") . runtimeFunctionPrototypes)
      bound <- call (ConstantOperand $ GlobalReference (ptr boundProto) boundName) [(ix', [])]
      let keyName = mkName "__softboundcets_load_key_shadow_stack"
      keyProto <- gets((! "__softboundcets_load_key_shadow_stack") . runtimeFunctionPrototypes)
      key <- call (ConstantOperand $ GlobalReference (ptr keyProto) keyName) [(ix', [])]
      let lockName = mkName "__softboundcets_load_lock_shadow_stack"
      lockProto <- gets((! "__softboundcets_load_lock_shadow_stack") . runtimeFunctionPrototypes)
      lock <- call (ConstantOperand $ GlobalReference (ptr lockProto) lockName) [(ix', [])]
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
      let fname = mkName "__softboundcets_get_global_lock"
      fproto <- gets ((! "__softboundcets_get_global_lock") . runtimeFunctionPrototypes)
      glp <- call (ConstantOperand $ GlobalReference (ptr fproto) fname) []
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
      emitShadowStackInitialization op 0
      emitNamedTerm x

    instrumentTerm x = emitNamedTerm x

    -- If the pointer is in the metadataTable, emit the shadow stack initialization
    -- code for the pointer's base, bound, key, and lock, placing them in the
    -- shadow stack at the specified position.

    emitShadowStackInitialization op ix = do
      maybeBBKL <- gets (Data.Map.lookup op . metadataTable)
      case maybeBBKL of
        (Just (base, bound, key, lock)) -> do
          ix' <- pure $ int32 ix
          baseCast <- bitcast base (ptr i8)
          let baseName = mkName "__softboundcets_store_base_shadow_stack"
          baseProto <- gets ((! "__softboundcets_store_base_shadow_stack") .runtimeFunctionPrototypes)
          _ <- call (ConstantOperand $ GlobalReference (ptr baseProto) baseName)
                    [(baseCast, []), (ix', [])]
          boundCast <- bitcast bound (ptr i8)
          let boundName = mkName "__softboundcets_store_bound_shadow_stack"
          boundProto <- gets ((! "__softboundcets_store_bound_shadow_stack") .runtimeFunctionPrototypes)
          _ <- call (ConstantOperand $ GlobalReference (ptr boundProto) boundName)
                    [(boundCast, []), (ix', [])]
          let keyName = mkName "__softboundcets_store_key_shadow_stack"
          keyProto <- gets ((! "__softboundcets_store_key_shadow_stack") .runtimeFunctionPrototypes)
          _ <- call (ConstantOperand $ GlobalReference (ptr keyProto) keyName)
                    [(key, []), (ix', [])]
          lockCast <- bitcast lock (ptr i8)
          let lockName = mkName "__softboundcets_store_lock_shadow_stack"
          lockProto <- gets ((! "__softboundcets_store_lock_shadow_stack") .runtimeFunctionPrototypes)
          _ <- call (ConstantOperand $ GlobalReference (ptr lockProto) lockName)
                    [(lockCast, []), (ix', [])]
          return ()
        Nothing -> return ()

    -- We should never see this happen -- "named terminator" is a quirk of LLVM IR

    emitNamedTerm (_ := _) = undefined

    emitNamedTerm (Do t) = do
      modifyBlock $ \bb -> bb
        { partialBlockTerm = Just (Do t) }

    instrumentInst i@(_ := o)
      -- Instrument a load instruction if it is loading a pointer.
      -- We get the metadata for that pointer by calling __softboundcets_metadata_load()
      | (Load _ addr@(LocalReference (PointerType (PointerType _ _) _) _) _ _ _) <- o = do
        base <- alloca (ptr i8) Nothing 8
        bound <- alloca (ptr i8) Nothing 8
        key <- alloca (i64) Nothing 8
        lock <- alloca (ptr i8) Nothing 8
        addr' <- bitcast addr (ptr i8)
        let fname = mkName "__softboundcets_metadata_load"
        fproto <- gets ((! "__softboundcets_metadata_load") . runtimeFunctionPrototypes)
        _ <- call (ConstantOperand $ GlobalReference (ptr fproto) fname)
                  [(addr', []), (base, []), (bound, []), (key, []), (lock, [])]
        modify $ \s -> s { metadataTable = Data.Map.insert addr' (base, bound, key, lock) $ metadataTable s }
        emitNamedInst i

      | otherwise = emitNamedInst i

    instrumentInst i = emitNamedInst i

    emitNamedInst (n := i) = do
      modifyBlock $ \bb -> bb
        { partialBlockInstrs = partialBlockInstrs bb `snoc` (n := i) }

    emitNamedInst (Do i) = do
      emitInstrVoid i
