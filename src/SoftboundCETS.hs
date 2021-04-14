{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module SoftboundCETS (instrument) where

import Control.Monad.State hiding (void)
import Data.Set hiding (map, filter)
import Data.Map hiding (map, filter)
import Data.String (IsString(..))
import LLVM.AST
import LLVM.AST.Global
import LLVM.AST.Type
import LLVM.AST.Constant
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Internal.SnocList
import SoftboundCETSDefinitions

data SBCETSState = SBCETSState { globalLockPtr :: Maybe Operand
                               , runtimeFunctionPrototypes :: Map String Type
                               }

emptySBCETSState :: SBCETSState
emptySBCETSState = SBCETSState Nothing $ Data.Map.fromList [
  ("__softboundcets_get_global_lock", FunctionType (ptr i8) [] False),
  ("__softboundcets_metadata_load", FunctionType void [ptr i8, (ptr $ ptr i8), (ptr $ ptr i8), ptr i64, (ptr $ ptr i8)] False)
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
      if Data.Set.member (name f) ifs then
        let builderState = emptyIRBuilder { builderNameSuggestion = Just $ fromString "sbcets" }
            builder :: StateT SBCETSState IRBuilder () = instrumentBlocks $ basicBlocks f
        in GlobalDefinition $ f { basicBlocks = execIRBuilder builderState $
                                                flip evalStateT emptySBCETSState $
                                                builder }
      else if (name f) == (mkName "main") then
        GlobalDefinition $ f { name = mkName "softboundcets_main" }

      else g

    instrumentFunction _ x = x

    -- The very first thing we need to do in any function is to call
    -- __softboundcets_get_global_lock(), unless the function body is empty

    instrumentBlocks [] = return ()

    instrumentBlocks (first:[]) = do
      emitFirstBlock first
      modify $ \s -> s { globalLockPtr = Nothing }

    instrumentBlocks (first:blocks) = do
      emitFirstBlock first
      mapM_ emitBlock blocks
      modify $ \s -> s { globalLockPtr = Nothing }

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
      emitNamedTerm t

    emitBlock (BasicBlock n i t) = do
      emitBlockStart n
      mapM_ instrumentInst i
      emitNamedTerm t

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
        emitNamedInst i

      | otherwise = emitNamedInst i

    instrumentInst i = emitNamedInst i

    emitNamedInst (n := i) = do
      modifyBlock $ \bb -> bb
        { partialBlockInstrs = partialBlockInstrs bb `snoc` (n := i) }

    emitNamedInst (Do i) = do
      emitInstrVoid i
