{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module SoftboundCETS (instrument) where

import Control.Monad.State hiding (void)
import Data.Set hiding (map, filter)
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

data SBCETSState = SBCETSState { globalLockRef :: Maybe Operand }

emptySBCETSState :: SBCETSState
emptySBCETSState = SBCETSState Nothing

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
    functionsToInstrument defs = difference (fromList $ map getFuncName
                                                      $ filter isFuncDef
                                                      $ defs)
                                            (union ignoredFunctions
                                                   wrappedFunctions)

    isFuncDef (GlobalDefinition (Function {})) = True
    isFuncDef _ = False

    getFuncName (GlobalDefinition f@(Function {})) = name f
    getFuncName _ = undefined

    instrumentFunction ifs g@(GlobalDefinition f@(Function {})) =
      if member (name f) ifs then
        let builderState = emptyIRBuilder { builderNameSuggestion = Just $ fromString "sbcets" }
            builder :: StateT SBCETSState IRBuilder () = instrumentBlocks $ basicBlocks f
        in GlobalDefinition $ f { basicBlocks = execIRBuilder builderState $
                                                flip evalStateT emptySBCETSState $
                                                builder }
      else if (name f) == (mkName "main") then
        GlobalDefinition $ f { name = mkName "softboundcets_main" }
      else g

    instrumentFunction _ x = x

    -- We need to make sure that the first block in the function gets a handle
    -- to  __softboundcets_global_lock because we'll need to use it a lot.
    instrumentBlocks [] = return ()

    instrumentBlocks (first:[]) = do
      emitFirstBlock first

    instrumentBlocks (first:blocks) = do
      emitFirstBlock first
      mapM_ emitBlock blocks

    emitFirstBlock (BasicBlock n i t) = do
      emitBlockStart n
      gl <- call (ConstantOperand $ GlobalReference (ptr $ FunctionType (ptr i8) [] False)
                                                    (mkName "__softboundcets_get_global_lock"))
                 []
      modify $ \s -> s { globalLockRef = Just gl } -- record the local variable in the generated IR that contains the handle to the global lock
      mapM_ instrumentInst i
      emitNamedTerm t

    emitBlock (BasicBlock n i t) = do
      emitBlockStart n
      mapM_ instrumentInst i
      emitNamedTerm t

    emitNamedTerm (_ := _) = undefined
    -- we should never see this happen
    -- "named terminator" is a quirk of LLVM IR

    emitNamedTerm (Do t) = do
      modifyBlock $ \bb -> bb
        { partialBlockTerm = Just (Do t) }

    instrumentInst i@(_ := o)
      | (Load _ addr@(LocalReference (PointerType (PointerType _ _) _) _) _ _ _) <- o = do
        base <- alloca (ptr i8) Nothing 8
        bound <- alloca (ptr i8) Nothing 8
        key <- alloca (i64) Nothing 8
        lock <- alloca (ptr i8) Nothing 8
        addr' <- bitcast addr (ptr i8)
        _ <- call (ConstantOperand $ GlobalReference (ptr $ FunctionType void [ptr i8, (ptr $ ptr i8), (ptr $ ptr i8), ptr i64, (ptr $ ptr i8)] False)
                                                     (mkName "__softboundcets_metadata_load"))
                  [(addr', []), (base, []), (bound, []), (key, []), (lock, [])]
        emitNamedInst i

      | (Store {}) <- o = do
        -- ~ call (ConstantOperand $ GlobalReference "__softboundcets_spatial_store_deference_check" ...
        -- ~ call (ConstantOperand $ GlobalReference "__softboundcets_temporal_store_deference_check" ...
        emitNamedInst i

      | otherwise = emitNamedInst i

    instrumentInst i = emitNamedInst i

    emitNamedInst (n := i) = do
      modifyBlock $ \bb -> bb
        { partialBlockInstrs = partialBlockInstrs bb `snoc` (n := i) }

    emitNamedInst (Do i) = do
      emitInstrVoid i
