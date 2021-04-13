module SoftboundCETS (instrument) where

import Data.Set hiding (map, filter)
import Data.String (IsString(..))
import LLVM.AST
import LLVM.AST.Global
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Internal.SnocList
import SoftboundCETSDefinitions

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
    functionsToInstrument defs = union (singleton $ mkName "main")
                                       (difference (fromList $ map getFuncName
                                                             $ filter isFuncDef
                                                             $ defs)
                                                   (union ignoredFunctions
                                                          wrappedFunctions))

    isFuncDef (GlobalDefinition (Function {})) = True
    isFuncDef _ = False

    getFuncName (GlobalDefinition f@(Function {})) = name f
    getFuncName _ = undefined

    instrumentFunction ifs g@(GlobalDefinition f@(Function {})) =
      if member (name f) ifs then
        GlobalDefinition $ f { basicBlocks = execIRBuilder emptyIRBuilder $ instrumentBlocks $ basicBlocks f }
      else g

    instrumentFunction _ x = x

    instrumentBlocks blocks = do
      mapM emitBlock blocks

    emitBlock (BasicBlock n i t) = do
      emitBlockStart n
      mapM_ instrumentInst i
      emitNamedTerm t

    emitNamedInst (n := i) = do
      modifyBlock $ \bb -> bb
        { partialBlockInstrs = partialBlockInstrs bb `snoc` (n := i) }

    emitNamedInst (Do i) = do
      emitInstrVoid i

    emitNamedTerm (_ := _) = undefined
    -- we should never see this happen
    -- "named terminator" is a quirk of LLVM IR

    emitNamedTerm (Do t) = do
      modifyBlock $ \bb -> bb
        { partialBlockTerm = Just (Do t) }

    instrumentInst i@(_ := o)
      | (Load {}) <- o = do
        -- ~ call (ConstantOperand $ GlobalReference "__softboundcets_spatial_load_deference_check" ...
        -- ~ call (ConstantOperand $ GlobalReference "__softboundcets_temporal_load_deference_check" ...
        emitNamedInst i

      | (Store {}) <- o = do
        -- ~ call (ConstantOperand $ GlobalReference "__softboundcets_spatial_store_deference_check" ...
        -- ~ call (ConstantOperand $ GlobalReference "__softboundcets_temporal_store_deference_check" ...
        emitNamedInst i

    instrumentInst i = emitNamedInst i
