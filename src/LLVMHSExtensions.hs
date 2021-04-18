module LLVMHSExtensions where

import LLVM.AST
import LLVM.AST.Constant as C
import LLVM.AST.Global
import LLVM.AST.ParameterAttribute
import LLVM.AST.Type
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Instruction
import Control.Monad (forM)

-- | Define and emit a (non-variadic) function definition with attributes
functionWithAttrs
  :: MonadModuleBuilder m
  => Name  -- ^ Function name
  -> [(Type, ParameterName, [ParameterAttribute])]  -- ^ Parameter types and name suggestions
  -> Type  -- ^ Return type
  -> ([Operand] -> IRBuilderT m ())  -- ^ Function body builder
  -> m Operand
functionWithAttrs label argtys retty body = do
  let tys = (\(a,_,_) -> a) <$> argtys
  let attrs = (\(_,_,c) -> c) <$> argtys
  (paramNames, blocks) <- runIRBuilderT emptyIRBuilder $ do
    paramNames <- forM argtys $ \(_, paramName, _) -> case paramName of
      NoParameterName -> fresh
      ParameterName p -> fresh `named` p
    body $ zipWith LocalReference tys paramNames
    return paramNames
  let
    def = GlobalDefinition functionDefaults
      { name        = label
      , parameters  = (zipWith3 (\ty nm at -> Parameter ty nm at) tys paramNames attrs, False)
      , returnType  = retty
      , basicBlocks = blocks
      }
    funty = ptr $ FunctionType retty ((\(a,_,_) -> a) <$> argtys) False
  emitDefn def
  pure $ ConstantOperand $ C.GlobalReference funty label
