{-|
Module      : LLVMHelpers
Description : Helper functions for doing things with the LLVM API
Copyright   : (c) Andrew Anderson, 2021
License     : BSD-3
Maintainer  : aanderso@tcd.ie
Stability   : experimental
-}

module LLVMHelpers where

import Data.Map (lookup)
import Data.String (IsString(..))
import Data.List (isInfixOf)
import Control.Monad.State
import LLVM.AST
import LLVM.AST.Constant
import LLVM.AST.Global
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Internal.SnocList

-- | Helper to rewrite the called function symbol (for example, to a wrapper function symbol) at a callsite.
rewriteCalledFunctionName :: Name -> Instruction -> Instruction
rewriteCalledFunctionName n f
  | (Call tckind cconv retAttrs (Right (ConstantOperand (GlobalReference fty _))) params attrs meta) <- f =
      Call tckind cconv retAttrs (Right (ConstantOperand (GlobalReference fty n))) params attrs meta
  | otherwise = error $ "rewriteCalledFunctionName: expected call to function symbol but saw " ++ (show f)

-- | Code generation helper function
emitNamedTerm :: MonadIRBuilder m => Named Terminator -> m ()
emitNamedTerm t = do
  modifyBlock $ \bb -> bb
    { partialBlockTerm = Just t }

-- | Code generation helper function
emitNamedInst :: MonadIRBuilder m => Named Instruction -> m ()
emitNamedInst i
  | (_ := _) <- i = do
      modifyBlock $ \bb -> bb
        { partialBlockInstrs = partialBlockInstrs bb `snoc` i }
  | (Do o) <- i = emitInstrVoid o

-- | Helper function
appendName :: Name -> String -> Name
appendName (Name s) s' = Name (s <> fromString s')
appendName _ _ = undefined

-- | Helper predicate.
isInfixOfName :: String -> Name -> Bool
isInfixOfName s (Name s') = isInfixOf s $ show s'
isInfixOfName _ _ = False

-- | Helper predicate.
isConstantOperand :: Operand -> Bool
isConstantOperand (ConstantOperand {}) = True
isConstantOperand _ = False

-- | Helper predicate.
isLocalReference :: Operand -> Bool
isLocalReference (LocalReference {}) = True
isLocalReference _ = False

-- | Helper predicate.
isPointerType :: Type -> Bool
isPointerType (PointerType {}) = True
isPointerType _ = False

-- | Helper predicate.
isFunctionType :: Type -> Bool
isFunctionType (FunctionType {}) = True
isFunctionType _ = False

-- | Helper predicate.
isFuncDef :: Definition -> Bool
isFuncDef (GlobalDefinition f@(Function {})) = not $ null $ basicBlocks f
isFuncDef _ = False

-- | Helper predicate.
isTypeDef :: Definition -> Bool
isTypeDef (TypeDefinition {}) = True
isTypeDef _ = False

-- | Helper predicate.
getFuncName :: Definition -> Name
getFuncName (GlobalDefinition f@(Function {})) = name f
getFuncName _ = undefined

-- | Index into a type with a list of consecutive 'Operand' indices.
typeIndex :: MonadModuleBuilder m => Type -> [Operand] -> m (Maybe Type)
typeIndex ty [] = pure $ Just ty
typeIndex (PointerType ty _) (_:is') = typeIndex ty is'
typeIndex (StructureType _ elTys) (ConstantOperand (Int 32 val):is') =
  typeIndex (head $ drop (fromIntegral val) elTys) is'
typeIndex (StructureType _ _) (i:_) = error $ "Field indices for structure types must be i32 constants, got: " ++ (show i)
typeIndex (VectorType _ elTy) (_:is') = typeIndex elTy is'
typeIndex (ArrayType _ elTy) (_:is') = typeIndex elTy is'
typeIndex (NamedTypeReference nm) is' = do
  mayTy <- liftModuleState (gets (Data.Map.lookup nm . builderTypeDefs))
  case mayTy of
    Nothing -> pure Nothing
    Just ty -> typeIndex ty is'
typeIndex t (_:_) = error $ "Can't index into type: " ++ (show t)
