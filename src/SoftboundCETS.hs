module SoftboundCETS (instrument) where

import Data.Set hiding (map, filter)
import Data.String (IsString(..))
import LLVM.AST
import LLVM.AST.Global
import LLVM.IRBuilder.Module
import SoftboundCETSDefinitions

instrument :: Module -> IO Module
instrument m = do
  let sbcModule = buildModule (fromString "softboundcets") sbcetsModule
  putStrLn $ show $ functionsToInstrument m
  return $ sbcModule { moduleName = moduleName m,
                       moduleSourceFileName = moduleSourceFileName m,
                       moduleDataLayout = moduleDataLayout m,
                       moduleTargetTriple = moduleTargetTriple m,
                       moduleDefinitions = moduleDefinitions sbcModule ++
                                           moduleDefinitions m }
  where
    functionsToInstrument :: Module -> Set Name
    functionsToInstrument m' = union (singleton $ mkName "main")
                                     (difference (fromList $ map getFuncName
                                                           $ filter isFuncDef
                                                           $ moduleDefinitions m')
                                                 (union ignoredFunctions
                                                        wrappedFunctions))

    isFuncDef (GlobalDefinition (Function {})) = True
    isFuncDef _ = False

    getFuncName (GlobalDefinition f@(Function {})) = name f
    getFuncName _ = undefined
