module SoftboundCETS (instrument) where

import qualified LLVM.AST as AST

instrument :: AST.Module -> IO AST.Module
instrument m = return m
