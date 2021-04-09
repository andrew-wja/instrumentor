module Main where

import qualified Lib
import qualified SoftboundCETS

import Main.Utf8 (withUtf8, withStdTerminalHandles)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import LLVM.Module (linkModules)

main :: IO ()
main = withStdTerminalHandles $ withUtf8 $ do
  args <- getArgs

  if (length args) == 1 then do
    let inputFile = head args
    parsed <- Lib.readBC inputFile
    (sbc, instrumented) <- SoftboundCETS.instrument parsed
    sbc' <- Lib.fromAST sbc
    instrumented' <- Lib.fromAST instrumented
    linkModules instrumented' sbc'
    outputModule <- Lib.toAST instrumented'
    Lib.writeBC inputFile outputModule
    exitSuccess
  else do
    putStrLn "usage: instrumentor <file>.bc"
    exitFailure
