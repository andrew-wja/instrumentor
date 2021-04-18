module Main where

import qualified Utils
import qualified SoftboundCETS

import Main.Utf8 (withUtf8, withStdTerminalHandles)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Data.List (isSuffixOf)

main :: IO ()
main = withStdTerminalHandles $ withUtf8 $ do
  args <- getArgs

  if (length args) == 1 then do
    let inputFile = head args
    if isSuffixOf ".bc" inputFile then do
      parsed <- Utils.readBC inputFile
      instrumented <- SoftboundCETS.instrument parsed
      Utils.writeBC inputFile instrumented
      exitSuccess
    else if isSuffixOf ".ll" inputFile then do
      parsed <- Utils.readIR inputFile
      instrumented <- SoftboundCETS.instrument parsed
      Utils.writeIR inputFile instrumented
      exitSuccess
    else do
      putStrLn ("Invalid input file: " ++ inputFile)
      exitFailure
  else do
    putStrLn "usage: instrumentor <file>.bc"
    exitFailure
