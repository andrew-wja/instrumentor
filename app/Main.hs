module Main where

import qualified Lib
import qualified SoftboundCETS

import Main.Utf8 (withUtf8, withStdTerminalHandles)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import qualified Data.ByteString as BS

main :: IO ()
main = withStdTerminalHandles $ withUtf8 $ do
  args <- getArgs

  if (length args) == 1 then do
    let inputFile = head args
    parsed <- Lib.readBC inputFile
    (sbc, instrumented) <- SoftboundCETS.instrument parsed
    sbcIR <- Lib.printIR sbc
    BS.putStr sbcIR
    instrumentedIR <- Lib.printIR instrumented
    BS.putStr instrumentedIR
    exitSuccess
  else do
    putStrLn "usage: instrumentor <file>.bc"
    exitFailure
