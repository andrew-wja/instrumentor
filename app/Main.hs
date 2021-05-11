module Main where

import qualified CLI
import qualified Utils
import qualified SoftboundCETS

import Main.Utf8 (withUtf8, withStdTerminalHandles)
import System.Exit (exitFailure, exitSuccess)
import Data.List (isSuffixOf)
import Data.Char (isSpace)
import Options.Applicative (execParser)

main :: IO ()
main = withStdTerminalHandles $ withUtf8 $ do
  opts <- execParser CLI.options

  blacklist <- if (null $ CLI.blacklist opts)
               then return []
               else do
                 text <- readFile $ CLI.blacklist opts
                 return $
                   filter (not . null) $
                   map (filter (not . isSpace)) $
                   lines text

  if isSuffixOf ".bc" $ CLI.file opts
  then do
    parsed <- Utils.readBC $ CLI.file opts
    instrumented <- SoftboundCETS.instrument blacklist opts parsed
    Utils.writeBC (CLI.file opts) instrumented
    exitSuccess
  else if isSuffixOf ".ll" $ CLI.file opts
  then do
    parsed <- Utils.readIR $ CLI.file opts
    instrumented <- SoftboundCETS.instrument blacklist opts parsed
    Utils.writeIR (CLI.file opts) instrumented
    exitSuccess
  else do
    putStrLn ("Invalid input file: " ++ CLI.file opts)
    exitFailure
