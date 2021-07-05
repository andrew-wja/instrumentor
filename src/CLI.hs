{-|
Module      : CLI
Description : Command-line interface definition
Copyright   : (c) Andrew Anderson, 2021
License     : BSD-3
Maintainer  : aanderso@tcd.ie
Stability   : experimental
-}

module CLI where

import Options.Applicative

data Options = Options { instrumentLoad :: Bool -- ^ Whether to instrument load instructions
                       , instrumentStore :: Bool -- ^ Whether to instrument store instructions
                       , instrumentBitcast :: Bool -- ^ Whether to instrument pointer casts
                       , instrumentCall :: Bool -- ^ Whether to instrument function calls
                       , emitChecks :: Bool -- ^ Whether to emit runtime metadata validity checks
                       , instrumentStack :: Bool -- ^ Whether to instrument stack allocations
                       , file :: String -- ^ The path to the input module for instrumentation
                       , blacklist :: String -- ^ Function symbols in the blacklist will not be instrumented
                       }

defaultOptions :: Options
defaultOptions = Options { instrumentLoad = False
                         , instrumentStore = False
                         , instrumentBitcast = False
                         , instrumentCall = False
                         , emitChecks = False
                         , instrumentStack = False
                         , file = ""
                         , blacklist = ""
                         }

optParser :: Parser Options
optParser = Options
  <$> switch
      ( long "load"
      <> help "Instrument load instructions" )
  <*> switch
      ( long "store"
      <> help "Instrument store instructions" )
  <*> switch
      ( long "bitcast"
      <> help "Instrument bitcast instructions" )
  <*> switch
      ( long "call"
      <> help "Instrument call instructions" )
  <*> switch
      ( long "checks"
      <> help "Enable integrity checks for dynamically loaded metadata" )
  <*> switch
      ( long "stack"
      <> help "Instrument stack allocations" )
  <*> argument str
      ( metavar "FILE"
      <> help "LLVM module to instrument" )
  <*> strOption
      ( long "blacklist"
      <> metavar "FILE"
      <> help "Blacklist for ignored function symbols (calls to these functions will not be instrumented)" )

options :: ParserInfo Options
options = info (helper <*> optParser) (fullDesc <> footer "For help with a specific command, use --help")

