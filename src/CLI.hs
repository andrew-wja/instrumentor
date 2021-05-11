module CLI where

import Options.Applicative

data Options = Options { instrumentLoad :: Bool
                       , instrumentStore :: Bool
                       , instrumentBitcast :: Bool
                       , instrumentCall :: Bool
                       , emitChecks :: Bool
                       -- ~ , instrumentHeap :: Bool
                       -- ~ , instrumentStack :: Bool
                       -- ~ , instrumentGlobals :: Bool
                       , file :: String
                       , blacklist :: String
                       }

defaultOptions :: Options
defaultOptions = Options { instrumentLoad = False
                         , instrumentStore = False
                         , instrumentBitcast = False
                         , instrumentCall = False
                         , emitChecks = False
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
  -- ~ <*> switch
      -- ~ ( long "heap"
      -- ~ <> help "Instrument heap allocations" )
  -- ~ <*> switch
      -- ~ ( long "stack"
      -- ~ <> help "Instrument stack allocations" )
  -- ~ <*> switch
      -- ~ ( long "globals"
      -- ~ <> help "Instrument globals (allocations handled by the linker)" )
  <*> argument str
      ( metavar "FILE"
      <> help "LLVM module to instrument" )
  <*> strOption
      ( long "blacklist"
      <> metavar "FILE"
      <> help "Blacklist for ignored function symbols (calls to these functions will not be instrumented)" )

options :: ParserInfo Options
options = info (helper <*> optParser) (fullDesc <> footer "For help with a specific command, use --help")

