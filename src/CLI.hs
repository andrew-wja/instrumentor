module CLI where

import Options.Applicative

data Options = Options { ignoreLoad :: Bool
                       , ignoreStore :: Bool
                       , ignoreGetElementPtr :: Bool
                       , ignoreBitcast :: Bool
                       , ignoreCall :: Bool
                       , file :: String
                       }

defaultOptions :: Options
defaultOptions = Options False False False False False ""

optParser :: Parser Options
optParser = Options
  <$> switch
      ( long "load"
      <> help "Ignore load instructions" )
  <*> switch
      ( long "store"
      <> help "Ignore store instructions" )
  <*> switch
      ( long "getelementptr"
      <> help "Ignore getelementptr instructions" )
  <*> switch
      ( long "bitcast"
      <> help "Ignore bitcast instructions" )
  <*> switch
      ( long "call"
      <> help "Ignore call instructions" )
  <*> argument str
      ( metavar "FILE"
      <> help "LLVM module to instrument" )

options :: ParserInfo Options
options = info (helper <*> optParser) (fullDesc <> footer "For help with a specific command, use --help")

