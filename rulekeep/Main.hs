module Main where

import Data.List
import Data.Monoid
import qualified Data.Text.IO as TIO
import Data.Yaml
import Control.Monad
import Options.Applicative
import System.Directory
import System.FilePath
import System.IO

import Game.Agora.Data.Index
import Game.Agora.Data.Proposal
import Game.Agora.Data.Rule
import Game.Agora.Lint
import Game.Agora.Ruleset

data Command = SLR | FLR | Lint
  deriving (Show)

commands = subparser (command "slr" (info (pure SLR) $ progDesc "Print the SLR")
                   <> command "flr" (info (pure FLR) $ progDesc "Print the FLR")
                   <> command "lint" (info (pure Lint) $ progDesc "Lint data")
                     )
commandLine =
  info (commands <**> helper) (fullDesc
                            <> progDesc "Manage the Agora ruleset"
                            <> header "rulekeep - Agoran Rulekeepor tools"
                              )

main :: IO ()
main = do
    options <- execParser commandLine
    let ruleDir = "rules"
    let propDir = "proposals"
    ruleFiles <- fmap (map (ruleDir </>)) $ listDirectory ruleDir
    propFiles <- fmap (map (propDir </>)) $ listDirectory propDir
    rules <- mapM decodeFileEither ruleFiles
    props <- mapM decodeFileEither propFiles
    index <- decodeFileEither "index"
    let rules' = map (uncurry extract) $ zip ruleFiles rules
    let props' = map (uncurry extract) $ zip propFiles props
    let index' = extract "index" index
    case options of
      SLR -> TIO.putStr $ slr rules' props' index'
      FLR -> TIO.putStr $ flr rules' props' index'
      Lint -> lint rules' props' index'
  where
    extract :: FilePath -> Either ParseException a -> a
    extract f (Left e) = error $ f ++ ": " ++ prettyPrintParseException e
    extract f (Right a) = a
