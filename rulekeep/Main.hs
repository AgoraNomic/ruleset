module Main where

import Data.List
import qualified Data.Text.IO as TIO
import Data.Yaml
import Control.Monad
import System.Directory
import System.FilePath
import System.IO

import Game.Agora.Data.Index
import Game.Agora.Data.Proposal
import Game.Agora.Data.Rule
import Game.Agora.Ruleset

main :: IO ()
main = do
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
    TIO.putStr $ slr rules' props' index'
  where
    extract :: FilePath -> Either ParseException a -> a
    extract f (Left e) = error $ f ++ ": " ++ prettyPrintParseException e
    extract f (Right a) = a
