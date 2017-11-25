module Main where

import Data.List
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Yaml
import Control.Monad
import Options.Applicative
import System.Directory
import System.FilePath
import System.IO

import Game.Agora.Data.Index
import Game.Agora.Data.Proposal as AP
import Game.Agora.Data.Rule as AR
import Game.Agora.Lint
import Game.Agora.Ruleset

data Command = SLR | FLR | Lint
  deriving (Show)

ruleDir = "rules"
propDir = "proposals"

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
    ruleFiles <- fmap (map (ruleDir </>)) $ listDirectory ruleDir
    propFiles <- fmap (map (propDir </>)) $ listDirectory propDir
    rulesRaw <- mapM decodeFileEither ruleFiles
    propsRaw <- mapM decodeFileEither propFiles
    indexRaw <- decodeFileEither "index"
    let
      rules' = map (uncurry extract) $ zip ruleFiles rulesRaw
      props' = map (uncurry extract) $ zip propFiles propsRaw
      index = extract "index" indexRaw
      rules = M.fromList $ map (\r -> (AR.name r, r)) rules'
      props = M.fromList $ map (\p -> (AP.id p, p)) props'
    err <- (&&) <$> checkDup "rules" AR.name rules' <*> checkDup "props" AP.id props'
    unless err $ case options of
      SLR -> TIO.putStr $ slr rules props index
      FLR -> TIO.putStr $ flr rules props index
      Lint -> lint rules props index
  where
    extract :: FilePath -> Either ParseException a -> a
    extract f (Left e) = error $ f ++ ": " ++ prettyPrintParseException e
    extract f (Right a) = a

    checkDup :: T.Text -> (v -> T.Text) -> [v] -> IO Bool
    checkDup n f vs =
      let longs = filter ((> 2) . length) $ group $ map f vs
      in do
        mapM_ (\ks -> TIO.putStrLn $ T.concat ["Duplicate ", n, " ID: ", head ks]) longs
        return $ not $ null longs
