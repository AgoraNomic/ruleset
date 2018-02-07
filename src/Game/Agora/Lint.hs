module Game.Agora.Lint where

import Control.Monad
import Data.Maybe
import Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO

import qualified Game.Agora.Data.Index as AI
import qualified Game.Agora.Data.Rule as AR
import qualified Game.Agora.Data.Proposal as AP

lint :: AR.RuleMap -> AP.PropMap -> AI.Index -> IO ()
lint rules props index = do
  mapM_ lintRule rules
  checkMissing rules index

lintRule :: AR.Rule -> IO ()
lintRule r = return ()

checkMissing :: AR.RuleMap -> AI.Index -> IO ()
checkMissing rules index = do
    let
      missing = M.keys rules \\ concatMap AI.rules index
      bad = filter (not . repealed) $ catMaybes $ map (\k -> M.lookup k rules) $ missing
    mapM_ (\r -> putStrLn $ ("Unrepealed rule missing from index: " ++) $
                 show $ AR.id $ r) bad
  where
    repealed r = case AR.rcChange $ last $ AR.history $ r of
      AR.Repeal -> True
      _ -> False
