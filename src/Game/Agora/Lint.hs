module Game.Agora.Lint where

import Data.Maybe
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Game.Agora.Data.Index
import qualified Game.Agora.Data.Rule as AR
import qualified Game.Agora.Data.Proposal as AP

lint :: [AR.Rule] -> [AP.Proposal] -> Index -> IO ()
lint rules props index = do
  mapM_ lintRule rules

lintRule :: AR.Rule -> IO ()
lintRule r = do
  when ((any $ isCompoundChange . AR.rcChange) $ AR.history r) $ do
    TIO.putStrLn $ T.concat ["Rule ", T.pack $ show $ AR.id r, " has compound change"]
  when ((any $ isUnpoweredChange . AR.rcChange) $ AR.history r) $ do
    TIO.putStrLn $ T.concat ["Rule ", T.pack $ show $ AR.id r, " has a power change with no power"]

isCompoundChange :: AR.ChangeType -> Bool
isCompoundChange (AR.RetitlingAmendment{}) = True
isCompoundChange (AR.PowerChangeAmendment{}) = True
isCompoundChange _ = False

isUnpoweredChange :: AR.ChangeType -> Bool
isUnpoweredChange (pc@AR.PowerChange{}) =
  isNothing (AR.oldPower pc) || isNothing (AR.newPower pc)
isUnpoweredChange _ = False
