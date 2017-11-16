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
lintRule r = return ()
