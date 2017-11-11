module Game.Agora.Data.Proposal where

import Data.Aeson.Types
import Data.Text(Text)
import Data.Yaml
import GHC.Generics

import Game.Agora.Data.Common

data Proposal = Proposal { id :: Text
                         , author :: Maybe Text
                         , title :: Maybe Text
                         , coauthors :: Maybe [Text]
                         , disinterested :: Bool'
                         , campaign :: Maybe Text
                         }
  deriving (Show, Generic)

instance ToJSON Proposal where
  toJSON = genericToJSON $ options "" ""

instance FromJSON Proposal where
  parseJSON = genericParseJSON $ options "" ""
