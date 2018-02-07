module Game.Agora.Data.Index where

import Data.Aeson.Types
import Data.Map.Strict(Map)
import Data.Text(Text)
import Data.Yaml
import GHC.Generics

import Game.Agora.Data.Common

data Category = Category { name :: Text
                         , rules :: [Text]
                         }
  deriving (Show, Generic)

instance ToJSON Category where
  toJSON = genericToJSON $ options "" ""

instance FromJSON Category where
  parseJSON = genericParseJSON $ options "" ""

type Index = [Category]
