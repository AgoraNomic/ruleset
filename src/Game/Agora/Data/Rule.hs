module Game.Agora.Data.Rule where

import Data.Aeson.Types
import Data.Functor
import Data.Maybe
import Data.Scientific
import Data.Text (Text)
import Data.Time
import Data.Yaml
import GHC.Generics

import Game.Agora.Data.Common

data Mutability = Mutable | Immutable
  deriving (Show, Generic)

instance ToJSON Mutability where
  toJSON = genericToJSON $ options "" ""

instance FromJSON Mutability where
  parseJSON = genericParseJSON $ options "" ""

data ChangeType = Initial { changeMutability :: Mutability
                          , changeId :: Int
                          }
                | Enactment { mi :: Maybe Text }
                | Amendment { uncounted :: Bool' }
                | Retitling
                | Renumbering
                | Mutation { oldMi :: Text
                           , newMi :: Text
                           }
                | PowerChange { oldPower :: Maybe Scientific
                              , newPower :: Maybe Scientific
                              }
                | Repeal
                | Reenactment
                | Infection
                | CommitteeAssignment { committee :: Text }
                | InfectionAmendment { uncounted :: Bool' }
                | Unknown
  deriving (Show, Generic)

ctSumEncoding :: SumEncoding
ctSumEncoding = defaultTaggedObject { tagFieldName = "type" }

instance ToJSON ChangeType where
  toJSON = genericToJSON $ (options "change" "") { sumEncoding = ctSumEncoding }

instance FromJSON ChangeType where
  parseJSON = genericParseJSON $ (options "change" "") { sumEncoding = ctSumEncoding }

data ChangeDate = Date Day
                | UnknownHistory { unknown :: () }
                | Around { around :: Day }
                | Range { between :: Day
                        , and :: Day
                        }
  deriving (Show, Generic)

instance ToJSON ChangeDate where
  toJSON = genericToJSON $ (options "" "") { sumEncoding = UntaggedValue }

instance FromJSON ChangeDate where
  parseJSON = genericParseJSON $ (options "" "") { sumEncoding = UntaggedValue }

data Agent = ProposalAgent Text
           | RuleAgent Int
           | CleaningAgent { by :: Text }
           | RatificationAgent { document :: Text
                               , agent :: Agent
                               }
           | WithoutObjectionAgent Text
           | DecreeAgent Text
  deriving (Show, Generic)

instance ToJSON Agent where
  toJSON = genericToJSON $ (options "" "Agent") { sumEncoding = ObjectWithSingleField }

instance FromJSON Agent where
  parseJSON = genericParseJSON $ (options "" "Agent") { sumEncoding = ObjectWithSingleField }

data RuleChange = RuleChange { rcChange :: ChangeType
                             , rcDate :: ChangeDate
                             , rcAgent :: Maybe Agent
                             , rcSubstantial :: Bool'
                             , rcCosmetic :: Bool'
                             , rcUnattributed :: Bool'
                             }
  deriving (Show, Generic)

instance ToJSON RuleChange where
  toJSON = genericToJSON $ options "rc" ""

instance FromJSON RuleChange where
  parseJSON = genericParseJSON $ options "rc" ""

data CFJ = CFJ { cfjId :: Text
               , cfjCalled :: Maybe ChangeDate
               }
  deriving (Show, Generic)

instance ToJSON CFJ where
  toJSON = genericToJSON $ options "cfj" ""

instance FromJSON CFJ where
  parseJSON = genericParseJSON $ options "cfj" ""

data Annotation = Annotation { annCfjs :: [CFJ]
                             , annText :: Text
                             }
  deriving (Show, Generic)

instance ToJSON Annotation where
  toJSON = genericToJSON $ options "ann" ""

instance FromJSON Annotation where
  parseJSON = genericParseJSON $ options "ann" ""

data Rule = Rule { name :: Text
                 , history :: [RuleChange]
                 , text :: Text
                 , id :: Int
                 , power :: Scientific
                 , annotations :: [Annotation]
                 }
  deriving (Show, Generic)

instance ToJSON Rule where
  toJSON = genericToJSON $ options "" ""

instance FromJSON Rule where
  parseJSON = genericParseJSON $ options "" ""
