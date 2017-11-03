module Game.Agora.Data.Rule where

import Cases
import Data.Aeson.Types
import Data.Functor
import Data.Maybe
import Data.Scientific
import Data.Text (Text)
import Data.Text as T
import Data.Time
import Data.Yaml
import GHC.Generics

stripPrefix' :: Text -> Text -> Text
stripPrefix' p t = fromMaybe t $ T.stripPrefix p t

stripSuffix' :: Text -> Text -> Text
stripSuffix' s t = fromMaybe t $ T.stripSuffix s t

options :: Text -> Text -> Options
options p s = defaultOptions { fieldLabelModifier = T.unpack . spinalize . stripPrefix' p . T.pack
                             , constructorTagModifier = T.unpack . spinalize . stripSuffix' s . T.pack
                             , omitNothingFields = True
                             , allNullaryToStringTag = True
                             }

data True' = True' deriving (Show, Generic)
type Bool' = Maybe True'

instance ToJSON True' where
  toJSON True' = String "true"

instance FromJSON True' where
  parseJSON _ = return True'

data Mutability = Mutable | Immutable
  deriving (Show, Generic)

instance ToJSON Mutability where
  toJSON = genericToJSON $ options "" ""

data ChangeType = Initial { initialMutability :: Mutability
                          , initialId :: Text
                          }
                | Enactment
                | Amendment { uncounted :: Bool' }
                | Retitling
                | Mutation { oldMi :: Text
                           , newMi :: Text
                           }
                | PowerChange { oldPower :: Maybe Scientific
                              , newPower :: Maybe Scientific
                              }
                | Repeal
                | Reenactment
                | Infection
                | InfectionAmendment { uncounted :: Bool' }
                | RetitlingAmendment
                | PowerChangeAmendment
  deriving (Show, Generic)

ctSumEncoding :: SumEncoding
ctSumEncoding = defaultTaggedObject { tagFieldName = "type" }

instance ToJSON ChangeType where
  toJSON = genericToJSON $ (options "" "") { sumEncoding = ctSumEncoding }

data ChangeDate = Date Day
                | Around { around :: Day }
                | Range { between :: Day
                        , and :: Day
                        }
  deriving (Show, Generic)

instance ToJSON ChangeDate where
  toJSON = genericToJSON $ (options "" "") { sumEncoding = UntaggedValue }

data Agent = ProposalAgent Text
           | RuleAgent Text
           | CleaningAgent { by :: Text }
           | RatificationAgent { document :: Text
                               , agent :: Agent
                               }
  deriving (Show, Generic)

instance ToJSON Agent where
  toJSON = genericToJSON $ (options "" "Agent") { sumEncoding = ObjectWithSingleField }

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

data CFJ = CFJ { cfjId :: Text
               , cfjCalled :: Maybe Day
               }
  deriving (Show, Generic)

instance ToJSON CFJ where
  toJSON = genericToJSON $ options "cfj" ""

data Annotation = Annotation { annCfjs :: [CFJ]
                             , annText :: Text
                             }
  deriving (Show, Generic)

instance ToJSON Annotation where
  toJSON = genericToJSON $ options "ann" ""
  toEncoding = genericToEncoding $ options "ann" ""

data Rule = Rule { name :: Text
                 , history :: [RuleChange]
                 , text :: Text
                 , id :: Int
                 , rev :: Int
                 , power :: Scientific
                 , annotations :: [Annotation]
                 }
  deriving (Show, Generic)

instance ToJSON Rule where
  toJSON = genericToJSON $ options "" ""
  toEncoding = genericToEncoding $ options "" ""
