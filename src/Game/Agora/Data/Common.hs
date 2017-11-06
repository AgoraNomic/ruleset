module Game.Agora.Data.Common
( True'(..)
, Bool'
, options
) where

import Cases
import Data.Aeson.Types
import Data.Maybe
import Data.Text(Text)
import Data.Text as T
import GHC.Generics

data True' = True' deriving (Show, Generic)
type Bool' = Maybe True'

instance ToJSON True' where
  toJSON True' = String "true"

instance FromJSON True' where
  parseJSON _ = return True'

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
