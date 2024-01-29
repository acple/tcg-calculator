module TcgCalculator.Types.Id where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Maybe (Maybe, maybe')
import Data.Newtype (class Newtype)
import Data.UUID (UUID)
import Data.UUID as UUID
import Effect.Class (class MonadEffect, liftEffect)

----------------------------------------------------------------

newtype Id = Id UUID

derive instance Eq Id
derive instance Ord Id

derive instance Newtype Id _

instance Show Id where
  show (Id uuid) = "\"" <> UUID.toString uuid <> "\""

instance EncodeJson Id where
  encodeJson (Id uuid) = encodeJson $ UUID.toString uuid

instance DecodeJson Id where
  decodeJson json = decodeJson json <#> \id -> maybe' (\_ -> mkId id) Id (UUID.parseUUID id)

namespaceTcgCalculator :: UUID
namespaceTcgCalculator = UUID.genv5UUID "tcg-calculator" UUID.emptyUUID

mkId :: String -> Id
mkId s = Id $ UUID.genv5UUID s namespaceTcgCalculator

generateId :: forall m. MonadEffect m => m Id
generateId = liftEffect $ Id <$> UUID.genUUID

toString :: Id -> String
toString (Id uuid) = UUID.toString uuid

fromString :: String -> Maybe Id
fromString = map Id <<< UUID.parseUUID
