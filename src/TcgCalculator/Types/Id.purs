module TcgCalculator.Types.Id where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Maybe (Maybe, fromMaybe')
import Data.Newtype (class Newtype)
import Data.UUID (UUID)
import Data.UUID as UUID
import Effect.Class (class MonadEffect, liftEffect)
import Safe.Coerce (coerce)

----------------------------------------------------------------

newtype Id = Id UUID

derive newtype instance Eq Id
derive newtype instance Ord Id

derive instance Newtype Id _

instance Show Id where
  show (Id uuid) = "\"" <> UUID.toString uuid <> "\""

instance EncodeJson Id where
  encodeJson (Id uuid) = encodeJson $ UUID.toString uuid

instance DecodeJson Id where
  decodeJson json = decodeJson json <#> \id -> fromMaybe' (\_ -> mkId id) (fromString id)

namespaceTcgCalculator :: Id
namespaceTcgCalculator = coerce UUID.genv5UUID "tcg-calculator" UUID.emptyUUID

mkId :: String -> Id
mkId s = coerce UUID.genv5UUID s namespaceTcgCalculator

generateId :: forall m. MonadEffect m => m Id
generateId = liftEffect $ coerce UUID.genUUID

toString :: Id -> String
toString = coerce UUID.toString

fromString :: String -> Maybe Id
fromString = coerce UUID.parseUUID
