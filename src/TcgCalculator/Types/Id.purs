module TcgCalculator.Types.Id where

import Prelude

import Data.Maybe (Maybe)
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
