module TcgCalculator.Types.Id
  ( Id
  , fromString
  , generateId
  , mkId
  , namespaceTcgCalculator
  , toString
  )
  where

import Prelude

import Data.Maybe (Maybe)
import Data.UUID (UUID)
import Data.UUID as UUID
import Effect.Class (class MonadEffect, liftEffect)
import Safe.Coerce (coerce)

----------------------------------------------------------------

newtype Id :: forall k. k -> Type
newtype Id a = Id UUID
type role Id nominal

derive newtype instance Eq (Id a)
derive newtype instance Ord (Id a)

instance Show (Id a) where
  show (Id uuid) = "\"" <> UUID.toString uuid <> "\""

namespaceTcgCalculator :: forall a. Id a
namespaceTcgCalculator = coerce UUID.genv5UUID "tcg-calculator" UUID.emptyUUID

mkId :: forall a. String -> Id a
mkId s = coerce UUID.genv5UUID s namespaceTcgCalculator

generateId :: forall m a. MonadEffect m => m (Id a)
generateId = liftEffect $ coerce UUID.genUUID

toString :: forall a. Id a -> String
toString = coerce UUID.toString

fromString :: forall a. String -> Maybe (Id a)
fromString = coerce UUID.parseUUID
