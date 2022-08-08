module TcgCalculator.Types where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), maybe')
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.UUID (UUID)
import Data.UUID as UUID
import Effect.Class (class MonadEffect, liftEffect)

----------------------------------------------------------------

newtype Id = Id UUID

derive instance Eq Id
derive instance Ord Id

derive instance Newtype Id _

instance Show Id where
  show (Id uuid) = UUID.toString uuid

instance EncodeJson Id where
  encodeJson = encodeJson <<< show

instance DecodeJson Id where
  decodeJson json = decodeJson json <#> \json' -> maybe' (\_ -> mkId json') Id (UUID.parseUUID json')

namespaceTcgCalculator :: UUID
namespaceTcgCalculator = UUID.genv5UUID "tcg-calculator" UUID.emptyUUID

mkId :: String -> Id
mkId s = Id $ UUID.genv5UUID s namespaceTcgCalculator

generateId :: forall m. MonadEffect m => m Id
generateId = liftEffect $ Id <$> UUID.genUUID

----------------------------------------------------------------

type Card = { id :: Id, name :: String, count :: Int }

type Cards = Array Card

type Deck = { cards :: Array Card, others :: Int, hand :: Int }

----------------------------------------------------------------

data ConditionMode
  = AtLeast
  | JustDraw
  | Remains
  | JustRemains
  | Choice
  | LeftOne
  | LeftAll

derive instance Eq ConditionMode
derive instance Ord ConditionMode
derive instance Generic ConditionMode _

instance Show ConditionMode where
  show = genericShow

instance EncodeJson ConditionMode where
  encodeJson = encodeJson <<< show

instance DecodeJson ConditionMode where
  decodeJson json = note (UnexpectedValue json) <<< readConditionMode =<< decodeJson json

readConditionMode :: String -> Maybe ConditionMode
readConditionMode = case _ of
  "AtLeast" -> Just AtLeast
  "JustDraw" -> Just JustDraw
  "Remains" -> Just Remains
  "JustRemains" -> Just JustRemains
  "Choice" -> Just Choice
  "LeftOne" -> Just LeftOne
  "LeftAll" -> Just LeftAll
  _ -> Nothing

----------------------------------------------------------------

type Condition' = { mode :: ConditionMode, count :: Int, cards :: Cards }

newtype Condition = Condition Condition'

derive instance Eq Condition
derive instance Ord Condition

derive instance Newtype Condition _

derive newtype instance Show Condition
