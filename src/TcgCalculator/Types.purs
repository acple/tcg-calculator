module TcgCalculator.Types
  ( Card
  , Cards
  , Condition'
  , Condition(..)
  , ConditionMode(..)
  , Conditions
  , Deck
  , module Export
  , readConditionMode
  )
  where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import TcgCalculator.Types.Id (Id)
import TcgCalculator.Types.Id (Id, generateId) as Export

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

derive newtype instance Eq Condition
derive newtype instance Ord Condition

derive newtype instance Show Condition

derive newtype instance EncodeJson Condition
derive newtype instance DecodeJson Condition

derive instance Newtype Condition _

type Conditions = NonEmptyArray Condition
