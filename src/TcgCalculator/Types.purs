module TcgCalculator.Types where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

----------------------------------------------------------------

type Id = String -- TODO: use UUID

type Card = { id :: Id, name :: String, count :: Int }

type Cards = Array Card

type Deck = { cards :: Array Card, others :: Int, hand :: Int }

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

type Condition' = { mode :: ConditionMode, count :: Int, cards :: Cards }

newtype Condition = Condition Condition'

derive instance Eq Condition
derive instance Ord Condition
derive newtype instance Show Condition
derive instance Newtype Condition _
