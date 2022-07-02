module TcgCalculator.Types where

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJsonWith)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJsonWith)
import Data.Argonaut.Types.Generic (defaultEncoding)
import Data.Generic.Rep (class Generic)
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
  encodeJson = genericEncodeJsonWith defaultEncoding { unwrapSingleArguments = true }

instance DecodeJson ConditionMode where
  decodeJson = genericDecodeJsonWith defaultEncoding { unwrapSingleArguments = true }

type Condition' = { mode :: ConditionMode, count :: Int, cards :: Cards }

newtype Condition = Condition Condition'

derive newtype instance Eq Condition
derive newtype instance Ord Condition
derive newtype instance Show Condition
derive instance Newtype Condition _

derive newtype instance EncodeJson Condition
derive newtype instance DecodeJson Condition
