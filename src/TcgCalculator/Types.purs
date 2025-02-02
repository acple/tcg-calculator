module TcgCalculator.Types
  ( Card
  , CardJson
  , Cards
  , Condition
  , ConditionGroup
  , ConditionGroupExport
  , ConditionGroupJson
  , ConditionMode(..)
  , ConditionSet
  , ConditionSetExport
  , ConditionSetJson
  , Deck
  , DeckJson
  , Export
  , ExportJson
  , WorkerParam
  , module Export
  , readConditionMode
  )
  where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import TcgCalculator.Types.Id (Id)
import TcgCalculator.Types.Id (Id, generateId) as Export

----------------------------------------------------------------

type Card = { id :: Id, name :: String, count :: Int }

type Cards = Array Card

type Deck = { cards :: Cards, others :: Int, hand :: Int }

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

type Condition = { mode :: ConditionMode, count :: Int, cards :: Cards }

type ConditionGroup = NonEmptyArray Condition

type ConditionSet = Array ConditionGroup

----------------------------------------------------------------

type WorkerParam = { deck :: Deck, condition :: ConditionSet }

type Export = { deck :: Deck, condition :: ConditionSetExport }

type ConditionSetExport = Array ConditionGroupExport

type ConditionGroupExport =
  { conditions :: NonEmptyArray { condition :: Condition, disabled :: Boolean }
  , disabled :: Boolean
  }

----------------------------------------------------------------

type ExportJson = { deck :: DeckJson, condition :: ConditionSetJson }

type DeckJson = { cards :: Array CardJson, hand :: Int, others :: Int }

type CardJson = { name :: String, count :: Int }

type ConditionSetJson = Array ConditionGroupJson

type ConditionGroupJson =
  { conditions :: NonEmptyArray { mode :: ConditionMode, count :: Int, cards :: Array Int, disabled :: Boolean }
  , disabled :: Boolean
  }
