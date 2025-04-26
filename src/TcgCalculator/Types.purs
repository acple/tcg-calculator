module TcgCalculator.Types
  ( AppCondition
  , AppConditionGroup
  , AppConditionSet
  , AppState
  , AppStateJson
  , Card
  , CardId
  , CardJson
  , Cards
  , Condition
  , ConditionGroup
  , ConditionGroupJson
  , ConditionId
  , ConditionJson
  , ConditionMode(..)
  , ConditionSet
  , ConditionSetJson
  , Deck
  , DeckJson
  , GroupId
  , WorkerParam
  , module Export
  , readConditionMode
  , filterCards
  , toConditionGroup
  , toConditionSet
  )
  where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NE
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import TcgCalculator.Types.Id (Id)
import TcgCalculator.Types.Id (Id, generateId) as Export

----------------------------------------------------------------

type CardId = Id "card"

type Card = { id :: CardId, name :: String, count :: Int }

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

type ConditionSet = Array ConditionGroup

type ConditionGroup = NonEmptyArray Condition

type Condition = { mode :: ConditionMode, count :: Int, cards :: Array CardId }

filterCards :: Array CardId -> Cards -> Cards
filterCards ids = Array.filter (_.id >>> Array.elem <@> ids)

----------------------------------------------------------------

type WorkerParam = { deck :: Deck, condition :: ConditionSet }

----------------------------------------------------------------

type GroupId = Id "group"

type ConditionId = Id "condition"

type AppState = { deck :: Deck, condition :: AppConditionSet }

type AppConditionSet = Array { id :: GroupId, conditions :: AppConditionGroup, disabled :: Boolean }

type AppConditionGroup = NonEmptyArray AppCondition

type AppCondition = { id :: ConditionId, condition :: Condition, disabled :: Boolean }

toConditionSet :: AppConditionSet -> ConditionSet
toConditionSet = Array.filter (not _.disabled) >>> Array.sortBy (comparing _.id) >>> Array.mapMaybe (_.conditions >>> toConditionGroup)

toConditionGroup :: AppConditionGroup -> Maybe ConditionGroup
toConditionGroup = NE.filter (not _.disabled) >>> Array.sortBy (comparing _.id) >>> map _.condition >>> NE.fromArray

----------------------------------------------------------------

type AppStateJson = { deck :: DeckJson, condition :: ConditionSetJson }

type DeckJson = { cards :: Array CardJson, hand :: Int, others :: Int }

type CardJson = { name :: String, count :: Int }

type ConditionSetJson = Array { conditions :: ConditionGroupJson, disabled :: Boolean }

type ConditionGroupJson = NonEmptyArray ConditionJson

type ConditionJson = { mode :: ConditionMode, count :: Int, cards :: Array Int, disabled :: Boolean }
