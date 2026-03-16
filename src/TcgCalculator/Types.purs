module TcgCalculator.Types
  ( AppCondition
  , AppConditionGroup
  , AppConditionSet
  , AppState
  , Card
  , CardId
  , Cards
  , Condition
  , ConditionGroup
  , ConditionId
  , ConditionMode(..)
  , ConditionSet
  , Deck
  , GroupId
  , HashCard
  , HashCondition
  , HashConditionGroup
  , HashConditionSet
  , HashDeck
  , HashState
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
import Data.Set (Set)
import Data.Set as Set
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

type Condition = { mode :: ConditionMode, count :: Int, cards :: Set CardId }

filterCards :: Set CardId -> Cards -> Cards
filterCards = Array.filter <<< flip (Set.member <<< _.id)

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
toConditionSet = Array.filter (not _.disabled) >>> Array.sortWith _.id >>> Array.mapMaybe (_.conditions >>> toConditionGroup)

toConditionGroup :: AppConditionGroup -> Maybe ConditionGroup
toConditionGroup = NE.filter (not _.disabled) >>> Array.sortWith _.id >>> map _.condition >>> NE.fromArray

----------------------------------------------------------------

type HashState = { deck :: HashDeck, condition :: HashConditionSet }

type HashDeck = { cards :: Array HashCard, hand :: Int, others :: Int }

type HashCard = { name :: String, count :: Int }

type HashConditionSet = Array { conditions :: HashConditionGroup, disabled :: Boolean }

type HashConditionGroup = NonEmptyArray HashCondition

type HashCondition = { mode :: ConditionMode, count :: Int, cards :: Array Int, disabled :: Boolean }
