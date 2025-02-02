module TcgCalculator.Codec
  ( export
  , workerParam
  , module Export
  )
  where

import Prelude

import Codec.JSON.DecodeError (DecodeError)
import Codec.JSON.DecodeError as DecodeError
import Control.Monad.Except (ExceptT, except, runExcept)
import Data.Array (elemIndex, mapMaybe, (!!))
import Data.Codec (Codec', codec', hoist, (<~<))
import Data.Codec (encode, decode) as Export
import Data.Codec.JSON as CJ
import Data.Codec.JSON.Common as CJC
import Data.Codec.JSON.Record as CJR
import Data.Codec.JSON.Sum as CJS
import Data.Either (note)
import Data.Traversable (for, traverse)
import Effect (Effect)
import JSON (JSON)
import Record as Record
import TcgCalculator.Types (Card, CardJson, Condition, ConditionGroup, ConditionGroupJson, ConditionMode, ConditionSet, ConditionSetJson, Deck, DeckJson, Export, ExportJson, Id, WorkerParam, generateId, readConditionMode)
import TcgCalculator.Types.Id as Id
import Type.Proxy (Proxy(..))

----------------------------------------------------------------

export  :: Codec' (ExceptT DecodeError Effect) JSON Export
export = export' <~< hoist (except <<< runExcept) exportJson

export' :: Codec' (ExceptT DecodeError Effect) ExportJson Export
export' = codec' decode encode
  where

  decode :: ExportJson -> ExceptT DecodeError Effect Export
  decode { deck: { cards, hand, others }, condition: set } = do
    cards' <- traverse (flap $ Record.insert (Proxy @"id") <$> generateId) cards
    set' <- except $ note (DecodeError.basic "Could not decode conditions") do
      for set \group -> do
        { conditions: _, disabled: group.disabled } <$> for group.conditions \{ mode, count, cards: ids, disabled } -> do
          { condition: _, disabled } <<< { mode, count, cards: _ } <$> traverse (cards' !! _) ids
    pure { deck: { cards: cards', hand, others }, condition: set' }

  encode :: Export -> ExportJson
  encode { deck: { cards, hand, others }, condition: set } = do
    let set' = set <#> Record.modify (Proxy @"conditions") do
          map \{ condition: { mode, count, cards: cards' }, disabled } -> { mode, count, cards: mapMaybe (flip elemIndex cards) cards', disabled }
    let cards' = cards <#> \{ name, count } -> { name, count }
    { deck: { cards: cards', hand, others }, condition: set' }

----------------------------------------------------------------

workerParam :: CJ.Codec WorkerParam
workerParam = CJR.object { deck, condition: conditionSet }

conditionSet :: CJ.Codec ConditionSet
conditionSet = CJ.array conditionGroup

conditionGroup :: CJ.Codec ConditionGroup
conditionGroup = CJC.nonEmptyArray condition

condition :: CJ.Codec Condition
condition = CJR.object { mode: conditionMode, count: CJ.int, cards: CJ.array card }

deck :: CJ.Codec Deck
deck = CJR.object { cards: CJ.array card, others: CJ.int, hand: CJ.int }

card :: CJ.Codec Card
card = CJR.object { id, name: CJ.string, count: CJ.int }

id :: CJ.Codec Id
id = CJ.prismaticCodec "Id" Id.fromString Id.toString CJ.string

----------------------------------------------------------------

exportJson :: CJ.Codec ExportJson
exportJson = CJR.object { deck: deckJson, condition: setJson }

setJson :: CJ.Codec ConditionSetJson
setJson = CJ.array groupJson

groupJson :: CJ.Codec ConditionGroupJson
groupJson = CJR.object
  { conditions: CJC.nonEmptyArray $ CJR.object
      { mode: conditionMode
      , count: CJ.int
      , cards: CJ.array CJ.int
      , disabled: CJ.boolean
      }
  , disabled: CJ.boolean
  }

deckJson :: CJ.Codec DeckJson
deckJson = CJR.object { cards: CJ.array cardJson, others: CJ.int, hand: CJ.int }

cardJson :: CJ.Codec CardJson
cardJson = CJR.object { name: CJ.string, count: CJ.int }

----------------------------------------------------------------

conditionMode :: CJ.Codec ConditionMode
conditionMode = CJS.enumSum show readConditionMode
