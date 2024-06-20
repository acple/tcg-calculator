module TcgCalculator.Codec
  ( export
  , workerParam
  )
  where

import Prelude

import Data.Array.NonEmpty as NE
import Data.Codec.JSON as CJ
import Data.Codec.JSON.Record as CJR
import Data.Codec.JSON.Sum as CJS
import Data.Profunctor (wrapIso)
import TcgCalculator.Types (Card, Condition(..), ConditionMode, Conditions, ConditionsJson, Deck, ExportJson, WorkerParam, readConditionMode)
import TcgCalculator.Types.Id (Id)
import TcgCalculator.Types.Id as Id

----------------------------------------------------------------

export :: CJ.Codec ExportJson
export = CJR.object { deck, conditions: CJ.array conditionsExport }

conditionsExport :: CJ.Codec ConditionsJson
conditionsExport = CJR.object
  { conditions: CJ.array $ CJR.object
      { mode: conditionMode
      , count: CJ.int
      , cards: CJ.array id
      , disabled: CJ.boolean
      }
  , disabled: CJ.boolean
  }

----------------------------------------------------------------

workerParam :: CJ.Codec WorkerParam
workerParam = CJR.object { deck, conditions: CJ.array conditions }

conditions :: CJ.Codec Conditions
conditions = CJ.prismaticCodec "Conditions" NE.fromArray NE.toArray (CJ.array condition)

condition :: CJ.Codec Condition
condition = wrapIso Condition $ CJR.object { mode: conditionMode, count: CJ.int, cards: CJ.array card }

----------------------------------------------------------------

deck :: CJ.Codec Deck
deck = CJR.object { cards: CJ.array card, others: CJ.int, hand: CJ.int }

card :: CJ.Codec Card
card = CJR.object { id, name: CJ.string, count: CJ.int }

id :: CJ.Codec Id
id = CJ.prismaticCodec "Id" Id.fromString Id.toString CJ.string

conditionMode :: CJ.Codec ConditionMode
conditionMode = CJS.enumSum show readConditionMode
