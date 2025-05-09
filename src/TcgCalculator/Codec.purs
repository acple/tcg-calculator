module TcgCalculator.Codec
  ( appState
  , module Export
  )
  where

import Prelude

import Codec.JSON.DecodeError (DecodeError)
import Codec.JSON.DecodeError as DecodeError
import Control.Monad.Except (ExceptT, except, lift, runExcept)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Array (elemIndex, sort, (!!))
import Data.Codec (Codec', codec', hoist, (<~<))
import Data.Codec (encode, decode) as Export
import Data.Codec.JSON as CJ
import Data.Codec.JSON.Common as CJC
import Data.Codec.JSON.Record as CJR
import Data.Codec.JSON.Sum as CJS
import Data.Either (note)
import Data.Traversable (fold, traverse)
import Effect (Effect)
import JSON (JSON)
import TcgCalculator.Types (AppCondition, AppConditionGroup, AppConditionSet, AppState, AppStateJson, Card, CardId, CardJson, ConditionGroupJson, ConditionJson, ConditionMode, ConditionSetJson, DeckJson, generateId, readConditionMode)

----------------------------------------------------------------

appState  :: Codec' (ExceptT DecodeError Effect) JSON AppState
appState = appState' <~< hoist (except <<< runExcept) appStateJson

appState' :: Codec' (ExceptT DecodeError Effect) AppStateJson AppState
appState' = codec' decode encode
  where

  decode :: AppStateJson -> ExceptT DecodeError Effect AppState
  decode { deck: { cards, hand, others }, condition: set } = do
    cards' <- traverse decodeCard cards
    condition <- runReaderT (decodeConditionSet set) (cards' <#> _.id)
    pure { deck: { cards: cards', hand, others }, condition }

  decodeCard :: CardJson -> _ Card
  decodeCard { name, count } = { id: _, name, count } <$> generateId

  decodeConditionSet :: ConditionSetJson -> _ AppConditionSet
  decodeConditionSet = traverse \{ conditions, disabled } ->
    { id: _, conditions: _, disabled } <$> generateId <*> decodeConditionGroup conditions

  decodeConditionGroup :: ConditionGroupJson -> _ AppConditionGroup
  decodeConditionGroup = traverse decodeCondition

  decodeCondition :: ConditionJson -> ReaderT (Array CardId) _ AppCondition
  decodeCondition { mode, count, cards, disabled } = do
    ids <- ask
    id <- generateId
    cards' <- lift <<< except <<< note (DecodeError.basic "Could not decode conditions") <<< traverse (ids !! _) $ cards
    pure { id, condition: { mode, count, cards: cards' }, disabled }

  encode :: AppState -> AppStateJson
  encode { deck: { cards, hand, others }, condition: set } = do
    let cards' = encodeCard <$> cards
    let condition = encodeConditionSet set (cards <#> _.id)
    { deck: { cards: cards', hand, others }, condition }

  encodeCard :: Card -> CardJson
  encodeCard { name, count } = { name, count }

  encodeConditionSet :: AppConditionSet -> _ ConditionSetJson
  encodeConditionSet = traverse \{ conditions, disabled } -> { conditions: _, disabled } <$> encodeConditionGroup conditions

  encodeConditionGroup :: AppConditionGroup -> _ ConditionGroupJson
  encodeConditionGroup = traverse encodeCondition

  encodeCondition :: AppCondition -> Array CardId -> ConditionJson
  encodeCondition { condition: { mode, count, cards }, disabled } ids = do
    let cards' = sort <<< fold $ traverse (elemIndex <@> ids) cards
    { mode, count, cards: cards', disabled }

----------------------------------------------------------------

appStateJson :: CJ.Codec AppStateJson
appStateJson = CJR.object { deck: deckJson, condition: setJson }

setJson :: CJ.Codec ConditionSetJson
setJson = CJ.array $ CJR.object { conditions: groupJson, disabled: CJ.boolean }

groupJson :: CJ.Codec ConditionGroupJson
groupJson = CJC.nonEmptyArray $ CJR.object
  { mode: conditionMode
  , count: CJ.int
  , cards: CJ.array CJ.int
  , disabled: CJ.boolean
  }

deckJson :: CJ.Codec DeckJson
deckJson = CJR.object { cards: CJ.array cardJson, others: CJ.int, hand: CJ.int }

cardJson :: CJ.Codec CardJson
cardJson = CJR.object { name: CJ.string, count: CJ.int }

----------------------------------------------------------------

conditionMode :: CJ.Codec ConditionMode
conditionMode = CJS.enumSum show readConditionMode
