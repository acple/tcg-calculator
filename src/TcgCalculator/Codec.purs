module TcgCalculator.Codec
  ( appState
  , module Export
  )
  where

import Prelude

import Codec.JSON.DecodeError (DecodeError)
import Codec.JSON.DecodeError as DecodeError
import Control.Monad.Except (ExceptT(..), except, runExcept)
import Data.Array (elemIndex, sort, (!!))
import Data.Codec (Codec', codec', hoist, (<~<))
import Data.Codec (encode, decode) as Export
import Data.Codec.JSON as CJ
import Data.Codec.JSON.Common as CJC
import Data.Codec.JSON.Record as CJR
import Data.Codec.JSON.Sum as CJS
import Data.Either (note)
import Data.Functor.Compose (Compose(..))
import Data.Newtype (un)
import Data.Traversable (fold, for, sequence, traverse)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import JSON (JSON)
import Prim.Row as Row
import Record as Record
import TcgCalculator.Types (AppState, AppStateJson, CardJson, ConditionGroupJson, ConditionMode, ConditionSetJson, DeckJson, Id, generateId, readConditionMode)
import Type.Proxy (Proxy(..))

----------------------------------------------------------------

appState  :: Codec' (ExceptT DecodeError Effect) JSON AppState
appState = appState' <~< hoist (except <<< runExcept) appStateJson

appState' :: Codec' (ExceptT DecodeError Effect) AppStateJson AppState
appState' = codec' decode encode
  where

  decode :: AppStateJson -> ExceptT DecodeError Effect AppState
  decode { deck: { cards, hand, others }, condition: set } = do
    cards' <- traverse insertId cards
    let ids = cards' <#> _.id
    conditions <- ExceptT <<< sequence <<< note (DecodeError.basic "Could not decode conditions") <<< un Compose $ do
      for set \group -> do
        { conditions: _, disabled: group.disabled } <$> for group.conditions \{ mode, count, cards: indexes, disabled } -> do
          Compose $ insertId <<< { condition: _, disabled } <<< { mode, count, cards: _ } <<< sort <$> traverse (ids !! _) indexes
    set' <- traverse insertId conditions
    pure { deck: { cards: cards', hand, others }, condition: set' }

  insertId :: forall m r a. Row.Lacks "id" r => MonadEffect m => { | r } -> m { id :: Id a | r }
  insertId = flap $ Record.insert (Proxy @"id") <$> generateId

  encode :: AppState -> AppStateJson
  encode { deck: { cards, hand, others }, condition: set } = do
    let ids = cards <#> _.id
    let set' = set <#> \{ conditions, disabled: groupDisabled } -> do
          { conditions: conditions <#> \{ condition: { mode, count, cards: cards' }, disabled } -> { mode, count, cards: fold $ traverse (elemIndex <@> ids) cards', disabled }
          , disabled: groupDisabled
          }
    let cards' = cards <#> \{ name, count } -> { name, count }
    { deck: { cards: cards', hand, others }, condition: set' }

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
