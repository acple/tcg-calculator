module TcgCalculator.Codec
  ( appState
  , hashAppState
  , module Export
  )
  where

import Prelude

import Codec.JSON.DecodeError (DecodeError)
import Codec.JSON.DecodeError as DecodeError
import Control.Monad.Except (ExceptT, except, lift, runExcept)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Array (elemIndex, (!!))
import Data.Codec (Codec', codec', hoist, (<~<))
import Data.Codec (encode, decode) as Export
import Data.Codec.JSON as CJ
import Data.Codec.JSON.Common as CJC
import Data.Codec.JSON.Record as CJR
import Data.Codec.JSON.Sum as CJS
import Data.Either (note)
import Data.Set as Set
import Data.Traversable (traverse)
import Effect (Effect)
import JSON (JSON)
import TcgCalculator.Types (AppCondition, AppConditionGroup, AppConditionSet, AppState, Card, CardId, Condition, ConditionMode, Deck, HashCard, HashCondition, HashConditionGroup, HashConditionSet, HashDeck, HashState, Id, generateId, readConditionMode)
import TcgCalculator.Types.Id as Id

----------------------------------------------------------------

hashAppState  :: Codec' (ExceptT DecodeError Effect) JSON AppState
hashAppState = hashAppState' <~< hoist (except <<< runExcept) hashState

hashAppState' :: Codec' (ExceptT DecodeError Effect) HashState AppState
hashAppState' = codec' decode encode
  where

  decode :: HashState -> ExceptT DecodeError Effect AppState
  decode { deck: { cards, hand, others }, condition: set } = do
    cards' <- traverse decodeCard cards
    condition <- runReaderT (decodeConditionSet set) (cards' <#> _.id)
    pure { deck: { cards: cards', hand, others }, condition }

  decodeCard :: HashCard -> _ Card
  decodeCard { name, count } = { id: _, name, count } <$> generateId

  decodeConditionSet :: HashConditionSet -> _ AppConditionSet
  decodeConditionSet = traverse \{ conditions, disabled } ->
    { id: _, conditions: _, disabled } <$> generateId <*> decodeConditionGroup conditions

  decodeConditionGroup :: HashConditionGroup -> _ AppConditionGroup
  decodeConditionGroup = traverse decodeCondition

  decodeCondition :: HashCondition -> ReaderT (Array CardId) _ AppCondition
  decodeCondition { mode, count, cards, disabled } = do
    cardIds <- ask
    conditionId <- generateId
    cards' <- lift <<< except <<< note (DecodeError.basic "Could not decode conditions") <<< traverse (cardIds !! _) $ cards
    pure { id: conditionId, condition: { mode, count, cards: Set.fromFoldable cards' }, disabled }

  encode :: AppState -> HashState
  encode { deck: { cards, hand, others }, condition: set } = do
    let cards' = encodeCard <$> cards
    let condition = encodeConditionSet set (cards <#> _.id)
    { deck: { cards: cards', hand, others }, condition }

  encodeCard :: Card -> HashCard
  encodeCard { name, count } = { name, count }

  encodeConditionSet :: AppConditionSet -> _ HashConditionSet
  encodeConditionSet = traverse \{ conditions, disabled } -> { conditions: _, disabled } <$> encodeConditionGroup conditions

  encodeConditionGroup :: AppConditionGroup -> _ HashConditionGroup
  encodeConditionGroup = traverse encodeCondition

  encodeCondition :: AppCondition -> Array CardId -> HashCondition
  encodeCondition { condition: { mode, count, cards }, disabled } ids = do
    let cards' = Set.toUnfoldable <<< Set.mapMaybe (elemIndex <@> ids) $ cards
    { mode, count, cards: cards', disabled }

----------------------------------------------------------------

hashState :: CJ.Codec HashState
hashState = CJR.object { deck: hashDeck, condition: hashConditionSet }

hashConditionSet :: CJ.Codec HashConditionSet
hashConditionSet = CJ.array $ CJR.object { conditions: hashConditionGroup, disabled: CJ.boolean }

hashConditionGroup :: CJ.Codec HashConditionGroup
hashConditionGroup = CJC.nonEmptyArray hashCondition

hashCondition :: CJ.Codec HashCondition
hashCondition = CJR.object { mode: conditionMode, count: CJ.int, cards: CJ.array CJ.int, disabled: CJ.boolean }

hashDeck :: CJ.Codec HashDeck
hashDeck = CJR.object { cards: CJ.array hashCard, others: CJ.int, hand: CJ.int }

hashCard :: CJ.Codec HashCard
hashCard = CJR.object { name: CJ.string, count: CJ.int }

----------------------------------------------------------------

appState :: CJ.Codec AppState
appState = CJR.object { deck: appDeck, condition: appConditionSet }

appConditionSet :: CJ.Codec AppConditionSet
appConditionSet = CJ.array $ CJR.object { id, conditions: appConditionGroup, disabled: CJ.boolean }

appConditionGroup :: CJ.Codec AppConditionGroup
appConditionGroup = CJC.nonEmptyArray $ CJR.object { id, condition: appCondition, disabled: CJ.boolean }

appCondition :: CJ.Codec Condition
appCondition = CJR.object { mode: conditionMode, count: CJ.int, cards: CJC.set id }

appDeck :: CJ.Codec Deck
appDeck = CJR.object { cards: CJ.array appCard, others: CJ.int, hand: CJ.int }

appCard :: CJ.Codec Card
appCard = CJR.object { id, name: CJ.string, count: CJ.int }

----------------------------------------------------------------

conditionMode :: CJ.Codec ConditionMode
conditionMode = CJS.enumSum show readConditionMode

id :: forall a. CJ.Codec (Id a)
id = CJC.prismaticCodec "Id" Id.fromString Id.toString CJ.string
