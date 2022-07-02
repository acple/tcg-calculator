module App.Condition where

import Prelude

import App.ConditionBlock as ConditionBlock
import App.Result as Result
import Control.Alternative (guard)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Plus (empty)
import Data.Array as Array
import Data.Array.NonEmpty as NE
import Data.Array.NonEmpty.Internal (NonEmptyArray)
import Data.Array.ST as STA
import Data.Foldable (for_)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Traversable (for, traverse)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect)
import Effect.Random as Random
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Util as HU
import Record as Record
import TcgCalculator.Types (Condition(..), ConditionMode, Deck)
import Type.Proxy (Proxy(..))

----------------------------------------------------------------

type Id = String

type Export =
  { conditions :: Array { mode :: ConditionMode, count :: Int, cards :: Array Id, disabled :: Boolean }
  , disabled :: Boolean
  }

type Index = Int

data Updated
  = Updated
  | AllConditionDeleted

data Action
  = Initialize
  | AddCondition
  | RemoveCondition Id
  | Swap Index Index
  | ToggleItemDisabled Id
  | Receive Deck
  | Calculate

data Query a
  = GetConditions (NonEmptyArray Condition -> a)
  | GetState (Export -> a)
  | RestoreState Deck Export a
  | ToggleDisabled a

----------------------------------------------------------------

component :: H.Component Query Deck Updated Aff
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = action
      , handleQuery = runMaybeT <<< query
      , initialize = Just Initialize
      , receive = Just <<< Receive
      }
  }
  where

  initialState :: _ -> { conditions :: Array { id :: Id, disabled :: Boolean } , deck :: Deck , disabled :: Boolean }
  initialState = { conditions: [], deck: _, disabled: false }

  render { conditions, deck, disabled } =
    HH.div
      [ HP.classes
          [ H.ClassName "w-full p-1 rounded border-2 border-violet-300"
          , if disabled then H.ClassName "bg-slate-100 text-gray-400" else H.ClassName "bg-white text-gray-700"
          ]
      ]
      [ renderConditionHeader disabled
      , HH.div
          [ HP.class_ $ H.ClassName "my-1" ]
          $ renderConditionBlock deck.cards <$> conditions
      , renderConditionAddButton
      ]

  renderConditionHeader disabled = do
    HH.div
      [ HP.class_ $ H.ClassName "flex items-center justify-end gap-1" ]
      [ HH.div
          [ HP.class_ if disabled then H.ClassName "py-1 px-3 text-xl mr-auto" else H.ClassName "hidden" ]
          [ HH.text "Disabled" ]
      , HH.div
          [ HP.class_ if disabled then H.ClassName "text-gray-400" else H.ClassName "text-black" ]
          [ HH.slot_ (Proxy :: _ "result") unit Result.component unit ]
      ]

  renderConditionBlock cards { id, disabled } =
    HH.div
      [ HP.class_ $ H.ClassName "rounded"
      ]
      [ HH.div
          [ HP.classes
              [ H.ClassName "flex gap-1 px-1 rounded"
              , if disabled then H.ClassName "bg-slate-100 text-gray-400 line-through" else H.ClassName "bg-transparent"
              ]
          ]
          [ HH.div
              [ HP.class_ $ H.ClassName "flex mx-1" ]
              [ HU.removeButton (RemoveCondition id)
              , HU.toggleButton (ToggleItemDisabled id)
              ]
          , HH.div
              [ HP.class_ $ H.ClassName "grow min-w-0" ]
              [ HH.slot (Proxy :: _ "block") id ConditionBlock.component cards (const Calculate) ]
          ]
      ]

  renderConditionAddButton =
    HH.div
      [ HP.class_ $ H.ClassName "mx-1" ]
      [ HU.plusButton AddCondition ]

  action = case _ of
    Initialize -> do
      action AddCondition
      calculate
    AddCondition -> do
      conditions <- H.gets _.conditions
      id <- generateId
      H.modify_ _ { conditions = Array.snoc conditions { id, disabled: false } }
    RemoveCondition id -> do
      { conditions } <- H.modify do
        conditions <- _.conditions
        _ { conditions = Array.filter (_.id >>> (_ /= id)) conditions }
      if Array.null conditions
        then H.raise AllConditionDeleted
        else action Calculate
    Swap x y -> do
      conditions <- H.gets _.conditions
      let conditions' = STA.run do
            st <- STA.thaw conditions
            a <- STA.peek x st
            b <- STA.peek y st
            case a, b of
              Just a', Just b' -> STA.poke x b' st *> STA.poke y a' st $> st
              _, _ -> pure st
      H.modify_ _ { conditions = conditions' }
    ToggleItemDisabled id -> do
      H.modify_ do
        conditions <- _.conditions
        fromMaybe identity do
          i <- Array.findIndex (_.id >>> (_ == id)) conditions
          conditions' <- Array.modifyAt i (\s -> s { disabled = not s.disabled }) conditions
          pure _ { conditions = conditions' }
      action Calculate
    Receive deck -> do
      current <- H.gets _.deck
      when (current /= deck) do
        H.modify_ _ { deck = deck }
        calculate
    Calculate -> do
      calculate
      H.raise Updated

  getConditions = ado
    disabled <- map _.id <<< Array.filter _.disabled <$> H.gets _.conditions
    conditions <- H.requestAll (Proxy :: _ "block") ConditionBlock.GetCondition
    in NE.fromFoldable <<< Map.values <<< Map.filterKeys (Array.notElem <@> disabled) $ conditions

  calculate = do
    deck <- H.gets _.deck
    conditions <- Array.fromFoldable <$> getConditions
    H.tell (Proxy :: _ "result") unit (Result.Calculate deck conditions)

  generateId :: forall m. MonadEffect m => m Id
  generateId = H.liftEffect $ show <$> Random.random -- TODO: use UUID

  query :: _ ~> _
  query = case _ of
    GetConditions reply -> ado
      guard <<< not =<< H.gets _.disabled
      conditions <- MaybeT getConditions
      in reply conditions
    GetState reply -> do
      { conditions: cond, disabled: parentDisabled } <- H.get
      conditions <- H.lift $ H.requestAll (Proxy :: _ "block") ConditionBlock.GetCondition
      maybe empty pure ado
        conditions' <- for cond \{ id, disabled } -> do
          Map.lookup id conditions <#> \(Condition { mode, count, cards }) -> { mode, count, cards: cards <#> _.id, disabled }
        in reply { conditions: conditions', disabled: parentDisabled }
    RestoreState deck { conditions, disabled: parentDisabled } a -> do
      conditions' <- traverse (flap $ Record.insert (Proxy :: _ "id") <$> generateId) conditions
      H.modify_ _ { conditions = conditions' <#> \{ id, disabled } -> { id, disabled }, deck = deck, disabled = parentDisabled }
      for_ conditions' \{ id, mode, count, cards } -> do
        let cards' = Array.mapMaybe <@> cards $ \cardId -> Array.find (_.id >>> (_ == cardId)) deck.cards
        H.lift $ H.tell (Proxy :: _ "block") id (ConditionBlock.RestoreState deck.cards (Condition { mode, count, cards: cards' }))
      H.lift calculate
      pure a
    ToggleDisabled a -> do
      H.modify_ do
        disabled <- _.disabled
        _ { disabled = not disabled }
      pure a
