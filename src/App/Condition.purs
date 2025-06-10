module App.Condition where

import Prelude

import App.ConditionLine as ConditionLine
import App.Result as Result
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Array as Array
import Data.Array.NonEmpty as NE
import Data.Foldable (for_)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TcgCalculator.Types (AppConditionGroup, ConditionId, Deck, generateId, toConditionGroup)
import Type.Proxy (Proxy(..))
import Util.Halogen as HU

----------------------------------------------------------------

data Output
  = Updated
  | AllConditionDeleted

data Action
  = Initialize
  | AddCondition
  | RemoveCondition ConditionId
  | ToggleItemDisabled ConditionId
  | Receive Deck
  | ReceiveConditionUpdated

type ConditionState = { conditions :: AppConditionGroup, disabled :: Boolean }

data Query a
  = GetState (ConditionState -> a)
  | UpdateState ConditionState a
  | ToggleDisabled a

----------------------------------------------------------------

component :: H.Component Query Deck Output Aff
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = action
      , handleQuery = runMaybeT <<< query
      , receive = Just <<< Receive
      , initialize = Just Initialize
      }
  }
  where

  initialState :: _ { conditions :: Array { id :: ConditionId, disabled :: Boolean }, deck :: Deck, disabled :: Boolean }
  initialState = { conditions: [], deck: _, disabled: false }

  render { conditions, deck, disabled } =
    HH.div
      [ HP.classes
          [ H.ClassName "grow rounded border-2 border-violet-300 p-1"
          , if disabled then H.ClassName "bg-slate-100 text-gray-400" else H.ClassName "bg-white text-gray-700"
          ]
      ]
      [ renderConditionHeader disabled
      , HH.ul
          [ HP.class_ $ H.ClassName "my-1" ]
          $ renderConditionLine deck.cards <$> conditions
      , renderConditionAddButton
      ]

  renderConditionHeader disabled =
    HH.div
      [ HP.class_ $ H.ClassName "flex items-center justify-end gap-1" ]
      [ HH.div
          [ HP.class_ if disabled then H.ClassName "mr-auto w-0 py-1 pl-3 text-xl" else H.ClassName "hidden" ]
          [ HH.text "Disabled" ]
      , HH.div
          [ HP.class_ if disabled then H.ClassName "text-gray-400" else H.ClassName "text-black" ]
          [ HH.slot_ (Proxy @"result") unit Result.component unit ]
      ]

  renderConditionLine cards { id, disabled } =
    HH.li
      [ HP.classes
          [ H.ClassName "flex gap-1 rounded px-1"
          , if disabled then H.ClassName "bg-slate-100 text-gray-400 line-through" else H.ClassName "bg-transparent"
          ]
      ]
      [ HH.div
          [ HP.class_ $ H.ClassName "mx-1 flex" ]
          [ HU.removeButton (RemoveCondition id)
          , HU.toggleButton (ToggleItemDisabled id)
          ]
      , HH.div
          [ HP.class_ $ H.ClassName "min-w-0 grow" ]
          [ HH.slot (Proxy @"line") id ConditionLine.component cards (const ReceiveConditionUpdated) ]
      ]

  renderConditionAddButton =
    HH.div
      [ HP.class_ $ H.ClassName "px-1" ]
      [ HU.plusButton AddCondition ]

  action = case _ of
    Initialize -> do
      id <- generateId
      H.modify_ _ { conditions = [{ id, disabled: false }] }
    AddCondition -> do
      conditions <- H.gets _.conditions
      id <- generateId
      H.modify_ _ { conditions = Array.snoc conditions { id, disabled: false } }
      H.raise Updated
    RemoveCondition id -> do
      { conditions } <- H.modify do
        conditions <- _.conditions
        _ { conditions = Array.filter (_.id >>> (_ /= id)) conditions }
      H.raise if Array.null conditions then AllConditionDeleted else Updated
    ToggleItemDisabled id -> do
      H.modify_ do
        conditions <- _.conditions
        fromMaybe identity do
          i <- Array.findIndex (_.id >>> (_ == id)) conditions
          conditions' <- Array.modifyAt i toggleDisabled conditions
          pure _ { conditions = conditions' }
      H.raise Updated
    Receive deck -> do
      H.modify_ _ { deck = deck }
    ReceiveConditionUpdated -> do
      H.raise Updated

  toggleDisabled :: forall r. { disabled :: Boolean | r } -> { disabled :: Boolean | r }
  toggleDisabled = _ { disabled = _ } <*> not _.disabled

  query :: _ ~> _
  query = case _ of
    GetState reply -> MaybeT ado
      { conditions, disabled: groupDisabled } <- H.get
      lines <- H.requestAll (Proxy @"line") ConditionLine.GetCondition
      in reply <<< { conditions: _, disabled: groupDisabled } <$> do
        NE.fromArray conditions >>= traverse \{ id, disabled } -> { id, condition: _, disabled } <$> Map.lookup id lines
    UpdateState { conditions,  disabled: groupDisabled } a -> H.lift do
      { deck } <- H.modify _ { conditions = NE.toArray conditions <#> \{ id, disabled } -> { id, disabled }, disabled = groupDisabled }
      for_ conditions \{ id, condition } -> do
        H.tell (Proxy @"line") id (ConditionLine.UpdateState condition)
      let set = Array.fromFoldable <<< toConditionGroup $ conditions
      H.tell (Proxy @"result") unit (Result.Calculate deck set)
      pure a
    ToggleDisabled a -> do
      H.modify_ toggleDisabled
      pure a
