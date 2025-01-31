module App.Condition where

import Prelude

import App.ConditionLine as ConditionLine
import App.Result as Result
import Control.Alternative (guard)
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
import Record as Record
import TcgCalculator.Types (ConditionGroup, ConditionGroupExport, Deck, Id, generateId)
import Type.Proxy (Proxy(..))
import Util.Halogen as HU

----------------------------------------------------------------

data Output
  = Updated
  | AllConditionDeleted

data Action
  = Initialize
  | AddCondition
  | RemoveCondition Id
  | ToggleItemDisabled Id
  | Receive Deck
  | Calculate

data Query a
  = GetConditions (ConditionGroup -> a)
  | Export (ConditionGroupExport -> a)
  | RestoreState Deck ConditionGroupExport a
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

  initialState :: _ { conditions :: Array { id :: Id, disabled :: Boolean }, deck :: Deck, disabled :: Boolean }
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
          [ HH.slot (Proxy @"line") id ConditionLine.component cards (const Calculate) ]
      ]

  renderConditionAddButton =
    HH.div
      [ HP.class_ $ H.ClassName "px-1" ]
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
      when (deck /= current) do
        H.modify_ _ { deck = deck }
        calculate
    Calculate -> do
      calculate
      H.raise Updated

  getConditions = ado
    disabled <- map _.id <<< Array.filter _.disabled <$> H.gets _.conditions
    conditions <- H.requestAll (Proxy @"line") ConditionLine.GetCondition
    in NE.fromFoldable <<< Map.filterKeys (Array.notElem <@> disabled) $ conditions

  calculate = do
    deck <- H.gets _.deck
    conditions <- Array.fromFoldable <$> getConditions
    H.tell (Proxy @"result") unit (Result.Calculate deck conditions)

  query :: _ ~> _
  query = case _ of
    GetConditions reply -> ado
      guard <<< not =<< H.gets _.disabled
      conditions <- MaybeT getConditions
      in reply conditions
    Export reply -> MaybeT ado
      { conditions, disabled: parentDisabled } <- H.get
      lines <- H.requestAll (Proxy @"line") ConditionLine.GetCondition
      in reply <<< { conditions: _, disabled: parentDisabled } <$> do
        NE.fromArray conditions >>= traverse \{ id, disabled } -> { condition: _, disabled } <$> Map.lookup id lines
    RestoreState deck { conditions, disabled: parentDisabled } a -> H.lift do
      conditions' <- traverse (flap $ Record.insert (Proxy @"id") <$> generateId) $ NE.toArray conditions
      H.put { conditions: conditions' <#> \{id, disabled } -> { id, disabled }, deck, disabled: parentDisabled }
      for_ conditions' \{ id, condition } -> do
        H.tell (Proxy @"line") id (ConditionLine.RestoreState deck.cards condition)
      calculate
      pure a
    ToggleDisabled a -> do
      H.modify_ do
        disabled <- _.disabled
        _ { disabled = not disabled }
      pure a
