module App.App where

import Prelude

import App.Condition as Condition
import App.Deck as Deck
import App.Result as Result
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (decodeJson, parseJson, printJsonDecodeError)
import Data.Argonaut.Encode (encodeJson)
import Data.Array as Array
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import JSURI (decodeURIComponent)
import Routing.Hash as Hash
import TcgCalculator.Types (Deck, Id, generateId)
import Type.Proxy (Proxy(..))
import Util.Array as ArrayUtil
import Util.Halogen as HU

----------------------------------------------------------------

type Export = { deck :: Deck, conditions :: Array Condition.Export }

type Index = Int

data Action
  = Initialize
  | PrepareDefaultState
  | UpdateDeck Deck
  | AddCondition
  | RemoveCondition Id
  | ToggleDisabled Id
  | Swap Index Index
  | ReceiveConditionUpdated Id Condition.Output
  | Calculate
  | RestoreState String
  | SaveState

----------------------------------------------------------------

component :: H.Component (Const Void) Unit Void Aff
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval { handleAction = action, initialize = Just Initialize }
  }
  where

  initialState :: _ { deck :: Deck, conditions :: Array Id }
  initialState _ = { deck: { cards: [], others: 0, hand: 0 }, conditions: [] }

  render { deck, conditions } =
    HH.div
      [ HP.class_ $ H.ClassName "relative overflow-x-clip" ]
      [ HH.header
          [ HP.class_ $ H.ClassName "flex items-baseline px-2" ]
          [ HH.h1
              [ HP.class_ $ H.ClassName "p-1 text-lg" ]
              [ HH.a [ HP.href "." ] [ HH.text "Draw Calculator" ] ]
          , HH.text "-"
          , HH.h2
              [ HP.class_ $ H.ClassName "p-1" ]
              [ HH.text "TCG 用ドロー確率計算機" ]
          ]
      , HH.main
          [ HP.class_ $ H.ClassName "relative mb-[80vh] flex max-w-4xl flex-col gap-3 p-1" ]
          [ HH.div
              [ HP.class_ $ H.ClassName "flex flex-wrap gap-1" ]
              [ renderDeck
              , renderResult
              ]
          , HH.ul
              [ HP.class_ $ H.ClassName "flex flex-col gap-1" ]
              $ Array.mapWithIndex (renderCondition deck) conditions
          , renderConditionAddButton
          ]
      , HH.footer
          [ HP.class_ $ H.ClassName "flex items-baseline px-2" ]
          [ HH.a
              [ HP.class_ $ H.ClassName "m-1 rounded border border-sky-500 px-2 py-1 text-sm"
              , HP.href "https://github.com/acple/tcg-calculator", HP.target "_blank", HP.rel "noopener noreferrer"
              ]
              [ HH.text "→ 使い方とかソース" ]
          ]
      ]

  renderDeck =
    HH.div
      [ HP.class_ $ H.ClassName "grow" ]
      [ HH.slot (Proxy @"deck") unit Deck.component unit UpdateDeck ]

  renderResult =
    HH.div
      [ HP.class_ $ H.ClassName "flex grow basis-0 items-center rounded border-2 border-cyan-400 p-1" ]
      [ HH.div
          [ HP.class_ $ H.ClassName "flex w-full justify-end gap-1" ]
          [ HH.div_ [ HU.button (HH.text "Save") (H.ClassName "border border-rose-500 hover:bg-rose-100") SaveState ]
          , HH.slot_ (Proxy @"result") unit Result.component unit
          ]
      ]

  renderCondition deck i id =
    HH.li
      [ HP.class_ $ H.ClassName "flex items-start gap-1" ]
      [ HH.div
          [ HP.class_ $ H.ClassName "flex flex-col" ]
          [ HU.upButton (Swap (i - 1) i)
          , HU.removeButton (RemoveCondition id)
          , HU.toggleButton (ToggleDisabled id)
          , HU.downButton (Swap i (i + 1))
          ]
      , HH.slot (Proxy @"condition") id Condition.component deck (ReceiveConditionUpdated id)
      ]

  renderConditionAddButton =
    HH.div_
      [ HU.plusButton AddCondition ]

  action = case _ of
    Initialize -> do
      { emitter, listener } <- H.liftEffect HS.create
      _ <- H.subscribe emitter
      void <<< H.liftEffect $ Hash.matchesWith decodeURIComponent \_ hash -> do
        HS.notify listener if String.null hash
          then PrepareDefaultState
          else RestoreState hash
    PrepareDefaultState -> do
      { deck, conditions } <- H.get
      when (Array.null deck.cards && Array.null conditions) do
        id <- generateId
        let defaultDeck = { cards: [{ id, name: "Card1", count: 3 }], others: 37, hand: 5 }
        H.modify_ _ { deck = defaultDeck }
        H.tell (Proxy @"deck") unit (Deck.SetDeck defaultDeck)
        action AddCondition
    UpdateDeck deck -> do
      current <- H.gets _.deck
      when (deck /= current) do
        H.modify_ _ { deck = deck }
        action Calculate
    AddCondition -> do
      id <- generateId
      H.modify_ do
        conditions <- _.conditions
        _ { conditions = Array.snoc conditions id }
      action Calculate
    RemoveCondition id -> do
      H.modify_ do
        conditions <- _.conditions
        _ { conditions = Array.filter (_ /= id) conditions }
      action Calculate
    ToggleDisabled id -> do
      H.tell (Proxy @"condition") id Condition.ToggleDisabled
      action Calculate
    Swap x y -> do
      H.modify_ do
        conditions <- _.conditions
        _ { conditions = ArrayUtil.swap x y conditions }
    ReceiveConditionUpdated _ Condition.Updated -> do
      action Calculate
    ReceiveConditionUpdated id Condition.AllConditionDeleted -> do
      action $ RemoveCondition id
    Calculate -> do
      deck <- H.gets _.deck
      conditions <- H.requestAll (Proxy @"condition") Condition.GetConditions
      let conditions' = Array.fromFoldable conditions
      H.tell (Proxy @"result") unit (Result.Calculate deck conditions')
    RestoreState json -> do
      case parseJson json >>= decodeJson @Export of
        Left error -> do
          Console.error $ printJsonDecodeError error
          action PrepareDefaultState
        Right { deck, conditions } -> do
          conditions' <- traverse (flap $ { id: _, condition: _ } <$> generateId) conditions
          H.put { deck, conditions: conditions' <#> _.id }
          H.tell (Proxy @"deck") unit (Deck.SetDeck deck)
          for_ conditions' \{ id, condition } -> do
            H.tell (Proxy @"condition") id (Condition.RestoreState deck condition)
          action Calculate
    SaveState -> do
      { deck, conditions: ids } <- H.get
      conditions <- H.requestAll (Proxy @"condition") Condition.GetState
      let conditions' = Array.mapMaybe (Map.lookup <@> conditions) ids
      let json = encodeJson @Export { deck, conditions: conditions' }
      H.liftEffect $ Hash.setHash (stringify json)
