module App.App where

import Prelude

import App.Condition as Condition
import App.Deck as Deck
import App.Result as Result
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (decodeJson, parseJson, printJsonDecodeError)
import Data.Argonaut.Encode (encodeJson)
import Data.Array as Array
import Data.Array.ST as STA
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Foldable (fold, for_)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console as Console
import Effect.Random as Random
import Halogen as C
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Halogen.Util as HU
import JSURI (decodeFormURLComponent)
import Routing.Hash as Hash
import TcgCalculator.Types (Deck)
import Type.Proxy (Proxy(..))

type Export = { deck :: Deck, conditions :: Array Condition.Export }

type Id = String -- TODO: use UUID

type Query :: Type -> Type
type Query = Const Void

data Action
  = Initialize
  | PrepareDefaultState
  | UpdateDeck Deck
  | AddCondition
  | RemoveCondition Id
  | ToggleDisabled Id
  | Swap Int Int
  | ReceiveConditionUpdated Id Condition.Updated
  | Calculate
  | RestoreState String
  | SaveState

component :: H.Component Query Unit Void Aff
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval { handleAction = action, initialize = Just Initialize }
  }
  where

  initialState :: _ -> { deck :: Deck , conditions :: Array Id }
  initialState _ = { deck: { cards: [], others: 0, hand: 0 }, conditions: [] }

  render { deck, conditions } =
    HH.div
      [ HP.class_ $ H.ClassName "relative overflow-x-clip mb-[80vh]" ]
      [ HH.header_
          [ HH.h1
              [ HP.class_ $ H.ClassName "px-2 py-1" ]
              [ HH.a [ HP.href "." ] [ HH.text "Draw Calculator" ] ]
          ]
      , HH.div
          [ HP.class_ $ C.ClassName "flex flex-col gap-3 container max-w-4xl p-1" ]
          [ HH.div
              [ HP.class_ $ C.ClassName "flex flex-wrap gap-1" ]
              [ renderDeck
              , renderResult
              ]
          , HH.ul
              [ HP.class_ $ H.ClassName "flex flex-col gap-1" ]
              $ Array.mapWithIndex (renderCondition deck) conditions
          , renderConditionAddButton
          ]
      ]

  renderDeck =
    HH.div
      [ HP.class_ $ H.ClassName "grow" ]
      [ HH.slot (Proxy :: _ "deck") unit Deck.component unit UpdateDeck ]

  renderResult =
    HH.div
      [ HP.class_ $ H.ClassName "grow basis-0 flex items-center rounded border-2 border-cyan-400 p-1" ]
      [ HH.div
          [ HP.class_ $ H.ClassName "h-full mr-auto" ]
          [ HU.button (HH.text "Save") (H.ClassName "border border-rose-500 hover:bg-rose-100") SaveState ]
      , HH.slot_ (Proxy :: _ "result") unit Result.component unit
      ]

  renderCondition deck i id =
    HH.li
      [ HP.class_ $ H.ClassName "flex gap-1 items-start" ]
      [ HH.div
          [ HP.class_ $ H.ClassName "flex flex-col" ]
          [ HU.upButton (Swap (i - 1) i)
          , HU.removeButton (RemoveCondition id)
          , HU.toggleButton (ToggleDisabled id)
          , HU.downButton (Swap i (i + 1))
          ]
      , HH.slot (Proxy :: _ "condition") id Condition.component deck (ReceiveConditionUpdated id)
      ]

  renderConditionAddButton =
    HH.div_
      [ HU.plusButton AddCondition ]

  generateId :: forall m. MonadEffect m => m Id
  generateId = H.liftEffect $ show <$> Random.random

  action = case _ of
    Initialize -> do
      { emitter, listener } <- H.liftEffect HS.create
      _ <- H.subscribe emitter
      void <<< H.liftEffect $ Hash.hashes \_ hash -> do
        let hash' = fold $ decodeFormURLComponent hash
        HS.notify listener if String.null hash'
          then PrepareDefaultState
          else RestoreState hash'
    PrepareDefaultState -> do
      { deck, conditions } <- H.get
      when (Array.null deck.cards && Array.null conditions) do
        id <- generateId
        let defaultDeck = { cards: [{ id, name: "Card1", count: 3 }], others: 37, hand: 5 }
        H.tell (Proxy :: _ "deck") unit (Deck.SetDeck defaultDeck)
        action AddCondition
    UpdateDeck deck -> do
      H.modify_ _ { deck = deck }
    AddCondition -> do
      id <- generateId
      H.modify_ do
        conditions <- _.conditions
        _ { conditions = Array.snoc conditions id }
    RemoveCondition id -> do
      H.modify_ do
        conditions <- _.conditions
        _ { conditions = Array.filter (_ /= id) conditions }
      action Calculate
    ToggleDisabled id -> do
      H.tell (Proxy :: _ "condition") id Condition.ToggleDisabled
      action Calculate
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
    ReceiveConditionUpdated id response -> case response of
      Condition.Updated ->
        action Calculate
      Condition.AllConditionDeleted ->
        action (RemoveCondition id)
    Calculate -> do
      deck <- H.gets _.deck
      conditions <- H.requestAll (Proxy :: _ "condition") Condition.GetConditions
      let conditions' = Array.fromFoldable <<< Map.values $ conditions
      H.tell (Proxy :: _ "result") unit (Result.Calculate deck conditions')
    RestoreState json -> do
      let state = decodeJson =<< parseJson json :: _ Export
      case state of
        Left error -> do
          Console.error $ printJsonDecodeError error
          action PrepareDefaultState
        Right { deck, conditions } -> do
          conditions' <- traverse (flap $ { id: _, condition: _ } <$> generateId) conditions
          H.modify_ _ { deck = deck, conditions = _.id <$> conditions' }
          H.tell (Proxy :: _ "deck") unit (Deck.SetDeck deck)
          for_ conditions' \{ id, condition } -> do
            H.tell (Proxy :: _ "condition") id (Condition.RestoreState deck condition)
    SaveState -> do
      { deck, conditions: ids } <- H.get
      conditions <- H.requestAll (Proxy :: _ "condition") Condition.GetState
      let conditions' = Array.mapMaybe (Map.lookup <@> conditions) ids
      let json = encodeJson { deck, conditions: conditions' }
      H.liftEffect $ Hash.setHash (stringify json)
      pure unit
