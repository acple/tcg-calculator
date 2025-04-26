module App.App where

import Prelude

import App.Condition as Condition
import App.Deck as Deck
import App.Result as Result
import Codec.JSON.DecodeError as DecodeError
import Control.Monad.Except (except, runExceptT)
import Data.Array as Array
import Data.Array.NonEmpty as NE
import Data.Bifunctor (lmap)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Foldable (fold, for_)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Foreign as Foreign
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import JSON as JSON
import JSURI (decodeURIComponent)
import Record as Record
import Routing.PushState as PushState
import TcgCalculator.Codec as Codec
import TcgCalculator.Types (AppState, ConditionMode(..), Deck, Id, generateId, toConditionSet)
import Type.Proxy (Proxy(..))
import Util.Array as ArrayUtil
import Util.Halogen as HU

----------------------------------------------------------------

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
  | RestoreState String
  | ApplyState AppState
  | SaveState

----------------------------------------------------------------

component :: H.Component (Const Void) Unit Void Aff
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval { handleAction = action, initialize = Just Initialize }
  }
  where

  initialState :: _ { deck :: Deck, conditions :: Array Id, pushState :: AppState -> Effect Unit, replaceState :: AppState -> Effect Unit }
  initialState _ = { deck: { cards: [], others: 0, hand: 0 }, conditions: [], pushState: mempty, replaceState: mempty }

  render { deck, conditions } = do
    let deck' = deck { cards = Array.filter (_.name >>> not String.null) deck.cards }
    HH.div
      [ HP.class_ $ H.ClassName "relative overflow-x-clip" ]
      [ HH.header
          [ HP.class_ $ H.ClassName "flex items-baseline px-2" ]
          [ HH.h1
              [ HP.class_ $ H.ClassName "p-1 text-lg" ]
              [ HH.a [ HP.href "#" ] [ HH.text "Draw Calculator" ] ]
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
              $ Array.mapWithIndex (renderCondition deck') conditions
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
      [ HP.class_ $ H.ClassName "flex grow basis-0 items-center justify-end rounded border-2 border-cyan-400 p-1" ]
      [ HH.slot_ (Proxy @"result") unit Result.component unit ]

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
      psi <- H.liftEffect PushState.makeInterface
      H.modify_ _ { pushState = update psi.pushState, replaceState = update psi.replaceState }
      _ <- H.liftEffect $ psi.listen (HS.notify listener <<< handle)
      action <<< handle =<< H.liftEffect psi.locationState
      where
      update f export = do
        let json = Codec.encode Codec.appState export
        f (Foreign.unsafeToForeign export) ("#" <> JSON.print json)
      handle { state, hash }
        | not Foreign.isNull state = ApplyState (Foreign.unsafeFromForeign state) -- this works only with purescript-backend-optimizer
        | not String.null hash = RestoreState hash
        | otherwise = PrepareDefaultState
    PrepareDefaultState -> do
      cardId <- generateId
      conditionId <- generateId
      groupId <- generateId
      let card1 = { id: cardId, name: "Card1", count: 3 }
      let defaultDeck = { cards: [card1], others: 37, hand: 5 }
      let defaultCondition = { mode: AtLeast, count: 1, cards: [cardId] }
      let defaultConditionGroup = NE.singleton { id: conditionId, condition: defaultCondition, disabled: false }
      let defaultConditionSet = [{ id: groupId, conditions: defaultConditionGroup, disabled: false }]
      replaceState <- H.gets _.replaceState
      H.liftEffect $ replaceState { deck: defaultDeck, condition: defaultConditionSet }
    UpdateDeck deck -> do
      H.modify_ _ { deck = deck }
      action SaveState
    AddCondition -> do
      id <- generateId
      H.modify_ do
        conditions <- _.conditions
        _ { conditions = Array.snoc conditions id }
      action SaveState
    RemoveCondition id -> do
      H.modify_ do
        conditions <- _.conditions
        _ { conditions = Array.delete id conditions }
      action SaveState
    ToggleDisabled id -> do
      H.tell (Proxy @"condition") id Condition.ToggleDisabled
      action SaveState
    Swap x y -> do
      H.modify_ do
        conditions <- _.conditions
        _ { conditions = ArrayUtil.swap x y conditions }
      action SaveState
    ReceiveConditionUpdated _ Condition.Updated -> do
      action SaveState
    ReceiveConditionUpdated id Condition.AllConditionDeleted -> do
      action $ RemoveCondition id
    RestoreState hash -> do
      let json = fold $ String.stripPrefix (String.Pattern "#") =<< decodeURIComponent hash
      result <- H.liftEffect <<< runExceptT <<< parse $ json
      case result of
        Left error -> do
          Console.error $ "Failed to decode JSON: " <> DecodeError.print error
          action PrepareDefaultState
        Right state -> do
          replaceState <- H.gets _.replaceState
          H.liftEffect $ replaceState state
      where
      parse = Codec.decode Codec.appState <=< except <<< lmap DecodeError.basic <<< JSON.parse
    ApplyState { deck, condition: set } -> do
      let ids = set <#> _.id
      { deck: currentDeck, conditions: currentConditions } <- H.get
      when (deck /= currentDeck || ids /= currentConditions) do
        H.modify_ _ { deck = deck, conditions = ids }
        H.tell (Proxy @"deck") unit (Deck.SetDeck deck)
      for_ set \{ id, conditions, disabled } -> do
        H.tell (Proxy @"condition") id (Condition.UpdateState conditions disabled)
      H.tell (Proxy @"result") unit (Result.Calculate deck (toConditionSet set))
    SaveState -> do
      { deck, conditions: ids, pushState } <- H.get
      conditions <- H.requestAll (Proxy @"condition") Condition.GetState
      let condition = fold $ ids # traverse \id -> Record.insert (Proxy @"id") id <$> Map.lookup id conditions
      H.liftEffect $ pushState { deck, condition }
