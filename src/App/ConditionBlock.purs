module App.ConditionBlock where

import Prelude

import App.Selector as Selector
import Control.Monad.Maybe.Trans (runMaybeT)
import Data.Array as Array
import Data.Foldable (traverse_)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (alaF)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TcgCalculator.Types (Cards, Condition(..), Condition', ConditionMode(..), Id, readConditionMode)
import Type.Proxy (Proxy(..))
import Web.Event.Event as Event
import Web.HTML.HTMLInputElement as Input
import Web.UIEvent.FocusEvent as Focus

----------------------------------------------------------------

data Output = Updated

data Action
  = Initialize
  | UpdateConditionMode String
  | UpdateCardSelected (Array Id)
  | UpdateCardCount String
  | Receive Cards
  | SelectOnFocus Focus.FocusEvent

data Query a
  = GetCondition (Condition -> a)
  | RestoreState Cards Condition a

----------------------------------------------------------------

component :: H.Component Query Cards Output Aff
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

  initialState :: _ -> { cards :: Cards, condition :: Condition', minValue :: Int, maxValue :: Int }
  initialState = { cards: _, condition: { mode: AtLeast, count: 0, cards: [] }, minValue: 0, maxValue: 0 }

  render { cards, condition: { mode, count }, minValue, maxValue } = do
    HH.div
      [ HP.class_ $ H.ClassName "flex flex-wrap items-center justify-end" ]
      [ HH.div
          [ HP.class_ $ H.ClassName "grow w-48 rounded border p-0.5" ]
          [ renderCardSelector cards ]
      , HH.div
          [ HP.class_ $ H.ClassName "flex items-center" ]
          [ HH.div [ HP.class_ $ H.ClassName "mx-1" ] [ HH.text "を" ]
          , renderCardCounter count minValue maxValue
          , renderModeSelector mode
          ]
      ]

  renderCardSelector cards = do
    let cards' = cards <#> \card -> { id: card.id, value: card.name }
    HH.slot (Proxy @"selector") unit Selector.component cards' UpdateCardSelected

  renderCardCounter count min max =
    HH.input
      [ HP.classes
          [ H.ClassName "appearance-none w-12 shrink-0 p-1 text-right rounded border transition ease-in-out"
          , H.ClassName "bg-transparent border-gray-400 hover:border-gray-500"
          , H.ClassName "focus:outline-none focus:border-blue-600"
          ]
      , HP.type_ HP.InputNumber
      , HP.step $ HP.Step 1.0
      , HP.value $ show count
      , HP.min $ Int.toNumber min
      , HP.max $ Int.toNumber max
      , HE.onFocus SelectOnFocus
      , HE.onValueChange UpdateCardCount
      ]

  renderModeSelector mode =
    HH.select
      [ HP.classes
          [ H.ClassName "appearance-none w-full py-1 px-3 rounded border"
          , H.ClassName "bg-transparent border-gray-400 hover:border-gray-500"
          , H.ClassName "focus:outline-none focus:border-blue-600"
          ]
      , HP.value $ show mode
      , HE.onValueChange UpdateConditionMode
      ]
      [ HH.option [ HP.value $ show AtLeast ] [ HH.text "枚以上ドローする" ]
      , HH.option [ HP.value $ show JustDraw ] [ HH.text "枚ちょうどドローする" ]
      , HH.option [ HP.value $ show Remains ] [ HH.text "枚以上デッキに残す" ]
      , HH.option [ HP.value $ show JustRemains ] [ HH.text "枚ちょうどデッキに残す" ]
      , HH.option [ HP.value $ show Choice ] [ HH.text "種類以上ドローする" ]
      , HH.option [ HP.value $ show LeftOne ] [ HH.text "種類以上デッキに残す" ]
      , HH.option [ HP.value $ show LeftAll ] [ HH.text "種類以上ドローしない" ]
      ]

  action :: Action -> _
  action = case _ of
    Initialize -> do
      { cards, condition: { mode, count, cards: selected } } <- H.get
      updateStatus cards selected mode count
    UpdateCardSelected selected -> do
      updateCardSelected selected
      H.raise Updated
    UpdateConditionMode mode -> do
      readConditionMode mode # traverse_ \mode' -> do
        { cards, condition: { count, cards: selected } } <- H.get
        updateStatus cards selected mode' count
        H.raise Updated
    UpdateCardCount count -> do
      Int.fromString count # traverse_ \count' -> do
        { cards, condition: { mode, cards: selected } } <- H.get
        updateStatus cards selected mode count'
        H.raise Updated
    Receive cards -> do
      { condition: { cards: selected } } <- H.modify _ { cards = cards }
      updateCardSelected (selected <#> _.id)
    SelectOnFocus event -> do
      let element = Input.fromEventTarget <=< Event.target <<< Focus.toEvent $ event
      H.liftEffect $ traverse_ Input.select element
    where
    updateCardSelected selected = do
      { cards, condition: { mode, count } } <- H.get
      let selected' = cards # Array.filter \{ id } -> Array.elem id selected
      updateStatus cards selected' mode count
    updateStatus cards selected mode count = do
      let { min, max } = getMinMax selected mode
      H.put { cards, condition: { mode: mode, cards: selected, count: clamp min max count }, minValue: min, maxValue: max }

  getMinMax :: Cards -> ConditionMode -> { min :: Int, max :: Int }
  getMinMax cards = case _ of
    AtLeast -> do
      let max = countCards cards
      { min: min 1 max, max }
    JustDraw -> do
      let max = countCards cards
      { min: 0, max }
    Remains -> do
      let max = countCards cards
      { min: min 1 max, max }
    JustRemains -> do
      let max = countCards cards
      { min: 0, max }
    Choice -> do
      let max = Array.length cards
      { min: min 1 max, max }
    LeftOne -> do
      let max = Array.length cards
      { min: min 1 max, max }
    LeftAll -> do
      let max = Array.length cards
      { min: min 1 max, max }
    where
    countCards = alaF Additive Array.foldMap _.count

  query :: _ ~> _
  query = case _ of
    GetCondition reply -> do
      reply <<< Condition <$> H.gets _.condition
    RestoreState cards (Condition condition) a -> do
      let { min, max } = getMinMax condition.cards condition.mode
      H.put { cards, condition, minValue: min, maxValue: max }
      let items = cards <#> \card -> { id: card.id, value: card.name, selected: Array.elem card condition.cards }
      H.lift $ H.tell (Proxy @"selector") unit (Selector.SetItems items)
      pure a
