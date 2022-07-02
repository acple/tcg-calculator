module App.ConditionBlock where

import Prelude

import App.Selector as Selector
import Control.Monad.Maybe.Trans (runMaybeT)
import Data.Array as Array
import Data.Foldable (sum)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TcgCalculator.Types (Cards, Condition(..), Condition', ConditionMode(..))
import Type.Proxy (Proxy(..))

data Updated = Updated

data Action
  = UpdateCards Cards
  | UpdateConditionMode String
  | UpdateCardSelect (Array String)
  | UpdateCardCount String
  | UpdateStatus

data Query a
  = SetCondition Condition a
  | GetCondition (Condition -> a)
  | RestoreState Cards Condition a

component :: H.Component Query Cards Updated Aff
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = action
      , handleQuery = runMaybeT <<< query
      , initialize = Just UpdateStatus
      , receive = Just <<< UpdateCards
      }
  }
  where

  initialState :: _ -> { cards :: Cards, condition :: Condition', minValue :: Int, maxValue :: Int }
  initialState = { cards: _, condition: { mode: AtLeast, count: 0, cards: [] }, minValue: 0, maxValue: 0 }

  render { cards, condition: { mode, count }, minValue, maxValue } = do
    HH.div
      [ HP.class_ $ H.ClassName "flex items-center gap-1" ]
      [ HH.div
          [ HP.class_ $ H.ClassName "grow w-0 rounded border p-0.5" ]
          [ renderCardSelector cards ]
      , HH.text "を"
      , HH.span
          [ HP.class_ $ H.ClassName "flex" ]
          [ renderCardCounter count minValue maxValue
          , renderModeSelector mode
          ]
      ]

  renderCardSelector cards = do
    let cards' = cards <#> \card -> { id: card.id, value: card.name }
    HH.slot (Proxy :: _ "selector") unit Selector.component cards' UpdateCardSelect

  renderCardCounter count min max =
    HH.input
      [ HP.classes
          [ H.ClassName "appearance-none w-16 py-1 pl-3 pr-1 m-0 rounded border transition ease-in-out"
          , H.ClassName "bg-transparent border-gray-400 hover:border-gray-500"
          , H.ClassName "focus:outline-none focus:border-blue-600"
          ]
      , HP.type_ HP.InputNumber
      , HP.step $ HP.Step 1.0
      , HP.value $ show count
      , HP.min $ Int.toNumber min
      , HP.max $ Int.toNumber max
      , HE.onValueChange UpdateCardCount
      ]

  renderModeSelector mode =
    HH.select
      [ HP.classes
          [ H.ClassName "appearance-none py-1 pl-4 pr-8 rounded border"
          , H.ClassName "bg-transparent border-gray-400 hover:border-gray-500"
          , H.ClassName "focus:outline-none focus:border-blue-600"
          ]
      , HP.value $ show mode
      , HE.onValueChange UpdateConditionMode
      ]
      [ HH.option [ HP.value "AtLeast" ] [ HH.text "枚以上ドローする" ]
      , HH.option [ HP.value "JustDraw" ] [ HH.text "枚ちょうどドローする" ]
      , HH.option [ HP.value "Remains" ] [ HH.text "枚以上デッキに残す" ]
      , HH.option [ HP.value "JustRemains" ] [ HH.text "枚ちょうどデッキに残す"]
      , HH.option [ HP.value "Choice" ] [ HH.text "種類以上ドローする" ]
      , HH.option [ HP.value "LeftOne" ] [ HH.text "種類以上デッキに残す" ]
      , HH.option [ HP.value "LeftAll" ] [ HH.text "種類以上ドローしない" ]
      ]

  readConditionMode :: String -> Maybe ConditionMode
  readConditionMode = case _ of
    "AtLeast" -> Just AtLeast
    "JustDraw" -> Just JustDraw
    "Remains" -> Just Remains
    "JustRemains" -> Just JustRemains
    "Choice" -> Just Choice
    "LeftOne" -> Just LeftOne
    "LeftAll" -> Just LeftAll
    _ -> Nothing

  action :: Action -> _
  action = case _ of
    UpdateCards cards -> do
      current <- H.gets _.cards
      when (cards /= current) do
        { condition: { cards: selected } } <- H.modify _ { cards = cards }
        action $ UpdateCardSelect (selected <#> _.id)
    UpdateCardSelect selected -> do
      { cards, condition: { mode, count } } <- H.get
      let selected' = cards # Array.filter \{ id } -> Array.elem id selected
      updateStatus cards selected' mode count
    UpdateConditionMode mode -> do
      case readConditionMode mode of
        Just mode' -> do
          { cards, condition: { count, cards: selected } } <- H.get
          updateStatus cards selected mode' count
        _ -> pure unit
    UpdateCardCount count -> do
      case Int.fromString count of
        Just count' -> do
          { cards, condition: { mode, cards: selected } } <- H.get
          updateStatus cards selected mode count'
        _ -> pure unit
    UpdateStatus -> do
      { cards, condition: { mode, count, cards: selected } } <- H.get
      updateStatus cards selected mode count
    where
    updateStatus cards selected mode count = do
      let { min, max } = getMinMax selected mode
      H.modify_ _ { cards = cards, condition { mode = mode, cards = selected, count = clamp min max count }, minValue = min, maxValue = max }
      H.raise Updated

  getMinMax :: Cards -> ConditionMode -> { min :: Int, max :: Int }
  getMinMax cards = case _ of
    AtLeast -> do
      let max = cards <#> _.count # sum
      { min: min 1 max, max }
    JustDraw -> do
      let max = cards <#> _.count # sum
      { min: 0, max }
    Remains -> do
      let max = cards <#> _.count # sum
      { min: min 1 max, max }
    JustRemains -> do
      let max = cards <#> _.count # sum
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

  query :: _ ~> _
  query = case _ of
    SetCondition (Condition condition) a -> do
      H.modify_ _ { condition = condition }
      H.lift $ action UpdateStatus
      pure a
    GetCondition reply -> do
      reply <<< Condition <$> H.gets _.condition
    RestoreState cards (Condition condition) a -> do
      { condition: { cards: selected } } <- H.modify _ { cards = cards , condition = condition }
      let items = cards <#> \card -> { id: card.id, value: card.name, selected: Array.elem card selected }
      H.lift $ H.tell (Proxy :: _ "selector") unit (Selector.SetItems items)
      pure a
