module App.ConditionLine where

import Prelude

import App.Selector as Selector
import Control.Monad.Maybe.Trans (runMaybeT)
import Data.Array as Array
import Data.Foldable (foldMap)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (alaF)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TcgCalculator.Types (CardId, Cards, Condition, ConditionMode(..), filterCards, readConditionMode)
import Type.Proxy (Proxy(..))
import Web.Event.Event as Event
import Web.HTML.HTMLInputElement as Input
import Web.UIEvent.FocusEvent as Focus

----------------------------------------------------------------

data Output = Updated

data Action
  = UpdateConditionMode String
  | UpdateCardSelected (Array CardId)
  | UpdateCardCount String
  | Receive Cards
  | SelectOnFocus Focus.FocusEvent

data Query a
  = GetCondition (Condition -> a)
  | UpdateState Condition a

----------------------------------------------------------------

component :: H.Component Query Cards Output Aff
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = action
      , handleQuery = runMaybeT <<< query
      , receive = Just <<< Receive
      }
  }
  where

  initialState :: _ { cards :: Cards, condition :: Condition, min :: Int, max :: Int }
  initialState = { cards: _, condition: { mode: AtLeast, count: 0, cards: [] }, min: 0, max: 0 }

  render { cards, condition: { mode, count }, min, max } =
    HH.div
      [ HP.class_ $ H.ClassName "flex flex-wrap items-center justify-end" ]
      [ HH.div
          [ HP.class_ $ H.ClassName "w-48 grow rounded border border-gray-200 p-0.5" ]
          [ renderCardSelector cards ]
      , HH.div
          [ HP.class_ $ H.ClassName "flex items-center" ]
          [ HH.div [ HP.class_ $ H.ClassName "mx-1" ] [ HH.text "を" ]
          , renderCardCounter count min max
          , renderModeSelector mode
          ]
      ]

  renderCardSelector cards = do
    let items = cards <#> \card -> { key: card.id, value: card.name }
    HH.slot (Proxy @"selector") unit Selector.component items UpdateCardSelected

  renderCardCounter count min max =
    HH.input
      [ HP.classes
          [ H.ClassName "w-12 appearance-none rounded border border-gray-400 bg-transparent p-1 text-right"
          , H.ClassName "transition ease-in-out hover:border-gray-500 focus:border-blue-600 focus:outline-none"
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
          [ H.ClassName "w-full appearance-none rounded border border-gray-400 bg-transparent px-3 py-1"
          , H.ClassName "transition ease-in-out hover:border-gray-500 focus:border-blue-600 focus:outline-none"
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

  action = case _ of
    UpdateCardSelected selected -> do
      let selected' = Array.sort selected
      { cards, condition: { mode, count, cards: current } } <- H.get
      when (selected' /= current) do
        updateStatus cards selected' mode count
        H.raise Updated
    UpdateConditionMode mode -> do
      readConditionMode mode # foldMap \mode' -> do
        { cards, condition: { count, cards: selected } } <- H.get
        updateStatus cards selected mode' count
        H.raise Updated
    UpdateCardCount count -> do
      Int.fromString count # foldMap \count' -> do
        { cards, condition: { mode, cards: selected } } <- H.get
        updateStatus cards selected mode count'
        H.raise Updated
    Receive cards -> do
      { mode, count, cards: selected } <- H.gets _.condition
      updateStatus cards selected mode count
    SelectOnFocus event -> do
      let element = Input.fromEventTarget <=< Event.target <<< Focus.toEvent $ event
      H.liftEffect $ foldMap Input.select element
    where
    updateStatus cards selected mode count = do
      let cards' = filterCards selected cards
      let { min, max } = getMinMax cards' mode
      H.put { cards, condition: { mode, count: clamp min max count, cards: Array.sort $ cards' <#> _.id }, min, max }

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
      reply <$> H.gets _.condition
    UpdateState condition@{ mode, cards: selected } a -> H.lift do
      cards <- H.gets _.cards
      let cards' = filterCards selected cards
      let { min, max } = getMinMax cards' mode
      H.put { cards, condition, min, max }
      let items = cards <#> \card -> { key: card.id, value: card.name, selected: Array.elem card cards' }
      H.tell (Proxy @"selector") unit (Selector.SetItems items)
      pure a
