module App.Deck where

import Prelude

import Control.Alternative (guard)
import Data.Array ((!!))
import Data.Array as Array
import Data.Const (Const)
import Data.Foldable (foldMap)
import Data.Function (on)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.MediaType (MediaType(..))
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (alaF, collect)
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as HK
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TcgCalculator.Types (Card, CardId, Deck, generateId)
import TcgCalculator.Types.Id as Id
import Util.Array as ArrayUtil
import Util.Halogen as HU
import Web.Event.Event as Event
import Web.HTML.Event.DataTransfer as DataTransfer
import Web.HTML.Event.DragEvent as Drag
import Web.HTML.HTMLInputElement as Input
import Web.UIEvent.FocusEvent as Focus

----------------------------------------------------------------

data Action
  = Receive Deck
  | AddCard
  | RemoveCard Card
  | UpdateCard Card
  | UpdateTotal Int
  | UpdateHand Int
  | UpdateOthers Int
  | SelectOnFocus Focus.FocusEvent
  | StartReorder CardId Drag.DragEvent
  | HandleDragBehavior Drag.DragEvent
  | ExecuteReorder CardId Drag.DragEvent

----------------------------------------------------------------

component :: H.Component (Const Void) Deck Deck Aff
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval { handleAction = action, receive = Just <<< Receive }
  }
  where

  initialState :: _ Deck
  initialState = identity

  render { cards, others, hand } = do
    let cardCount = countCards cards
    let deckCount = cardCount + others
    HH.div
      [ HP.class_ $ H.ClassName "rounded border-2 border-amber-500 p-1" ]
      [ renderHeader deckCount hand cardCount
      , renderCardList others cards
      , renderFooter others cardCount
      ]

  renderHeader deckCount handCount cardCount =
    HH.div
      [ HP.class_ $ H.ClassName "flex gap-1" ]
      [ HH.div
          [ HP.class_ $ H.ClassName "mx-1 flex grow flex-wrap text-sky-900" ]
          [ HU.fa "fa-layer-group" [ H.ClassName "m-1 text-2xl" ]
          , HH.div [ HP.class_ $ H.ClassName "m-1" ] [ HH.text "デッキ情報" ]
          ]
      , renderIntegerInput "hand" "手札枚数:" handCount 1 deckCount UpdateHand
      , renderIntegerInput "deck" "デッキ枚数:" deckCount cardCount deckLimit UpdateTotal
      ]

  renderCardList others cards =
    HK.ul
      [ HP.class_ $ H.ClassName "m-1"
      , HE.onDragEnter HandleDragBehavior
      , HE.onDragOver HandleDragBehavior
      ]
      $ renderCard others <$> cards

  renderCard others card = do
    let id = Id.toString card.id
    Tuple id $ HH.li
      [ HP.class_ $ H.ClassName "flex"
      , HP.ref $ H.RefLabel id
      , HE.onDrop $ ExecuteReorder card.id
      ]
      [ HH.div
          [ HP.class_ $ H.ClassName "mx-1 flex items-center" ]
          [ HH.div
              [ HP.class_ $ H.ClassName "hidden cursor-grab px-1 text-gray-500 active:cursor-grabbing md:block"
              , HP.draggable true
              , HE.onDragStart $ StartReorder card.id
              ]
              [ HU.fa_ "fa-grip-vertical" ]
          , HU.removeButton $ RemoveCard card
          ]
      , HH.div
          [ HP.class_ $ H.ClassName "flex grow border-b border-gray-500" ]
          [ HH.input
              [ HP.classes [ H.ClassName "grow", styleFormInput ]
              , HP.type_ HP.InputText
              , HP.id $ "name-" <> id
              , HP.value card.name
              , HE.onValueChange $ UpdateCard <<< card { name = _ }
              ]
          , HH.input
              [ HP.class_ styleFormNumber
              , HP.type_ HP.InputNumber
              , HP.id $ "count-" <> id
              , HP.step $ HP.Step 1.0
              , HP.value $ show card.count
              , HP.min 0.0
              , HP.max if String.null card.name then 0.0 else Int.toNumber (card.count + others)
              , HE.onFocus SelectOnFocus
              , HE.onValueChange $ UpdateCard <<< card { count = _ } <<< fromMaybe 0 <<< Int.fromString
              ]
          ]
      ]

  renderFooter otherCount cardCount =
    HH.div
      [ HP.class_ $ H.ClassName "flex gap-1" ]
      [ HH.div
          [ HP.class_ $ H.ClassName "mx-1 grow" ]
          [ HU.plusButton AddCard ]
      , renderIntegerInput "others" "その他のカード:" otherCount 0 (deckLimit - cardCount) UpdateOthers
      ]

  renderIntegerInput id text count min max handler =
    HH.div
      [ HP.class_ $ H.ClassName "mx-1 flex flex-wrap justify-end border-b border-gray-500" ]
      [ HH.label [ HP.class_ $ H.ClassName "p-1", HP.for id ] [ HH.text text ]
      , HH.input
          [ HP.class_ styleFormNumber
          , HP.type_ HP.InputNumber
          , HP.id id
          , HP.value $ show count
          , HP.step $ HP.Step 1.0
          , HP.min $ Int.toNumber min
          , HP.max $ Int.toNumber max
          , HE.onFocus SelectOnFocus
          , HE.onValueChange $ handler <<< fromMaybe 0 <<< Int.fromString
          ]
      ]

  styleFormNumber = collect H.ClassName (String.joinWith " ")
    [ H.ClassName "w-16 text-right"
    , styleFormInput
    ]

  styleFormInput = collect H.ClassName (String.joinWith " ")
    [ H.ClassName "appearance-none rounded-none border-b border-transparent bg-white p-1 text-gray-700"
    , H.ClassName "transition-colors duration-75 hover:border-gray-500 focus:border-slate-800 focus:outline-none"
    ]

  action = case _ of
    Receive deck -> do
      H.put deck
    AddCard -> do
      id <- generateId
      deck <- H.get
      H.raise deck { cards = Array.snoc deck.cards { id, name: "", count: 0 } }
    RemoveCard card -> do
      deck <- H.get
      let cards = Array.deleteBy (eq `on` _.id) card deck.cards
      H.raise deck { cards = cards, others = deck.others + card.count }
    UpdateCard card -> do
      deck <- H.get
      foldMap H.raise do
        i <- findById card.id deck.cards
        old <- deck.cards !! i
        let new = card { count = if String.null card.name then 0 else clamp 0 (old.count + deck.others) card.count }
        cards <- Array.updateAt i new deck.cards
        pure deck { cards = cards, others = deck.others - (new.count - old.count) }
    UpdateTotal total -> do
      deck <- H.get
      H.raise deck { others = total - countCards deck.cards }
    UpdateHand hand -> do
      deck <- H.get
      let total = countCards deck.cards + deck.others
      H.raise deck { hand = clamp 1 total hand }
    UpdateOthers others -> do
      deck <- H.get
      let cardCount = countCards deck.cards
      let deckCount = clamp cardCount deckLimit (cardCount + others)
      H.raise deck { others = deckCount - cardCount, hand = min deck.hand deckCount }
    SelectOnFocus event -> do
      let target = Input.fromEventTarget <=< Event.target <<< Focus.toEvent $ event
      H.liftEffect $ foldMap Input.select target
    StartReorder id event -> do
      let transfer = Drag.dataTransfer event
      H.liftEffect $ DataTransfer.setData (MediaType dragItemMediaType) (Id.toString id) transfer
      elem <- H.getRef $ H.RefLabel (Id.toString id)
      H.liftEffect $ elem # foldMap \e -> do
        DataTransfer.setDragImage transfer e 5 15
    HandleDragBehavior event -> do
      let transfer = Drag.dataTransfer event
      when (DataTransfer.types transfer == [dragItemMediaType]) do
        H.liftEffect do
          Event.preventDefault $ Drag.toEvent event
          DataTransfer.setDropEffect DataTransfer.Move transfer
    ExecuteReorder destination event -> do
      let transfer = Drag.dataTransfer event
      id <- H.liftEffect $ DataTransfer.getData (MediaType dragItemMediaType) transfer
      unless (String.null id) do
        H.liftEffect <<< Event.preventDefault $ Drag.toEvent event
        deck@{ cards } <- H.get
        foldMap H.raise do
          target <- Id.fromString id
          guard $ target /= destination
          from <- findById target cards
          to <- findById destination cards
          pure deck { cards = ArrayUtil.shiftInsert from to cards }

  dragItemMediaType = "tcg-calculator/card"

  countCards = alaF Additive Array.foldMap _.count

  findById id = Array.findIndex (_.id >>> (_ == id))

  deckLimit = 255
