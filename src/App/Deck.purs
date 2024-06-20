module App.Deck where

import Prelude

import Control.Alternative (guard)
import Control.Monad.Maybe.Trans (runMaybeT)
import Data.Array ((!!))
import Data.Array as Array
import Data.Foldable (fold, traverse_)
import Data.Function (on)
import Data.Int as Int
import Data.Maybe (fromMaybe)
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
import TcgCalculator.Types (Card, Deck, Id, generateId)
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
  = AddCard
  | RemoveCard Card
  | UpdateCard Card
  | UpdateDeck Int
  | UpdateHand Int
  | UpdateOthers Int
  | SelectOnFocus Focus.FocusEvent
  | StartReorder Id Drag.DragEvent
  | HandleDragBehavior Drag.DragEvent
  | ExecuteReorder Id Drag.DragEvent

data Query a
  = SetDeck Deck a

----------------------------------------------------------------

component :: H.Component Query Unit Deck Aff
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval { handleAction = action, handleQuery = runMaybeT <<< query }
  }
  where

  initialState :: _ Deck
  initialState _ = { cards: [], others: 40, hand: 5 }

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
      , renderIntegerInput "手札枚数:" handCount 1 deckCount UpdateHand
      , renderIntegerInput "デッキ枚数:" deckCount cardCount deckLimit UpdateDeck
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
              , HP.value card.name
              , HE.onValueChange $ UpdateCard <<< card { name = _ }
              ]
          , HH.input
              [ HP.class_ styleFormNumber
              , HP.type_ HP.InputNumber
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
      , renderIntegerInput "その他のカード:" otherCount 0 (deckLimit - cardCount) UpdateOthers
      ]

  renderIntegerInput text count min max h =
    HH.div
      [ HP.class_ $ H.ClassName "mx-1 flex flex-wrap justify-end border-b border-gray-500" ]
      [ HH.div [ HP.class_ $ H.ClassName "m-1" ] [ HH.text text ]
      , HH.input
          [ HP.class_ styleFormNumber
          , HP.type_ HP.InputNumber
          , HP.value $ show count
          , HP.step $ HP.Step 1.0
          , HP.min $ Int.toNumber min
          , HP.max $ Int.toNumber max
          , HE.onFocus SelectOnFocus
          , HE.onValueChange $ h <<< fromMaybe 0 <<< Int.fromString
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
    AddCard -> do
      id <- generateId
      H.modify_ do
        cards <- _.cards
        _ { cards = Array.snoc cards { id, name: "", count: 0 } }
    RemoveCard card -> do
      raiseUpdated =<< H.modify do
        { cards, others } <- identity
        let cards' = Array.deleteBy ((==) `on` _.id) card cards
        _ { cards = cards', others = others + card.count }
    UpdateCard card -> do
      { cards, others } <- H.get
      fold do
        i <- Array.findIndex (_.id >>> (_ == card.id)) cards
        old <- cards !! i
        let new = if String.null card.name then card { count = 0 } else card { count = clamp 0 (old.count + others) card.count }
        cards' <- Array.updateAt i new cards
        pure $ raiseUpdated =<< H.modify _ { cards = cards', others = others - (new.count - old.count) }
    UpdateDeck total -> do
      cards <- H.gets _.cards
      let cardCount = countCards cards
      action $ UpdateOthers (total - cardCount)
    UpdateHand hand -> do
      raiseUpdated =<< H.modify do
        { cards, others } <- identity
        let deckCount = countCards cards + others
        _ { hand = clamp 1 deckCount hand }
    UpdateOthers others -> do
      raiseUpdated =<< H.modify do
        { cards, hand } <- identity
        let cardCount = countCards cards
        let deckCount = clamp cardCount deckLimit (cardCount + others)
        _ { others = deckCount - cardCount, hand = min hand deckCount }
    SelectOnFocus event -> do
      let element = Input.fromEventTarget <=< Event.target <<< Focus.toEvent $ event
      H.liftEffect $ traverse_ Input.select element
    StartReorder id event -> do
      let transfer = Drag.dataTransfer event
      H.liftEffect $ DataTransfer.setData (MediaType dragItemMediaType) (Id.toString id) transfer
      elem <- H.getRef $ H.RefLabel (Id.toString id)
      H.liftEffect $ elem # traverse_ \e -> do
        DataTransfer.setDragImage transfer e 5 15
    HandleDragBehavior event -> do
      let transfer = Drag.dataTransfer event
      when (DataTransfer.types transfer == [dragItemMediaType]) do
        H.liftEffect <<< Event.preventDefault $ Drag.toEvent event
    ExecuteReorder destination event -> do
      let transfer = Drag.dataTransfer event
      id <- H.liftEffect $ DataTransfer.getData (MediaType dragItemMediaType) transfer
      unless (String.null id) do
        H.liftEffect <<< Event.preventDefault $ Drag.toEvent event
        cards <- H.gets _.cards
        traverse_ (raiseUpdated <=< H.modify <<< flip _ { cards = _ }) do
          target <- Id.fromString id
          guard $ target /= destination
          from <- Array.findIndex (_.id >>> (_ == target)) cards
          to <- Array.findIndex (_.id >>> (_ == destination)) cards
          pure $ ArrayUtil.shiftInsert from to cards

  dragItemMediaType = "tcg-calculator/card"

  raiseUpdated deck =
    H.raise deck { cards = Array.filter (_.name >>> not String.null) deck.cards }

  countCards = alaF Additive Array.foldMap _.count

  deckLimit = 255

  query :: _ ~> _
  query = case _ of
    SetDeck deck a -> do
      H.put deck
      pure a
