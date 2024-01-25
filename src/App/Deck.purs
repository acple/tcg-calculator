module App.Deck where

import Prelude

import Control.Monad.Maybe.Trans (runMaybeT)
import Data.Array ((!!))
import Data.Array as Array
import Data.Foldable (fold)
import Data.Function (on)
import Data.Int as Int
import Data.Maybe (fromMaybe)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (alaF, collect)
import Data.String as String
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TcgCalculator.Types (Card, Deck, generateId)
import Util.Array as ArrayUtil
import Util.Halogen as HU

----------------------------------------------------------------

type Index = Int

data Action
  = AddCard
  | RemoveCard Card
  | UpdateCard Card
  | UpdateDeck Int
  | UpdateHand Int
  | UpdateOthers Int
  | Swap Index Index

data Query a
  = SetDeck Deck a

----------------------------------------------------------------

component :: H.Component Query Unit Deck Aff
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = action, handleQuery = runMaybeT <<< query }
  }
  where

  initialState :: _ -> Deck
  initialState _ = { cards: [], others: 40, hand: 5 }

  render { cards, others, hand } = do
    let cardCount = countCards cards
    let deckCount = cardCount + others
    HH.div
      [ HP.class_ $ H.ClassName "p-1 rounded border-2 border-amber-500" ]
      [ renderHeader deckCount hand cardCount
      , HH.ul [ HP.class_ $ H.ClassName "m-1" ] $ renderCard others <$> cards
      , renderFooter others cardCount
      ]

  renderHeader deckCount handCount cardCount =
    HH.div
      [ HP.class_ $ H.ClassName "flex items-baseline gap-1" ]
      [ HH.span
          [ HP.class_ $ H.ClassName "grow mx-3 my-1 inline-flex items-baseline gap-2 text-sky-900" ]
          [ HU.fa "fa-layer-group" [ H.ClassName "text-2xl" ]
          , HH.text "デッキ情報"
          ]
      , HH.div [ HP.class_ $ H.ClassName "mx-1 border-b border-gray-500 text-right" ]
          [ HH.span [ HP.class_ $ H.ClassName "mx-1" ] [ HH.text "手札枚数:" ]
          , HH.input
              [ HP.class_ styleFormNumber
              , HP.type_ HP.InputNumber
              , HP.step $ HP.Step 1.0
              , HP.value $ show handCount
              , HP.min 1.0
              , HP.max $ Int.toNumber deckCount
              , HE.onValueChange (UpdateHand <<< fromMaybe 0 <<< Int.fromString)
              ]
          ]
      , HH.div [ HP.class_ $ H.ClassName "mx-1 border-b border-gray-500 text-right" ]
          [ HH.span [ HP.class_ $ H.ClassName "mx-1" ] [ HH.text "デッキ枚数:" ]
          , HH.input
              [ HP.class_ styleFormNumber
              , HP.type_ HP.InputNumber
              , HP.step $ HP.Step 1.0
              , HP.value $ show deckCount
              , HP.min $ Int.toNumber cardCount
              , HP.max $ Int.toNumber deckLimit
              , HE.onValueChange (UpdateDeck <<< fromMaybe 0 <<< Int.fromString)
              ]
          ]
      ]

  renderCard others card =
    HH.li
      [ HP.class_ $ H.ClassName "flex border-b border-gray-500" ]
      [ HH.div
          [ HP.class_ $ H.ClassName "mx-1" ]
          [ HU.removeButton (RemoveCard card) ]
      , HH.input
          [ HP.classes [ H.ClassName "grow", styleFormInput ]
          , HP.type_ HP.InputText
          , HP.value card.name
          , HE.onValueChange (UpdateCard <<< card { name = _ })
          ]
      , HH.input
          [ HP.class_ styleFormNumber
          , HP.type_ HP.InputNumber
          , HP.step $ HP.Step 1.0
          , HP.value $ show card.count
          , HP.min 0.0
          , HP.max if String.null card.name then 0.0 else Int.toNumber (card.count + others)
          , HE.onValueChange (UpdateCard <<< card { count = _ } <<< fromMaybe 0 <<< Int.fromString)
          ]
      ]

  renderFooter otherCount cardCount =
    HH.div
      [ HP.class_ $ H.ClassName "flex items-baseline gap-1" ]
      [ HH.div
          [ HP.class_ $ H.ClassName "grow mx-1" ]
          [ HU.plusButton AddCard ]
      , HH.div [ HP.class_ $ H.ClassName "mx-1 border-b border-gray-500" ]
          [ HH.span [ HP.class_ $ H.ClassName "mx-1" ] [ HH.text "その他のカード:" ]
          , HH.input
              [ HP.class_ styleFormNumber
              , HP.type_ HP.InputNumber
              , HP.value $ show otherCount
              , HP.step $ HP.Step 1.0
              , HP.min 0.0
              , HP.max $ Int.toNumber (deckLimit - cardCount)
              , HE.onValueChange (UpdateOthers <<< fromMaybe 0 <<< Int.fromString)
              ]
          ]
      ]

  styleFormNumber = collect H.ClassName (String.joinWith " ")
    [ H.ClassName "w-16 text-right"
    , styleFormInput
    ]

  styleFormInput = collect H.ClassName (String.joinWith " ")
    [ H.ClassName "appearance-none py-1 px-1 m-0 border-b transition-colors duration-75"
    , H.ClassName "bg-white text-gray-700 border-transparent hover:border-gray-500"
    , H.ClassName "focus:outline-none focus:border-slate-800"
    ]

  action = case _ of
    AddCard -> do
      id <- generateId
      H.modify_ do
        cards <- _.cards
        _ { cards = Array.snoc cards { id, name: "", count: 0 } }
    RemoveCard card -> do
      { cards, others } <- H.get
      let cards' = Array.deleteBy ((==) `on` _.id) card cards
      raiseUpdate =<< H.modify _ { cards = cards', others = others + card.count }
    UpdateCard card -> do
      { cards, others } <- H.get
      fold do
        i <- Array.findIndex (_.id >>> (_ == card.id)) cards
        old <- cards !! i
        let new = if String.null card.name then card { count = 0 } else card { count = clamp 0 (old.count + others) card.count }
        cards' <- Array.updateAt i new cards
        pure $ raiseUpdate =<< H.modify _ { cards = cards', others = others - (new.count - old.count) }
    UpdateDeck total -> do
      cards <- H.gets _.cards
      let cardCount = countCards cards
      action $ UpdateOthers (total - cardCount)
    UpdateHand hand -> do
      raiseUpdate =<< H.modify do
        { cards, others } <- identity
        let deckCount = countCards cards + others
        _ { hand = clamp 1 deckCount hand }
    UpdateOthers others -> do
      raiseUpdate =<< H.modify do
        { cards, hand } <- identity
        let cardCount = countCards cards
        let deckCount = clamp cardCount deckLimit (cardCount + others)
        _ { others = deckCount - cardCount, hand = min hand deckCount }
    Swap x y -> do -- TODO
      H.modify_ do
        cards <- _.cards
        _ { cards = ArrayUtil.swap x y cards }

  raiseUpdate deck =
    H.raise deck { cards = Array.filter (_.name >>> not String.null) deck.cards }

  countCards = alaF Additive Array.foldMap _.count

  deckLimit = 255

  query :: _ ~> _
  query = case _ of
    SetDeck deck a -> do
      H.put deck
      pure a
