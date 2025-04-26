module App.Result where

import Prelude

import App.Worker as Worker
import Control.Alternative (guard)
import Control.Monad.Maybe.Trans (runMaybeT)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Function (on)
import Data.Maybe (Maybe(..))
import Data.Number.Format as Format
import Effect.Aff (Aff, attempt, throwError)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TcgCalculator as TC
import TcgCalculator.Types (ConditionSet, Deck)
import Util.Halogen as HU

----------------------------------------------------------------

data Query a
  = Calculate Deck ConditionSet a

----------------------------------------------------------------

component :: H.Component Query Unit Void Aff
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval { handleQuery = runMaybeT <<< query }
  }
  where

  initialState :: _ { deck :: Deck, condition :: ConditionSet, combination :: BigInt, total :: BigInt, calculation :: Maybe H.ForkId }
  initialState _ = { deck: { cards: [], others: 0, hand: 0 }, condition: [], combination: zero, total: zero, calculation: Nothing }

  render { combination, total, calculation } =
    HH.div
      [ HP.class_ $ H.ClassName "flex min-w-60 items-center" ]
      [ HH.div
          [ HP.class_ $ H.ClassName "mx-1 flex w-36 items-center justify-end gap-1" ]
          case calculation of
            Just _ ->
              [ HH.div [ HP.class_ $ H.ClassName "text-2xl" ] [ HH.text "Calculating" ]
              , HU.fa_ "fa-spinner fa-pulse"
              ]
            _ ->
              [ HH.div
                  [ HP.class_ $ H.ClassName "text-2xl" ]
                  [ HH.text if total == zero
                      then "N/A"
                      else Format.toStringWith (Format.fixed 4) (100.0 * ((/) `on` BigInt.toNumber) combination total) <> "%"
                  ]
              ]
      , HH.div
          [ HP.class_ $ H.ClassName "mx-1 flex min-w-20 justify-end" ]
          [ HH.div
              [ HP.class_ $ H.ClassName "flex flex-col divide-y divide-gray-400" ]
              [ HH.div [ HP.class_ $ H.ClassName "px-1 text-right" ] [ HH.text $ BigInt.toString combination ]
              , HH.div [ HP.class_ $ H.ClassName "px-1 text-right" ] [ HH.text $ BigInt.toString total ]
              ]
          ]
      ]

  query :: _ ~> _
  query = case _ of
    Calculate deck condition a -> do
      let deck' = TC.normalizeDeck deck condition
      { deck: currentDeck, condition: currentCondition } <- H.get
      guard $ deck' /= currentDeck || condition /= currentCondition
      H.modify_ _ { deck = deck', condition = condition }
      newCalculation <- H.lift $ H.fork do
        result <- H.liftAff <<< attempt $ Worker.run { deck: deck', condition }
        case result of
          Left error -> do
            H.modify_ _ { combination = zero, total = zero, calculation = Nothing }
            throwError error
          Right combination -> do
            let total = TC.calculateTotal deck'
            H.modify_ _ { combination = combination, total = total, calculation = Nothing }
      currentCalculation <- H.gets _.calculation
      H.modify_ _ { calculation = Just newCalculation }
      H.lift $ foldMap H.kill currentCalculation
      pure a
