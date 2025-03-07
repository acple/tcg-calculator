module App.Result where

import Prelude

import App.Worker as Worker
import Control.Monad.Maybe.Trans (runMaybeT)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(..))
import Data.Foldable (traverse_)
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

  initialState :: _ { combination :: BigInt, total :: BigInt, calculation :: Maybe H.ForkId }
  initialState _ = { combination: zero, total: zero, calculation: Nothing }

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
              [ HH.div [ HP.class_ $ H.ClassName "w-full px-1 text-right" ] [ HH.text $ BigInt.toString combination ]
              , HH.div [ HP.class_ $ H.ClassName "w-full px-1 text-right" ] [ HH.text $ BigInt.toString total ]
              ]
          ]
      ]

  query :: _ ~> _
  query = case _ of
    Calculate deck condition a -> H.lift do
      newCalculation <- H.fork do
        let deck' = TC.normalizeDeck deck condition
        result <- H.liftAff <<< attempt $ Worker.run { deck: deck', condition }
        case result of
          Left error -> do
            H.put { combination: zero, total: zero, calculation: Nothing }
            throwError error
          Right combination -> do
            let total = TC.calculateTotal deck'
            H.put { combination, total, calculation: Nothing }
      currentCalculation <- H.gets _.calculation
      H.modify_ _ { calculation = Just newCalculation }
      traverse_ H.kill currentCalculation
      pure a
