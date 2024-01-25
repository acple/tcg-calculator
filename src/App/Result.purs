module App.Result where

import Prelude

import App.Worker as Worker
import Control.Monad.Maybe.Trans (runMaybeT)
import Data.Array.NonEmpty (NonEmptyArray)
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
import Halogen.Query.HalogenM (ForkId)
import TcgCalculator as TC
import TcgCalculator.Types (Condition, Deck)
import Util.Halogen as HU

----------------------------------------------------------------

data Query a
  = Calculate Deck (Array (NonEmptyArray Condition)) a

----------------------------------------------------------------

component :: H.Component Query Unit Void Aff
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval { handleQuery = runMaybeT <<< query }
  }
  where

  initialState :: _ -> { combination :: BigInt, total :: BigInt, calculation :: Maybe ForkId }
  initialState _ = { combination: zero, total: zero, calculation: Nothing }

  render { combination, total, calculation } =
    HH.div
      [ HP.class_ $ H.ClassName "flex justify-end items-center min-w-60" ]
      [ HH.div
          [ HP.class_ $ H.ClassName "flex justify-end items-center gap-1 mx-1 w-36" ]
          case calculation of
            Just _ ->
              [ HH.span [ HP.class_ $ H.ClassName "text-2xl" ] [ HH.text "Calculating" ]
              , HU.fa_ "fa-spinner fa-pulse"
              ]
            _ ->
              [ HH.span
                  [ HP.class_ $ H.ClassName "text-2xl" ]
                  [ HH.text if total == zero
                      then "N/A"
                      else Format.toStringWith (Format.fixed 4) (100.0 * ((/) `on` BigInt.toNumber) combination total) <> "%"
                  ]
              ]
      , HH.div
          [ HP.class_ $ H.ClassName "flex justify-end mx-1 min-w-20" ]
          [ HH.div
              [ HP.class_ $ H.ClassName "flex flex-col items-end" ]
              [ HH.div [ HP.class_ $ H.ClassName "px-1" ] [ HH.text $ BigInt.toString combination ]
              , HH.hr [ HP.class_ $ H.ClassName "border-gray-400 w-full" ]
              , HH.div [ HP.class_ $ H.ClassName "px-1" ] [ HH.text $ BigInt.toString total ]
              ]
          ]
      ]

  query :: _ ~> _
  query = case _ of
    Calculate deck conditions a -> H.lift do
      newCalculation <- H.fork do
        let deck' = TC.normalizeDeck deck conditions
        result <- H.liftAff <<< attempt $ Worker.run { deck: deck', conditions }
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
