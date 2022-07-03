module App.Result where

import Prelude

import Control.Monad.Maybe.Trans (runMaybeT)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Foldable (foldMap)
import Data.Function (on)
import Data.Maybe (Maybe(..))
import Data.Number.Format as Format
import Effect.Aff (Aff, Milliseconds(..))
import Effect.Aff as Aff
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM (ForkId)
import TcgCalculator as TC
import TcgCalculator.Types (Condition, Deck)

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
      [ HP.class_ $ H.ClassName "flex align-baseline" ]
      [ HH.div
          [ HP.class_ $ H.ClassName "flex justify-end items-center w-40 py-1 px-3 text-2xl" ]
          [ HH.text $ case calculation of
              Just _ -> "Calculating..."
              _ -> if total == zero
                then "N/A"
                else do
                  let prob = 100.0 * ((/) `on` BigInt.toNumber) combination total
                  Format.toStringWith (Format.fixed 4) prob <> "%"
          ]
      , HH.div
          [ HP.class_ $ H.ClassName "w-20 min-w-fit flex justify-end" ]
          [ HH.div
              [ HP.class_ $ H.ClassName "inline-flex flex-col mx-1" ]
              [ HH.div [ HP.class_ $ H.ClassName "text-right px-1" ] [ HH.text $ BigInt.toString combination ]
              , HH.hr_
              , HH.div [ HP.class_ $ H.ClassName "text-right px-1" ] [ HH.text $ BigInt.toString total ]
              ]
          ]
      ]

  query :: _ ~> _
  query = case _ of
    Calculate deck conditions a -> H.lift do
      newCalculation <- H.fork do
        H.liftAff $ Aff.delay (Milliseconds 350.0)
        let deck' = TC.normalizeDeck deck conditions
        H.liftAff $ Aff.delay (Milliseconds 0.0)
        let combination = TC.calculate deck' conditions
        H.liftAff $ Aff.delay (Milliseconds 0.0)
        let total = TC.calculateTotal deck'
        H.modify_ _ { combination = combination, total = total, calculation = Nothing }
      currentCalculation <- H.gets _.calculation
      H.modify_ _ { calculation = Just newCalculation }
      foldMap H.kill currentCalculation
      pure a
