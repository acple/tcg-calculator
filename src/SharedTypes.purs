module SharedTypes where

import Data.Array.NonEmpty (NonEmptyArray)
import TcgCalculator.Types (Condition, Deck)

----------------------------------------------------------------

type TcgCalculatorWorkerParam = { deck :: Deck, conditions :: Array (NonEmptyArray Condition) }
