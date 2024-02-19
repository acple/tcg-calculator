module SharedTypes where

import TcgCalculator.Types (Conditions, Deck)

----------------------------------------------------------------

type TcgCalculatorWorkerParam = { deck :: Deck, conditions :: Array Conditions }
