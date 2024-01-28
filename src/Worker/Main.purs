module Worker.Main where

import Prelude

import Data.Argonaut.Decode (decodeJson, printJsonDecodeError)
import Data.BigInt as BigInt
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class.Console as Console
import Foreign (unsafeFromForeign)
import SharedTypes (TcgCalculatorWorkerParam)
import TcgCalculator as TC
import Web.Worker.GlobalScope as Worker
import Web.Worker.MessageEvent as MessageEvent

----------------------------------------------------------------

main :: Effect Unit
main = do
  Worker.onMessage \event -> do
    case decodeJson @TcgCalculatorWorkerParam <<< unsafeFromForeign $ MessageEvent.data_ event of
      Left error -> Console.error $ printJsonDecodeError error
      Right { deck, conditions } -> do
        let result = TC.calculate deck conditions
        Worker.postMessage $ BigInt.toString result
    Worker.close
