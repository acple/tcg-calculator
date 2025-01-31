module Worker.Main where

import Prelude

import Codec.JSON.DecodeError as DecodeError
import Control.Monad.Except (runExcept)
import Data.BigInt as BigInt
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class.Console as Console
import Foreign (unsafeFromForeign)
import TcgCalculator as TC
import TcgCalculator.Codec as Codec
import Web.Worker.GlobalScope as Worker
import Web.Worker.MessageEvent as MessageEvent

----------------------------------------------------------------

main :: Effect Unit
main = do
  Worker.onMessage \event -> do
    case runExcept <<< Codec.decode Codec.workerParam <<< unsafeFromForeign $ MessageEvent.data_ event of
      Left error -> Console.error $ DecodeError.print error
      Right { deck, condition } -> do
        let result = TC.calculate deck condition
        Worker.postMessage $ BigInt.toString result
    Worker.close
