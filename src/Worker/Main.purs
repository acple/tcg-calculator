module Worker.Main where

import Prelude

import Data.BigInt as BigInt
import Effect (Effect)
import Foreign (unsafeFromForeign)
import TcgCalculator as TC
import Web.Worker.GlobalScope as Worker
import Web.Worker.MessageEvent as MessageEvent

----------------------------------------------------------------

main :: Effect Unit
main = do
  Worker.onMessage \event -> do
    let { deck, condition } = unsafeFromForeign $ MessageEvent.data_ event
    let result = TC.calculate deck condition
    Worker.postMessage $ BigInt.toString result
    Worker.close
