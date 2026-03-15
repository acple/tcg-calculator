module Worker.Client (run) where

import Prelude

import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(..), note)
import Data.Maybe (maybe)
import Effect (Effect)
import Effect.Aff (Aff, effectCanceler, error, makeAff)
import Foreign (unsafeFromForeign)
import TcgCalculator.Types (WorkerParam)
import Web.HTML.Event.ErrorEvent as ErrorEvent
import Web.Worker.MessageEvent as MessageEvent
import Web.Worker.Worker (Worker)
import Web.Worker.Worker as Worker

----------------------------------------------------------------

foreign import createWorker :: Effect Worker

run :: WorkerParam -> Aff BigInt
run param = makeAff \reply -> do
  worker <- createWorker
  worker # Worker.onMessage \event -> reply do
    let result = unsafeFromForeign $ MessageEvent.data_ event
    note (error "BigInt.fromString") $ BigInt.fromString result
  worker # Worker.onError \event -> do
    Worker.terminate worker
    let message = maybe "Unknown worker error" ErrorEvent.message $ ErrorEvent.fromEvent event
    reply $ Left (error message)
  Worker.postMessage param worker
  pure $ effectCanceler (Worker.terminate worker)
