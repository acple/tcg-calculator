module App.Worker where

import Prelude

import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(..), note)
import Data.Maybe (maybe)
import Effect.Aff (Aff, effectCanceler, error, makeAff)
import Foreign (unsafeFromForeign)
import TcgCalculator.Types (WorkerParam)
import Web.HTML.Event.ErrorEvent as ErrorEvent
import Web.Worker.MessageEvent as MessageEvent
import Web.Worker.Worker as Worker

----------------------------------------------------------------

run :: WorkerParam -> Aff BigInt
run param = makeAff \reply -> do
  worker <- Worker.new "bundle/worker.js" Worker.defaultWorkerOptions { type = Worker.Module }
  worker # Worker.onMessage \event -> reply do
    let result = unsafeFromForeign $ MessageEvent.data_ event
    note (error "BigInt.fromString") $ BigInt.fromString result
  worker # Worker.onError \event -> do
    Worker.terminate worker
    let message = maybe "Unknown worker error" ErrorEvent.message $ ErrorEvent.fromEvent event
    reply $ Left (error message)
  Worker.postMessage param worker
  pure $ effectCanceler (Worker.terminate worker)
