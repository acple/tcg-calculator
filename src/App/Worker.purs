module App.Worker where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Argonaut.Encode (encodeJson)
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(..), note)
import Data.Maybe (maybe)
import Data.Semigroup.Foldable (intercalateMap)
import Effect.Aff (Aff, effectCanceler, error, makeAff)
import Foreign (readString, renderForeignError)
import SharedTypes (TcgCalculatorWorkerParam)
import Web.HTML.Event.ErrorEvent as ErrorEvent
import Web.Worker.MessageEvent as MessageEvent
import Web.Worker.Worker as Worker

----------------------------------------------------------------

run :: TcgCalculatorWorkerParam -> Aff BigInt
run param = makeAff \reply -> do
  worker <- Worker.new "bundle/worker.js" Worker.defaultWorkerOptions { type = Worker.Module }
  worker # Worker.onMessage \event -> reply do
    result <- lmap (error <<< intercalateMap "\n" renderForeignError) <<< runExcept <<< readString $ MessageEvent.data_ event
    note (error "BigInt.fromString") $ BigInt.fromString result
  worker # Worker.onError \event -> do
    let message = maybe "Unknown worker error" ErrorEvent.message $ ErrorEvent.fromEvent event
    reply <<< Left $ error message
  Worker.postMessage (encodeJson param) worker
  pure $ effectCanceler (Worker.terminate worker)
