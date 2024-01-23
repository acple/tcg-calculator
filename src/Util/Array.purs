module Util.Array where

import Prelude

import Control.Monad.ST (ST)
import Data.Array.ST (STArray)
import Data.Array.ST as STA
import Data.Maybe (Maybe(..))

----------------------------------------------------------------

swap :: forall a. Int -> Int -> Array a -> Array a
swap x y array = STA.run do
  st <- STA.thaw array
  swapST x y st
  pure st

swapST :: forall h a. Int -> Int -> STArray h a -> ST h Unit
swapST x y st = do
  a <- STA.peek x st
  b <- STA.peek y st
  case a, b of
    Just a', Just b' -> void do STA.poke x b' st *> STA.poke y a' st
    _, _ -> pure unit
