module Util.Array where

import Prelude

import Data.Array.ST as STA
import Data.Maybe (Maybe(..))

----------------------------------------------------------------

swap :: forall a. Int -> Int -> Array a -> Array a
swap x y array = STA.run do
  st <- STA.thaw array
  a <- STA.peek x st
  b <- STA.peek y st
  case a, b of
    Just a', Just b' -> STA.poke x b' st *> STA.poke y a' st $> st
    _, _ -> pure st
