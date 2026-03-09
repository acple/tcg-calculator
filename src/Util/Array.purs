module Util.Array where

import Prelude

import Data.Array.ST as STA
import Data.Maybe (Maybe(..))

----------------------------------------------------------------

-- swap 1 3 [1, 2, 3, 4, 5] -> [1, 4, 3, 2, 5]
swap :: forall a. Int -> Int -> Array a -> Array a
swap x y array = STA.run do
  st <- STA.thaw array
  a <- STA.peek x st
  b <- STA.peek y st
  case a, b of
    Just a', Just b' -> void do STA.poke x b' st *> STA.poke y a' st
    _, _ -> pure unit
  pure st

-- shiftInsert 1 3 [1, 2, 3, 4, 5] -> [1, 3, 4, 2, 5]
shiftInsert :: forall a. Int -> Int -> Array a -> Array a
shiftInsert from to array | from == to = array
shiftInsert from to array = STA.run do
  st <- STA.thaw array
  item <- STA.splice from 1 [] st
  _ <- STA.splice to 0 item st
  pure st
