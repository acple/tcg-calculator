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

shiftInsert :: forall a. Int -> Int -> Array a -> Array a
shiftInsert from to array | from == to = array
shiftInsert from to array = STA.run do
  st <- STA.thaw array
  item <- STA.splice from 1 [] st
  _ <- STA.splice to 0 item st
  pure st
