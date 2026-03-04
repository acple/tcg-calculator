module TcgCalculator.Math
  ( Combination
  , combinationNumber
  , combinations
  )
  where

import Prelude

import Data.Array (index, length, singleton, uncons, zipWith, (:))
import Data.Array.Partial as P
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.List.Lazy as L
import Data.Maybe (Maybe(..), fromMaybe)
import Partial.Unsafe (unsafePartial)

----------------------------------------------------------------

-- nCr
combinationNumber :: Int -> Int -> BigInt
combinationNumber = go
  where
  go n r | n < 0 || n < r = zero
  go n r | n - r < r = go n (n - r)
  go _ 0 = one
  go n 1 = BigInt.fromInt n
  go n r = fromMaybe zero $ pascalTriangle L.!! (n - 2) >>= flip index (r - 1)
  pascalTriangle = L.iterate step [BigInt.fromInt 2] -- pascal triangle starting at the third row, omitting the first element and the second half
  step prev = unsafePartial $ zipWith (+) ([one] <> prev) if BigInt.odd (P.head prev) then prev <> [P.last prev] else prev

----------------------------------------------------------------

type Combination a = Array (Array a)

-- combinations 2 [1, 2, 3, 4] -> [[1, 2], [1, 3], [1, 4], [2, 3], [2, 4], [3, 4]]
combinations :: forall a. Int -> Array a -> Combination a
combinations n a
  | n == 0        = [[]]
  | length a == n = [a]
  | length a < n  = []
  | n == 1        = singleton <$> a
  | otherwise     = case uncons a of
      Just { head, tail } -> ((head : _) <$> combinations (n - 1) tail) <> combinations n tail
      _ -> []
