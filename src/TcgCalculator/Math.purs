module TcgCalculator.Math
  ( Combination
  , PartitionNumber
  , Permutation
  , combinationNumber
  , combinations
  , distinctPermutations
  , partitionNumber
  )
  where

import Prelude

import Control.Monad.ST (ST)
import Data.Array (drop, findLastIndex, index, length, singleton, uncons, zipWith, (!!), (..), (:))
import Data.Array.Partial as P
import Data.Array.ST (STArray)
import Data.Array.ST as STA
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Function.Memoize (memoize2)
import Data.List.Lazy as L
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (unfoldr1)
import Partial.Unsafe (unsafePartial)
import Util.Array (swapST)

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

type PartitionNumber = Array (Array Int)

-- partitionNumber 4 -> [[4], [3, 1], [2, 2], [2, 1, 1], [1, 1, 1, 1]]
partitionNumber :: Int -> PartitionNumber
partitionNumber = go
  where
  go n | n < 0 = []
  go n = go' n n
  go' = memoize2 impl
    where
    impl 0 _ = [[]]
    impl n k = k .. 1 >>= \i -> (i : _) <$> go' (n - i) (min (n - i) i)

----------------------------------------------------------------

type Permutation a = Array (Array a)

-- distinctPermutations [3, 2, 1] -> [[3, 2, 1], [3, 1, 2], [2, 3, 1], [2, 1, 3], [1, 3, 2], [1, 2, 3]]
-- permutations for arrays with duplicated items
-- input array must be sorted in DESCENDING order
-- https://en.wikipedia.org/wiki/Permutation#Generation_in_lexicographic_order
distinctPermutations :: forall a. Ord a => Array a -> Permutation a
distinctPermutations = unfoldr1 \a -> Tuple a (prevPerm a)
  where
  prevPerm :: Array a -> Maybe (Array a)
  prevPerm a = do
    k <- findLastIndex identity $ zipWith (>) a (drop 1 a)
    v <- a !! k
    l <- findLastIndex (v > _) a
    pure $ STA.run do
      st <- STA.thaw a
      swapST k l st
      reverseST (k + 1) (length a - 1) st
      pure st
  reverseST :: forall h. Int -> Int -> STArray h a -> ST h Unit
  reverseST x y _ | x >= y = pure unit
  reverseST x y st = do
    swapST x y st
    reverseST (x + 1) (y - 1) st

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
