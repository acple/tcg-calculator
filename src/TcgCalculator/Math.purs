module TcgCalculator.Math
  ( Combination
  , PartitionNumber
  , PascalTriangle
  , Permutation
  , combinationNumber
  , combinations
  , createPascalTriangle
  , distinctPermutations
  , partitionNumber
  , partitionNumbers
  , pascalTriangle
  )
  where

import Prelude

import Control.Monad.ST (ST)
import Data.Array (drop, dropWhile, findLastIndex, fromFoldable, head, length, singleton, uncons, unsafeIndex, zipWith, (!!), (..), (:))
import Data.Array.ST (STArray)
import Data.Array.ST as STA
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Foldable (fold, product)
import Data.List as L
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (iterateN, unfoldr1)
import Partial.Unsafe (unsafePartial)
import Util.Array (swapST)

----------------------------------------------------------------

type PascalTriangle = Array (Array BigInt)

createPascalTriangle :: Int -> PascalTriangle
createPascalTriangle size | size <= 0 = []
createPascalTriangle size = [one] # iterateN size \r -> zipWith (+) ([zero] <> r) (r <> [zero])

ptCacheSize :: Int
ptCacheSize = 64

pascalTriangle :: PascalTriangle
pascalTriangle = createPascalTriangle ptCacheSize

combinationNumber :: Int -> Int -> BigInt
combinationNumber n r
  | n < 0 || r < 0  = zero
  | r == 0          = one
  | r == 1          = BigInt.fromInt n
  | n - r < r       = combinationNumber n (n - r)
  | n < ptCacheSize = unsafePartial $ pascalTriangle `unsafeIndex` n `unsafeIndex` r -- fast path using cached pascal triangle
  | otherwise       = product' ((n - r + 1) .. n) / product' (1 .. r)
  where
  product' = product <<< map BigInt.fromInt

----------------------------------------------------------------

type PartitionNumber = Array (Array Int)

-- partitionNumber 4 -> [[4], [3, 1], [2, 2], [2, 1, 1], [1, 1, 1, 1]]
partitionNumber :: Int -> PartitionNumber
partitionNumber n | n < 0 = []
partitionNumber 0 = [[]]
partitionNumber n = fold <<< L.head $ buildPartitionNumbers n

-- partitionNumbers 3 -> [partitionNumber 3, partitionNumber 2, paritionNumber 1, partitionNumber 0]
partitionNumbers :: Int -> Array PartitionNumber
partitionNumbers n | n < 0 = []
partitionNumbers 0 = [[[]]]
partitionNumbers n = fromFoldable $ buildPartitionNumbers n

buildPartitionNumbers :: Int -> L.List PartitionNumber
buildPartitionNumbers 0 = L.singleton [[]]
buildPartitionNumbers k = do
  let prev = buildPartitionNumbers (k - 1)
  new prev 1 L.: prev
  where
  new :: L.List PartitionNumber -> Int -> PartitionNumber
  new (h L.: t) i = new t (i + 1) <> ((i : _) <$> dropWhile (maybe false (i < _) <<< head) h)
  new _         _ = []

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

-- combination 2 [1, 2, 3, 4] -> [[1, 2], [1, 3], [1, 4], [2, 3], [2, 4], [3, 4]]
combinations :: forall a. Int -> Array a -> Combination a
combinations n a
  | n == 0        = [[]]
  | length a == n = [a]
  | length a < n  = []
  | n == 1        = singleton <$> a
  | otherwise     = case uncons a of
      Just { head, tail } -> ((head : _) <$> combinations (n - 1) tail) <> combinations n tail
      _ -> []
