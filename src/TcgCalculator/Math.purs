module TcgCalculator.Math
  ( Combination
  , PartitionNumber
  , PascalTriangle
  , Permutation
  , combinationNumber
  , combinations
  , createPascalTriangle
  , partitionNumber
  , partitionNumbers
  , pascalTriangle
  , permutations
  )
  where

import Prelude

import Data.Array (filter, fromFoldable, head, insertAt, length, singleton, uncons, zipWith, (!!), (..), (:))
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Foldable (fold, product)
import Data.List as L
import Data.Maybe (Maybe(..), fromMaybe)

----------------------------------------------------------------

type PascalTriangle = Array (Array BigInt)

createPascalTriangle :: Int -> PascalTriangle
createPascalTriangle size = f [one] size
  where
  f _ n | n < 0 = []
  f r 0 = [r]
  f r n = r : f (zipWith (+) ([zero] <> r) (r <> [zero])) (n - 1)

ptCacheSize :: Int
ptCacheSize = 255

pascalTriangle :: PascalTriangle
pascalTriangle = createPascalTriangle ptCacheSize

combinationNumber :: Int -> Int -> BigInt
combinationNumber n r
  | n < r            = zero
  | r == 0 || n == r = one
  | r == 1           = BigInt.fromInt n
  | n <= ptCacheSize = fromMaybe zero $ pascalTriangle !! n >>= (_ !! r) -- fast path using cached pascal triangle
  | otherwise        = do
      let k = min r (n - r)
      product' ((n - k + 1) .. n) / product' (1 .. k)
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
partitionNumbers 0 = [[]]
partitionNumbers n = fromFoldable $ buildPartitionNumbers n

buildPartitionNumbers :: Int -> L.List PartitionNumber
buildPartitionNumbers 0 = L.singleton [[]]
buildPartitionNumbers k = do
  let prev = buildPartitionNumbers (k - 1)
  new prev 1 L.: prev
  where
  new :: L.List PartitionNumber -> Int -> PartitionNumber
  new (h L.: t) i = new t (i + 1) <> ((i : _) <$> filter ((_ <= i) <<< fromMaybe 0 <<< head) h)
  new _         _ = []

----------------------------------------------------------------

type Permutation a = Array (Array a)

-- permutations [1, 2, 3] -> [[1, 2, 3], [2, 1, 3], [2, 3, 1], [1, 3, 2], [3, 1, 2], [3, 2, 1]]
permutations :: Array ~> Permutation
permutations [] = [[]]
permutations [a] = [[a]]
permutations a = case uncons a of
  Just { head, tail } -> do
    let r = 0 .. length tail
    p <- permutations tail
    r <#> \i -> fold $ insertAt i head p
  _ -> []

----------------------------------------------------------------

type Combination a = Array (Array a)

-- combination 2 [1, 2, 3, 4] -> [[1, 2], [1, 3], [1, 4], [2, 3], [2, 4], [3, 4]]
combinations :: forall a. Int -> Array a -> Combination a
combinations n a
  | length a < n  = []
  | length a == n = [a]
  | n == 0        = [[]]
  | n == 1        = singleton <$> a
  | otherwise     = case uncons a of
      Just { head, tail } -> ((head : _) <$> combinations (n - 1) tail) <> combinations n tail
      _ -> []
