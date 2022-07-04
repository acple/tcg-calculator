module TcgCalculator where

import Prelude

import Control.Alternative (empty, guard)
import Data.Array (all, any, concatMap, filter, foldMap, group, groupAllBy, length, nubByEq, nubEq, replicate, take, unionBy, zipWith, (\\))
import Data.Array.NonEmpty (NonEmptyArray, foldl1, toArray)
import Data.BigInt (BigInt)
import Data.Foldable (and, product, sum)
import Data.Function (on)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (alaF, unwrap)
import TcgCalculator.Math (Combination, combinationNumber, combinations, partitionNumber, partitionNumbers, permutations)
import TcgCalculator.Types (Card, Cards, Condition(..), ConditionMode(..), Deck)

----------------------------------------------------------------

calculate :: Deck -> Array (NonEmptyArray Condition) -> BigInt
calculate deck conditions = do
  let drawPattern = generateDrawPatterns deck
  let conditionPattern = buildConditionPattern =<< conditions
  let pattern = filter (\d -> any (satisfyCondition d) conditionPattern) drawPattern
  sum $ calculatePatternCount deck <$> pattern

normalizeDeck :: Deck -> Array (NonEmptyArray Condition) -> Deck
normalizeDeck deck conditions = do
  let used = usedCards conditions
  let unused = deck.cards \\ used
  deck { cards = used, others = deck.others + sumBy _.count unused }
  where
  usedCards = nubByEq ((==) `on` _.id) <<< concatMap (_.cards <<< unwrap) <<< concatMap toArray

calculateTotal :: Deck -> BigInt
calculateTotal { cards, others, hand } = combinationNumber (sumBy _.count cards + others) hand

----------------------------------------------------------------

-- 各カードをそれぞれ何枚引いているかの状態を表す
type DrawPattern = Array { card :: Card, draw :: Int }

generateDrawPatterns :: Deck -> Array DrawPattern
generateDrawPatterns { cards, others, hand } = ado
  let zeroDrawPattern = { card: _, draw: 0 } <$> cards
  drawPattern <- mkDrawPattern' cards $ join <<< take (others + 1) $ partitionNumbers hand
  in unionBy ((==) `on` _.card.id) drawPattern zeroDrawPattern

calculatePatternCount :: Deck -> DrawPattern -> BigInt
calculatePatternCount { others, hand } pattern = do
  let patternCount = product $ pattern <#> \{ card: { count }, draw } -> combinationNumber count draw -- 条件のカードを引くときの組み合わせの数
  let drawCount = sumBy _.draw pattern
  patternCount * combinationNumber others (hand - drawCount) -- 残りの手札に条件外のカードを引く組み合わせの数

----------------------------------------------------------------

-- 各カードを何枚まで引いて良いかの条件の組み合わせを表す
type ConditionPattern = Array { card :: Card, min :: Int, max :: Int }

satisfyCondition :: DrawPattern -> ConditionPattern -> Boolean
satisfyCondition dp cp = and do
  { card, min, max } <- cp
  { card: card', draw } <- dp
  guard $ card.id == card'.id
  pure $ min <= draw && draw <= max

-- 一列の条件を満たすパターンのリストを生成
buildConditionPattern :: NonEmptyArray Condition -> Array ConditionPattern
buildConditionPattern conditions = do
  let patterns = mkConditionPattern <$> conditions
  foldl1 <@> patterns $ \left right -> do
    l <- left
    r <- right
    let merged = mergeConditionPattern l r
    if isValidConditionPattern merged then pure merged else empty

mergeConditionPattern :: ConditionPattern -> ConditionPattern -> ConditionPattern
mergeConditionPattern left right = foldl1 merge <$> groupAllBy (comparing _.card) (left <> right)
  where
  merge { card, min: min1, max : max1 } { min: min2, max : max2 } = { card, min: min1 + min2, max: min max1 max2 }

isValidConditionPattern :: ConditionPattern -> Boolean
isValidConditionPattern = all \{ card: { count }, min, max } -> min <= max && min <= count

-- Condition に対応する全パターンのリストを出力する
mkConditionPattern :: Condition -> Array ConditionPattern
mkConditionPattern (Condition { mode, count, cards }) = case mode of
  -- cards の中から count 枚以上を引くパターン
  AtLeast -> ado
    pattern <- mkDrawPattern cards count
    in pattern <#> \p -> { card: p.card , min: p.draw, max: p.card.count }
  -- cards の中からちょうど count 枚を引くパターン
  JustDraw -> ado
    pattern <- mkDrawPattern cards count
    let cond = pattern <#> \p -> { card: p.card , min: p.draw, max: p.draw }
    in unionBy ((==) `on` _.card.id) cond $ { card: _, min: 0, max: 0 } <$> cards -- fill others with zero
  -- count 枚以上デッキに残すパターン
  Remains -> ado
    pattern <- mkDrawPattern cards (sumBy _.count cards - count)
    let cond =  pattern <#> \p -> { card: p.card , min: 0, max: p.draw }
    in unionBy ((==) `on` _.card.id) cond $ { card: _, min: 0, max: 0 } <$> cards
  -- ちょうど count 枚デッキに残すパターン
  JustRemains ->
    mkConditionPattern (Condition { mode: JustDraw, count: (sumBy _.count cards - count), cards })
  -- cards の中から count 種類以上を1枚以上引くパターン
  Choice -> ado
    pattern <- mkDrawPattern' cards [replicate count 1]
    in pattern <#> \p -> { card: p.card , min: 1, max: p.card.count }
  -- cards の中から count 種類以上を1枚以上残すパターン
  LeftOne -> ado
    pattern <- mkDrawPattern' cards [replicate count 1]
    in pattern <#> \p -> { card: p.card , min: 0, max: p.card.count - 1 }
  -- cards の中から count 種類以上を1枚も引かないパターン
  LeftAll -> ado
    pattern <- mkDrawPattern' cards [replicate count 1]
    in pattern <#> \p -> { card: p.card , min: 0, max: 0 }

-- カードを指定枚数引く全ての組み合わせを列挙する
mkDrawPattern :: Cards -> Int -> Array DrawPattern
mkDrawPattern cards count = mkDrawPattern' cards $ partitionNumber count

-- 指定の枚数パターンに合致するカードの組み合わせを全て列挙する
mkDrawPattern' :: Cards -> Combination Int -> Array DrawPattern
mkDrawPattern' cards pattern = do
  p <- pattern
  let len = length p
  let c = combinations len cards
  p' <- p # case length (group p) of -- `p` must be sorted
    1            -> pure
    n | n == len -> permutations
    _            -> nubEq <<< permutations
  filter (all \d -> d.draw <= d.card.count) $ zipWith { draw: _, card: _ } p' <$> c

----------------------------------------------------------------

sumBy :: forall a. (a -> Int) -> Array a -> Int
sumBy = alaF Additive foldMap
