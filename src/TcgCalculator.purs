module TcgCalculator where

import Prelude

import Control.Alternative (empty)
import Data.Array (all, any, concat, concatMap, deleteBy, filter, find, foldMap, foldr, groupAll, groupAllBy, length, nubByEq, nubEq, replicate, take, unionBy, zipWith, (!!), (..))
import Data.Array.NonEmpty (NonEmptyArray, foldl1, toArray)
import Data.BigInt (BigInt)
import Data.Foldable (fold, maximum, product, sum)
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (alaF, unwrap)
import TcgCalculator.Math (Combination, combinationNumber, combinations, partitionNumber, partitionNumbers, permutations)
import TcgCalculator.Types (Card, Cards, Condition(..), ConditionMode(..), Deck)

----------------------------------------------------------------

calculate :: Deck -> Array (NonEmptyArray Condition) -> BigInt
calculate deck conditions = do
  let drawPattern = generateDrawPatterns deck
  let conditionPattern = nubEq $ buildConditionPattern =<< conditions
  let pattern = filter (\dp -> any (satisfyCondition dp) conditionPattern) drawPattern
  sum $ calculatePatternCount deck <$> pattern

normalizeDeck :: Deck -> Array (NonEmptyArray Condition) -> Deck
normalizeDeck deck conditions = do
  let used = usedCards conditions
  let unused = diffCards deck.cards used
  deck { cards = used, others = deck.others + sumBy _.count unused }
  where
  usedCards = nubByEq ((==) `on` _.id) <<< concatMap (_.cards <<< unwrap) <<< concatMap toArray
  diffCards = foldr $ deleteBy ((==) `on` _.id)

calculateTotal :: Deck -> BigInt
calculateTotal { cards, others, hand } = combinationNumber (sumBy _.count cards + others) hand

----------------------------------------------------------------

-- 各カードをそれぞれ何枚引いているかの状態を表す
type DrawPattern = Array { card :: Card, draw :: Int }

generateDrawPatterns :: Deck -> Array DrawPattern
generateDrawPatterns { cards, others, hand } = do
  let zeroDrawPattern = { card: _, draw: 0 } <$> cards
  let maxDrawCount = min hand (sumBy _.count cards)
  let maxPatternLength = others - (hand - maxDrawCount) + 1
  ado
    drawPattern <- mkDrawPattern' cards <<< concat <<< take maxPatternLength $ partitionNumbers maxDrawCount
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
satisfyCondition dp = all \{ card: { id }, min, max } -> fromMaybe false ado
  { draw } <- find (_.card.id >>> (_ == id)) dp
  in min <= draw && draw <= max

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
mergeConditionPattern left right = foldl1 merge <$> groupAllBy (comparing _.card.id) (left <> right)
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
    in pattern <#> \p -> { card: p.card, min: p.draw, max: p.card.count }
  -- cards の中からちょうど count 枚を引くパターン
  JustDraw -> ado
    pattern <- mkDrawPattern cards count
    let cond = pattern <#> \p -> { card: p.card, min: p.draw, max: p.draw }
    in unionBy ((==) `on` _.card.id) cond $ { card: _, min: 0, max: 0 } <$> cards -- fill others with zero
  -- count 枚以上デッキに残すパターン
  Remains -> ado
    pattern <- mkDrawPattern cards (sumBy _.count cards - count)
    let cond =  pattern <#> \p -> { card: p.card, min: 0, max: p.draw }
    in unionBy ((==) `on` _.card.id) cond $ { card: _, min: 0, max: 0 } <$> cards
  -- ちょうど count 枚デッキに残すパターン
  JustRemains ->
    mkConditionPattern (Condition { mode: JustDraw, count: (sumBy _.count cards - count), cards })
  -- cards の中から count 種類以上を1枚以上引くパターン
  Choice -> ado
    pattern <- mkDrawPattern' cards [replicate count 1]
    in pattern <#> \p -> { card: p.card, min: 1, max: p.card.count }
  -- cards の中から count 種類以上を1枚以上残すパターン
  LeftOne -> ado
    pattern <- mkDrawPattern' cards [replicate count 1]
    in pattern <#> \p -> { card: p.card, min: 0, max: p.card.count - 1 }
  -- cards の中から count 種類以上を1枚も引かないパターン
  LeftAll -> ado
    pattern <- mkDrawPattern' cards [replicate count 1]
    in pattern <#> \p -> { card: p.card, min: 0, max: 0 }

-- カードを指定枚数引く全ての組み合わせを列挙する
mkDrawPattern :: Cards -> Int -> Array DrawPattern
mkDrawPattern cards count = mkDrawPattern' cards $ partitionNumber count

-- 指定の枚数パターンに合致するカードの組み合わせを全て列挙する
mkDrawPattern' :: Cards -> Combination Int -> Array DrawPattern
mkDrawPattern' _ [] = []
mkDrawPattern' _ [[]] = [[]]
mkDrawPattern' cards pattern = do
  let cardsLength = length cards
  let pattern' = filter (length >>> (_ <= cardsLength)) pattern
  let maxPatternLength = min cardsLength (fromMaybe 0 <<< maximum $ length <$> pattern')
  let cardCombinations = combinations <@> cards <$> 0 .. maxPatternLength
  p <- pattern'
  let len = length p
  let con = fold $ cardCombinations !! len
  p' <- p # case length (groupAll p) of
    1            -> pure
    n | n == len -> permutations
    _            -> nubEq <<< permutations
  filter (all \d -> d.draw <= d.card.count) $ zipWith { draw: _, card: _ } p' <$> con

----------------------------------------------------------------

sumBy :: forall a. (a -> Int) -> Array a -> Int
sumBy = alaF Additive foldMap
