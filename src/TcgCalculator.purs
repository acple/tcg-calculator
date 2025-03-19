module TcgCalculator where

import Prelude

import Control.Alternative (empty)
import Data.Array (all, any, concatMap, deleteBy, filter, find, findIndex, foldMap, groupAllBy, length, nub, nubBy, replicate, sortBy, updateAt, zipWith, (..), (:))
import Data.Array.NonEmpty (foldl1, toArray)
import Data.BigInt (BigInt)
import Data.Foldable (and, fold, foldr, product)
import Data.Function (on)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (alaF)
import Data.Tuple (Tuple(..))
import TcgCalculator.Math (combinationNumber, combinations, distinctPermutations, partitionNumber)
import TcgCalculator.Types (Card, Cards, Condition, ConditionGroup, ConditionMode(..), ConditionSet, Deck)

----------------------------------------------------------------

-- 条件を満たす組み合わせの個数を計算する
calculate :: Deck -> ConditionSet -> BigInt
calculate deck set = do
  let drawPattern = generateDrawPatterns deck
  let conditionPatterns = normalizeConditionPatterns $ buildConditionPattern =<< set
  let pattern = filter (\dp -> any (satisfyCondition dp) conditionPatterns) drawPattern
  sumBy (calculatePatternCount deck) pattern

-- 指定した条件式で使用していないカードをデッキから取り除く
normalizeDeck :: Deck -> ConditionSet -> Deck
normalizeDeck deck set = do
  let used = usedCards set
  let unused = diffCards deck.cards used
  deck { cards = used, others = deck.others + sumBy _.count unused }
  where
  usedCards = nubBy (comparing _.id) <<< concatMap _.cards <<< concatMap toArray
  diffCards = foldr $ deleteBy (eq `on` _.id)

-- 確率計算のため、全組み合わせの個数を計算する
calculateTotal :: Deck -> BigInt
calculateTotal { cards, others, hand } = combinationNumber (sumBy _.count cards + others) hand

----------------------------------------------------------------

-- 各カードをそれぞれ何枚引いているかの状態を表す
type DrawPattern = Array { card :: Card, draw :: Int }

-- Deck から再現可能な全ての手札の組み合わせを列挙する
generateDrawPatterns :: Deck -> Array DrawPattern
generateDrawPatterns { cards, others, hand } = do
  let maxDrawCount = min hand (sumBy _.count cards)
  let minDrawCount = max 0 (hand - others) -- others が hand より少ない場合に成り立たないパターンは予めフィルタする
  mkDrawPattern cards =<< maxDrawCount .. minDrawCount

-- 与えた DrawPattern にマッチする組み合わせの個数を返す
calculatePatternCount :: Deck -> DrawPattern -> BigInt
calculatePatternCount { others, hand } pattern = do
  let patternCount = product $ pattern <#> \{ card: { count }, draw } -> combinationNumber count draw -- 条件のカードを引くときの組み合わせの数
  patternCount * combinationNumber others (hand - sumBy _.draw pattern) -- 残りの手札に条件外のカードを引く組み合わせの数

----------------------------------------------------------------

-- カードとそのドロー可能枚数を組み合わせた一つの条件式を表す
-- 暗黙的に card.id で昇順にソートされていることを期待する
type ConditionPattern = Array { card :: Card, min :: Int, max :: Int }

satisfyCondition :: DrawPattern -> ConditionPattern -> Boolean
satisfyCondition dp = all \{ card: { id }, min, max } -> do
  let draw = maybe 0 _.draw $ find (_.card.id >>> (_ == id)) dp
  min <= draw && draw <= max

-- 条件式をマージ (AND) して ConditionPattern のリストに変換する
buildConditionPattern :: ConditionGroup -> Array ConditionPattern
buildConditionPattern group = do
  let patterns = mkConditionPattern <$> group
  foldl1 <@> patterns $ \left right -> do
    l <- left
    r <- right
    let merged = mergeConditionPattern l r
    if isValidConditionPattern merged then pure merged else empty

mergeConditionPattern :: ConditionPattern -> ConditionPattern -> ConditionPattern
mergeConditionPattern left right = foldl1 merge <$> groupAllBy (comparing _.card.id) (left <> right)
  where
  merge { card, min: min1, max: max1 } { min: min2, max: max2 } = { card, min: min1 + min2, max: min max1 max2 }

isValidConditionPattern :: ConditionPattern -> Boolean
isValidConditionPattern = all \{ card: { count }, min, max } -> min <= max && min <= count

-- 重複する条件を取り除いてパターンを正規化する
normalizeConditionPatterns :: Array ConditionPattern -> Array ConditionPattern
normalizeConditionPatterns = groupAllBy (comparing $ map _.card.id) >=> foldr collect []
  where
  collect condition patterns = if any (include condition) patterns -- より広い条件を既に含む場合は何もしない
    then patterns
    else case findIndex (flip include condition) patterns of -- より広い条件が与えられた場合は上書きする
      Just i -> fold $ updateAt i condition patterns
      Nothing -> condition : patterns
  include = map and <<< zipWith \{ min: min1, max: max1 } { min: min2, max: max2 } -> min2 <= min1 && max1 <= max2

-- 一つの Condition に対応する全パターンのリストを出力する
mkConditionPattern :: Condition -> Array ConditionPattern
mkConditionPattern { mode, count, cards } = do
  let cards' = sortBy (comparing _.id) cards
  case mode of
    -- cards の中から count 枚以上を引くパターン
    AtLeast -> ado
      pattern <- mkDrawPattern cards' count
      in pattern <#> \p -> { card: p.card, min: p.draw, max: p.card.count }
    -- cards の中からちょうど count 枚を引くパターン
    JustDraw -> ado
      pattern <- mkDrawPattern cards' count
      in cards' <#> \card -> do
        let draw = maybe 0 _.draw $ find (_.card.id >>> (_ == card.id)) pattern
        { card, min: draw, max: draw }
    -- count 枚以上デッキに残すパターン
    Remains -> ado
      pattern <- mkDrawPattern cards' (sumBy _.count cards' - count)
      in cards' <#> \card -> do
        let draw = maybe 0 _.draw $ find (_.card.id >>> (_ == card.id)) pattern
        { card, min: 0, max: draw }
    -- ちょうど count 枚デッキに残すパターン
    JustRemains ->
      mkConditionPattern { mode: JustDraw, count: (sumBy _.count cards' - count), cards: cards' }
    -- cards の中から count 種類以上を1枚以上引くパターン
    Choice -> ado
      pattern <- mkDrawPattern' cards' [replicate count 1]
      in pattern <#> \p -> { card: p.card, min: 1, max: p.card.count }
    -- cards の中から count 種類以上を1枚以上残すパターン
    LeftOne -> ado
      pattern <- mkDrawPattern' cards' [replicate count 1]
      in pattern <#> \p -> { card: p.card, min: 0, max: p.card.count - 1 }
    -- cards の中から count 種類以上を1枚も引かないパターン
    LeftAll -> ado
      pattern <- mkDrawPattern' cards' [replicate count 0]
      in pattern <#> \p -> { card: p.card, min: 0, max: 0 }

-- カードを指定枚数引く全ての組み合わせを列挙する
mkDrawPattern :: Cards -> Int -> Array DrawPattern
mkDrawPattern cards count = mkDrawPattern' cards $ partitionNumber count

-- 指定の枚数パターンに合致するカードの組み合わせを全て列挙する
-- 引数 cards の順序は維持される
-- 引数 pattern の各要素は予め降順にソートされている必要がある
mkDrawPattern' :: Cards -> Array (Array Int) -> Array DrawPattern
mkDrawPattern' cards pattern = do
  let cardsLength = length cards
  let cardCounts = sortBy (flip compare) $ _.count <$> cards
  let pattern' = filter (length >>> (_ <= cardsLength) && and <<< zipWith (>=) cardCounts) pattern
  let patternLength = nub $ length <$> pattern'
  let cardCombinations = Map.fromFoldable $ Tuple <*> flip combinations cards <$> patternLength
  p <- pattern'
  let cardCombination = fold $ Map.lookup (length p) cardCombinations
  p' <- distinctPermutations p
  filter (all \d -> d.draw <= d.card.count) $ zipWith { draw: _, card: _ } p' <$> cardCombination

----------------------------------------------------------------

sumBy :: forall a m. Semiring m => (a -> m) -> Array a -> m
sumBy = alaF Additive foldMap
