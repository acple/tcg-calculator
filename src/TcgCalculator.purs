module TcgCalculator where

import Prelude

import Control.Alternative (empty, guard)
import Data.Array (any, concatMap, filter, findIndex, groupAllBy, mapMaybe, notElem, nub, null, sortBy, updateAt, (!!), (..), (:))
import Data.Array.NonEmpty (foldl1, toArray)
import Data.BigInt (BigInt)
import Data.Foldable (class Foldable, all, and, fold, foldMap, foldr)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (alaF)
import Data.Traversable (mapAccumL)
import Data.Tuple (Tuple(..))
import TcgCalculator.Math (combinationNumber, combinations)
import TcgCalculator.Types (Card, Cards, Condition, ConditionGroup, ConditionMode(..), ConditionSet, Deck, CardId, filterCards)

----------------------------------------------------------------

-- 条件を満たす組み合わせの個数を計算する
calculate :: Deck -> ConditionSet -> BigInt
calculate deck set = countMatchingPatterns deck $ normalizeConditionPatterns $ buildConditionPattern deck.cards =<< set

-- カードを順に走査し、いずれかの ConditionPattern を満たすドローパターンの組み合わせ数を数え上げる
countMatchingPatterns :: Deck -> Array ConditionPattern -> BigInt
countMatchingPatterns deck patterns = do
  let candidates = mapMaybe (toCandidate deck.hand) patterns
  foldr step (leaf deck.others) deck.cards deck.hand candidates
  where

  toCandidate :: Int -> ConditionPattern -> Maybe { remaining :: Int, entries :: ConditionPattern }
  toCandidate hand pattern = do
    let remaining = sumBy _.min pattern
    guard $ remaining <= hand
    pure { remaining, entries: pattern }

  leaf others hand candidates = if null candidates then zero else combinationNumber others hand

  step :: Card -> (Int -> Array _ -> BigInt) -> Int -> Array _ -> BigInt
  step card k hand candidates = 0 .. min card.count hand # sumBy \draw -> do
    let hand' = hand - draw
    let candidates' = mapMaybe (update card.id draw hand') candidates
    if null candidates' then zero else combinationNumber card.count draw * k hand' candidates'

  update cardId draw hand candidate = case Map.lookup cardId candidate.entries of
    Nothing -> pure candidate
    Just { min, max } -> do
      let remaining = candidate.remaining - min
      guard $ min <= draw && draw <= max && remaining <= hand
      pure candidate { remaining = remaining }

-- 指定した条件式で使用していないカードをデッキから取り除く
normalizeDeck :: Deck -> ConditionSet -> Deck
normalizeDeck { cards, others, hand } set = do
  let used = usedCards set
  let cards' = sortBy (comparing _.id) <<< filterCards used $ cards
  let unused = diffCards used cards
  { cards: cards', others: others + sumBy _.count unused, hand }
  where
  usedCards = nub <<< concatMap _.cards <<< concatMap toArray
  diffCards used = filter (_.id >>> notElem <@> used)

-- 確率計算のため、全組み合わせの個数を計算する
calculateTotal :: Deck -> BigInt
calculateTotal { cards, others, hand } = combinationNumber (sumBy _.count cards + others) hand

----------------------------------------------------------------

-- カードとそのドロー可能枚数を組み合わせた一つの条件式を表す
type ConditionPattern = Map CardId { card :: Card, min :: Int, max :: Int }

----------------------------------------------------------------

-- 条件式をマージ (AND) して ConditionPattern のリストに変換する
buildConditionPattern :: Cards -> ConditionGroup -> Array ConditionPattern
buildConditionPattern cards group = do
  let patterns = mkConditionPattern cards <$> group
  foldl1 <@> patterns $ \left right -> do
    l <- left
    r <- right
    let merged = mergeConditionPattern l r
    if isValidConditionPattern merged then pure merged else empty

mergeConditionPattern :: ConditionPattern -> ConditionPattern -> ConditionPattern
mergeConditionPattern = Map.unionWith merge
  where
  merge a b = { card: a.card, min: a.min + b.min, max: min a.max b.max } -- min は交差でなく合算 (各条件のドロー割り当てを同時に確保することを意味する)

isValidConditionPattern :: ConditionPattern -> Boolean
isValidConditionPattern = all \{ card: { count }, min, max } -> min <= max && min <= count

-- 重複する条件を取り除いてパターンを正規化する
normalizeConditionPatterns :: Array ConditionPattern -> Array ConditionPattern
normalizeConditionPatterns = groupAllBy (comparing Map.keys) >=> foldr collect []
  where
  collect condition patterns = if any (include condition) patterns -- より広い条件を既に含む場合は何もしない
    then patterns
    else case findIndex (flip include condition) patterns of -- より広い条件が与えられた場合は上書きする
      Just i -> fold $ updateAt i condition patterns
      Nothing -> condition : patterns
  include = map and <<< Map.intersectionWith \{ min: min1, max: max1 } { min: min2, max: max2 } -> min2 <= min1 && max1 <= max2

----------------------------------------------------------------

-- 一つの Condition に対応する全パターンのリストを出力する
mkConditionPattern :: Cards -> Condition -> Array ConditionPattern
mkConditionPattern cards { mode, count, cards: ids } = do
  let cards' = filterCards ids cards
  toConditionPattern <<< filterCondition <$> case mode of
    -- cards の中から count 枚以上を引くパターン
    AtLeast -> ado
      pattern <- mkDrawPattern count cards'
      in pattern <#> \p -> { card: p.card, min: p.draw, max: p.card.count }
    -- cards の中からちょうど count 枚を引くパターン
    JustDraw -> ado
      pattern <- mkDrawPattern count cards'
      in cards' # mapWithDraw pattern \card draw -> { card, min: draw, max: draw }
    -- count 枚以上デッキに残すパターン
    Remains -> ado
      pattern <- mkDrawPattern (sumBy _.count cards' - count) cards'
      in cards' # mapWithDraw pattern \card draw -> { card, min: 0, max: draw }
    -- ちょうど count 枚デッキに残すパターン
    JustRemains -> ado
      pattern <- mkDrawPattern (sumBy _.count cards' - count) cards'
      in cards' # mapWithDraw pattern \card draw -> { card, min: draw, max: draw }
    -- cards の中から count 種類以上を1枚以上引くパターン
    Choice -> ado
      selected <- combinations count cards'
      in selected <#> \card -> { card, min: 1, max: card.count }
    -- cards の中から count 種類以上を1枚以上残すパターン
    LeftOne -> ado
      selected <- combinations count cards'
      in selected <#> \card -> { card, min: 0, max: card.count - 1 }
    -- cards の中から count 種類以上を1枚も引かないパターン
    LeftAll -> ado
      selected <- combinations count cards'
      in selected <#> \card -> { card, min: 0, max: 0 }
  where
  mapWithDraw pattern f = _.value <<< do
    mapAccumL <@> 0 $ \i card -> case pattern !! i of
      Just p | p.card.id == card.id -> { accum: i + 1, value: f card p.draw }
      _ -> { accum: i, value: f card 0 }
  filterCondition = filter \{ card, min, max } -> not (min == 0 && max == card.count)
  toConditionPattern = Map.fromFoldable <<< map (flip Tuple <*> _.card.id)

----------------------------------------------------------------

-- 各カードをそれぞれ何枚引いているかの状態を表す
type DrawPattern = Array { card :: Card, draw :: Int }

-- カードを指定枚数引く全ての組み合わせを列挙する
-- 結果は cards の順序を保持した部分列の集合となる
mkDrawPattern :: Int -> Cards -> Array DrawPattern
mkDrawPattern count _ | count < 0  = []
mkDrawPattern 0 _ = [[]]
mkDrawPattern count cards = do
  let capacity = sumBy _.count cards
  if capacity < count then [] else foldr step leaf cards count capacity
  where
  leaf _ _ = [[]]
  step :: Card -> (Int -> Int -> Array DrawPattern) -> Int -> Int -> Array DrawPattern
  step _ _ 0 _ = [[]]
  step { count: 0 } k remaining capacity = k remaining capacity
  step card k remaining capacity = do
    let capacity' = capacity - card.count
    let maxDraw = min remaining card.count
    let minDraw = max 1 (remaining - capacity')
    let draws = maxDraw .. minDraw >>= \draw -> ({ card, draw } : _) <$> k (remaining - draw) capacity'
    if capacity' < remaining then draws else draws <> k remaining capacity'

----------------------------------------------------------------

sumBy :: forall f a m. Foldable f => Semiring m => (a -> m) -> f a -> m
sumBy = alaF Additive foldMap
