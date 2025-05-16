module Test.Main where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NE
import Data.BigInt as BigInt
import Data.Foldable (foldMap)
import Data.Function (on)
import Effect (Effect)
import Effect.Aff (Aff)
import TcgCalculator (ConditionPattern, buildConditionPattern, calculate, mergeConditionPattern, mkConditionPattern, normalizeDeck)
import TcgCalculator.Math (combinationNumber, combinations, distinctPermutations, partitionNumber)
import TcgCalculator.Types (Card, Cards, ConditionMode(..))
import TcgCalculator.Types.Id (mkId)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [consoleReporter] do
  mathTest
  tcgCalculatorTest

----------------------------------------------------------------

mathTest :: Spec Unit
mathTest = do
  describe "TcgCalculator.Math" do
    it "combinationNumber" do
      combinationNumberTest
    it "partitionNumber" do
      partitionNumberTest
    it "permutations" do
      permutationsTest
    it "combinations" do
      combinationsTest
  where

  combinationNumberTest = do
    combinationNumber 5 0 `shouldEqual` BigInt.fromInt 1
    combinationNumber 5 1 `shouldEqual` BigInt.fromInt 5
    combinationNumber 5 2 `shouldEqual` BigInt.fromInt 10
    combinationNumber 40 5 `shouldEqual` BigInt.fromInt 658008
    combinationNumber 120 3 `shouldEqual` BigInt.fromInt 280840
    combinationNumber (-1) 0 `shouldEqual` BigInt.fromInt 0
    combinationNumber 5 (-1) `shouldEqual` BigInt.fromInt 0

  partitionNumberTest = do
    partitionNumber 5 `shouldEqual` [[5], [4, 1], [3, 2], [3, 1, 1], [2, 2, 1], [2, 1, 1, 1], [1, 1, 1, 1, 1]]
    partitionNumber 4 `shouldEqual` [[4], [3, 1], [2, 2], [2, 1, 1], [1, 1, 1, 1]]
    partitionNumber 2 `shouldEqual` [[2], [1, 1]]
    partitionNumber 1 `shouldEqual` [[1]]
    partitionNumber 0 `shouldEqual` [[]]
    partitionNumber (-1) `shouldEqual` []

  permutationsTest = do
    distinctPermutations [3, 2, 1] `shouldEqual` [[3, 2, 1], [3, 1, 2], [2, 3, 1], [2, 1, 3], [1, 3, 2], [1, 2, 3]]
    distinctPermutations [3, 1, 1] `shouldEqual` [[3, 1, 1], [1, 3, 1], [1, 1, 3]]
    distinctPermutations [3, 3, 1, 1] `shouldEqual` [[3, 3, 1, 1], [3, 1, 3, 1], [3, 1, 1, 3], [1, 3, 3, 1], [1, 3, 1, 3], [1, 1, 3, 3]]
    distinctPermutations ['b', 'a'] `shouldEqual` [['b', 'a'], ['a', 'b']]
    distinctPermutations ["x"] `shouldEqual` [["x"]]
    distinctPermutations ([] :: Array Unit) `shouldEqual` [[]]

  combinationsTest = do
    combinations 2 [1, 2, 3] `shouldEqual`
      [[1, 2], [1, 3], [2, 3]]
    combinations 2 ['a', 'b', 'c', 'd'] `shouldEqual`
      [['a', 'b'], ['a', 'c'], ['a', 'd'], ['b', 'c'], ['b', 'd'], ['c', 'd']]
    combinations 3 [1, 2, 3, 4, 5] `shouldEqual`
      [[1, 2, 3], [1, 2, 4], [1, 2, 5], [1, 3, 4], [1, 3, 5], [1, 4, 5], [2, 3, 4], [2, 3, 5], [2, 4, 5], [3, 4, 5]]
    combinations 1 [1, 2, 3, 4] `shouldEqual` [[1], [2], [3], [4]]
    combinations 4 [1, 2, 3, 4] `shouldEqual` [[1, 2, 3, 4]]
    combinations 5 [1, 2, 3, 4] `shouldEqual` []
    combinations 0 [1, 2, 3, 4] `shouldEqual` [[]]
    combinations (-1) [1, 2, 3, 4] `shouldEqual` []

----------------------------------------------------------------

tcgCalculatorTest :: Spec Unit
tcgCalculatorTest = do
  describe "TcgCalculator" do
    mkConditionPatternTest
    mergeConditionPatternTest
    buildConditionPatternTest
    calculateTest

mkConditionPatternTest :: Spec Unit
mkConditionPatternTest = do
  describe "mkConditionPattern" do
    it "Condition AtLeast" do
      mkConditionPatternAtLeastTest
    it "Condition JustDraw" do
      mkConditionPatternJustDrawTest
    it "Condition Remains" do
      mkConditionPatternRemainsTest
    it "Condition JustRemains" do
      mkConditionPatternJustRemainsTest
    it "Condition Choice" do
      mkConditionPatternChoiceTest
    it "Condition LeftAll" do
      mkConditionPatternLeftAllTest
  where

  test = mkConditionPattern testCards

  mkConditionPatternAtLeastTest = do
    test { mode: AtLeast, count: 1, cards: [cardA.id] } `conditionEqual` [[{ card: cardA, min: 1, max: 3 }]]
    test { mode: AtLeast, count: 2, cards: [cardA.id] } `conditionEqual` [[{ card: cardA, min: 2, max: 3 }]]
    test { mode: AtLeast, count: 3, cards: [cardA.id] } `conditionEqual` [[{ card: cardA, min: 3, max: 3 }]]
    test { mode: AtLeast, count: 4, cards: [cardA.id] } `conditionEqual` []
    test { mode: AtLeast, count: 1, cards: [cardA.id, cardB.id] } `conditionEqual` [[{ card: cardA, min: 1, max: 3 }], [{ card: cardB, min: 1, max: 2 }]]
    test { mode: AtLeast, count: 2, cards: [cardA.id, cardB.id] } `conditionEqual`
      [ [{ card: cardA, min: 2, max: 3 }]
      , [{ card: cardB, min: 2, max: 2 }]
      , [{ card: cardA, min: 1, max: 3 }, { card: cardB, min: 1, max: 2 }]
      ]
    test { mode: AtLeast, count: 3, cards: [cardA.id, cardB.id] } `conditionEqual`
      [ [{ card: cardA, min: 3, max: 3 }]
      , [{ card: cardA, min: 2, max: 3 }, { card: cardB, min: 1, max: 2 }]
      , [{ card: cardA, min: 1, max: 3 }, { card: cardB, min: 2, max: 2 }]
      ]

  mkConditionPatternJustDrawTest = do
    test { mode: JustDraw, count: 1, cards: [cardA.id] } `conditionEqual` [[{ card: cardA, min: 1, max: 1 }]]
    test { mode: JustDraw, count: 2, cards: [cardA.id] } `conditionEqual` [[{ card: cardA, min: 2, max: 2 }]]
    test { mode: JustDraw, count: 3, cards: [cardA.id] } `conditionEqual` [[{ card: cardA, min: 3, max: 3 }]]
    test { mode: JustDraw, count: 4, cards: [cardA.id] } `conditionEqual` []
    test { mode: JustDraw, count: 1, cards: [cardA.id, cardB.id] } `conditionEqual`
      [ [{ card: cardA, min: 1, max: 1 }, { card: cardB, min: 0, max: 0 }]
      , [{ card: cardA, min: 0, max: 0 }, { card: cardB, min: 1, max: 1 }]
      ]
    test { mode: JustDraw, count: 2, cards: [cardA.id, cardB.id] } `conditionEqual`
      [ [{ card: cardA, min: 2, max: 2 }, { card: cardB, min: 0, max: 0 }]
      , [{ card: cardA, min: 0, max: 0 }, { card: cardB, min: 2, max: 2 }]
      , [{ card: cardA, min: 1, max: 1 }, { card: cardB, min: 1, max: 1 }]
      ]
    test { mode: JustDraw, count: 3, cards: [cardA.id, cardB.id] } `conditionEqual`
      [ [{ card: cardA, min: 3, max: 3 }, { card: cardB, min: 0, max: 0 }] -- [3, 0]
      -- , [{ card: cardA, min: 0, max: 0 }, { card: cardB, min: 3, max: 3 }] -- cardB.count < 3
      , [{ card: cardA, min: 2, max: 2 }, { card: cardB, min: 1, max: 1 }] -- [2, 1]
      , [{ card: cardA, min: 1, max: 1 }, { card: cardB, min: 2, max: 2 }] -- [1, 2]
      ]
    test { mode: JustDraw, count: 3, cards: [cardA.id, cardB.id, cardC.id] } `conditionEqual`
      [ [{ card: cardA, min: 3, max: 3 }, { card: cardB, min: 0, max: 0 }, { card: cardC, min: 0, max: 0 }]
      -- , [{ card: cardA, min: 0, max: 0 }, { card: cardB, min: 3, max: 3 }, { card: cardC, min: 0, max: 0 }] -- cardB.count < 3
      , [{ card: cardA, min: 0, max: 0 }, { card: cardB, min: 0, max: 0 }, { card: cardC, min: 3, max: 3 }]

      , [{ card: cardA, min: 2, max: 2 }, { card: cardB, min: 1, max: 1 }, { card: cardC, min: 0, max: 0 }]
      , [{ card: cardA, min: 2, max: 2 }, { card: cardB, min: 0, max: 0 }, { card: cardC, min: 1, max: 1 }]
      , [{ card: cardA, min: 0, max: 0 }, { card: cardB, min: 2, max: 2 }, { card: cardC, min: 1, max: 1 }]

      , [{ card: cardA, min: 1, max: 1 }, { card: cardB, min: 2, max: 2 }, { card: cardC, min: 0, max: 0 }]
      , [{ card: cardA, min: 1, max: 1 }, { card: cardB, min: 0, max: 0 }, { card: cardC, min: 2, max: 2 }]
      , [{ card: cardA, min: 0, max: 0 }, { card: cardB, min: 1, max: 1 }, { card: cardC, min: 2, max: 2 }]

      , [{ card: cardA, min: 1, max: 1 }, { card: cardB, min: 1, max: 1 }, { card: cardC, min: 1, max: 1 }]
      ]

  mkConditionPatternRemainsTest = do
    test { mode: Remains, count: 1, cards: [cardA.id] } `conditionEqual` [[{ card: cardA, min: 0, max: 2 }]]
    test { mode: Remains, count: 2, cards: [cardA.id] } `conditionEqual` [[{ card: cardA, min: 0, max: 1 }]]
    test { mode: Remains, count: 3, cards: [cardA.id] } `conditionEqual` [[{ card: cardA, min: 0, max: 0 }]]
    test { mode: Remains, count: 4, cards: [cardA.id] } `conditionEqual` []
    test { mode: Remains, count: 1, cards: [cardA.id, cardB.id] } `conditionEqual` [[{ card: cardA, min: 0, max: 2 }], [{ card: cardB, min: 0, max: 1 }]]
    test { mode: Remains, count: 2, cards: [cardA.id, cardB.id] } `conditionEqual`
      [ [{ card: cardA, min: 0, max: 1 }] -- [2, (0)]
      , [{ card: cardB, min: 0, max: 0 }] -- [(0), 2]
      , [{ card: cardA, min: 0, max: 2 }, { card: cardB, min: 0, max: 1 }] -- [1, 1]
      ]
    test { mode: Remains, count: 3, cards: [cardA.id, cardB.id, cardC.id] } `conditionEqual`
      [ [{ card: cardA, min: 0, max: 0 }] -- [3, (0), (0)]
      , [{ card: cardC, min: 0, max: 0 }] -- [(0), (0), 3]
      , [{ card: cardA, min: 0, max: 1 }, { card: cardB, min: 0, max: 1 }] -- [2, 1, (0)]
      , [{ card: cardA, min: 0, max: 1 }, { card: cardC, min: 0, max: 2 }] -- [2, (0), 1]
      , [{ card: cardB, min: 0, max: 0 }, { card: cardC, min: 0, max: 2 }] -- [(0), 2, 1]
      , [{ card: cardA, min: 0, max: 2 }, { card: cardB, min: 0, max: 0 }] -- [1, 2, (0)]
      , [{ card: cardA, min: 0, max: 2 }, { card: cardC, min: 0, max: 1 }] -- [1, (0), 2]
      , [{ card: cardB, min: 0, max: 1 }, { card: cardC, min: 0, max: 1 }] -- [(0), 1, 2]
      , [{ card: cardA, min: 0, max: 2 }, { card: cardB, min: 0, max: 1 }, { card: cardC, min: 0, max: 2 }] -- [1, 1, 1]
      ]

  mkConditionPatternJustRemainsTest = do
    -- Condition JustRemains n cards == Condition JustDraw (cardCount - n) cards
    test { mode: JustRemains, count: 1, cards: [cardA.id] } `conditionEqual` [[{ card: cardA, min: 2, max: 2 }]]
    test { mode: JustRemains, count: 2, cards: [cardA.id] } `conditionEqual` [[{ card: cardA, min: 1, max: 1 }]]
    test { mode: JustRemains, count: 3, cards: [cardA.id] } `conditionEqual` [[{ card: cardA, min: 0, max: 0 }]]
    test { mode: JustRemains, count: 4, cards: [cardA.id] } `conditionEqual` []
    test { mode: JustRemains, count: 2, cards: [cardA.id, cardB.id] } `conditionEqual`
      [ [{ card: cardA, min: 3, max: 3 }, { card: cardB, min: 0, max: 0 }]
      , [{ card: cardA, min: 2, max: 2 }, { card: cardB, min: 1, max: 1 }]
      , [{ card: cardA, min: 1, max: 1 }, { card: cardB, min: 2, max: 2 }]
      ]

  mkConditionPatternChoiceTest = do
    test { mode: Choice, count: 1, cards: [cardA.id] } `conditionEqual` [[{ card: cardA, min: 1, max: 3 }]]
    test { mode: Choice, count: 1, cards: [cardA.id, cardB.id] } `conditionEqual` [[{ card: cardA, min: 1, max: 3 }], [{ card: cardB, min: 1, max: 2 }]]
    test { mode: Choice, count: 2, cards: [cardA.id, cardB.id] } `conditionEqual` [[{ card: cardA, min: 1, max: 3 }, { card: cardB, min: 1, max: 2 }]]
    test { mode: Choice, count: 1, cards: [cardA.id, cardB.id, cardC.id] } `conditionEqual`
      [ [{ card: cardA, min: 1, max: 3 }]
      , [{ card: cardB, min: 1, max: 2 }]
      , [{ card: cardC, min: 1, max: 3 }]
      ]
    test { mode: Choice, count: 2, cards: [cardA.id, cardB.id, cardC.id] } `conditionEqual`
      [ [{ card: cardA, min: 1, max: 3 }, { card: cardB, min: 1, max: 2 }]
      , [{ card: cardA, min: 1, max: 3 }, { card: cardC, min: 1, max: 3 }]
      , [{ card: cardB, min: 1, max: 2 }, { card: cardC, min: 1, max: 3 }]
      ]

  mkConditionPatternLeftAllTest = do
    test { mode: LeftAll, count: 1, cards: [cardA.id] } `conditionEqual` [[{ card: cardA, min: 0, max: 0 }]]
    test { mode: LeftAll, count: 1, cards: [cardA.id, cardB.id] } `conditionEqual` [[{ card: cardA, min: 0, max: 0 }], [{ card: cardB, min: 0, max: 0 }]]
    test { mode: LeftAll, count: 2, cards: [cardA.id, cardB.id] } `conditionEqual` [[{ card: cardA, min: 0, max: 0 }, { card: cardB, min: 0, max: 0 }]]
    test { mode: LeftAll, count: 1, cards: [cardA.id, cardB.id, cardC.id] } `conditionEqual`
      [ [{ card: cardA, min: 0, max: 0 }]
      , [{ card: cardB, min: 0, max: 0 }]
      , [{ card: cardC, min: 0, max: 0 }]
      ]
    test { mode: LeftAll, count: 2, cards: [cardA.id, cardB.id, cardC.id] } `conditionEqual`
      [ [{ card: cardA, min: 0, max: 0 }, { card: cardB, min: 0, max: 0 }]
      , [{ card: cardA, min: 0, max: 0 }, { card: cardC, min: 0, max: 0 }]
      , [{ card: cardB, min: 0, max: 0 }, { card: cardC, min: 0, max: 0 }]
      ]

mergeConditionPatternTest :: Spec Unit
mergeConditionPatternTest = do
  describe "mergeConditionPattern" do
    it "empty" do
      mergeConditionPattern [] [] `shouldEqual` []
    it "merges many condition" do
      mergeConditionPattern
        [{ card: cardA, min: 1, max: 3 }] [{ card: cardB, min: 1, max: 2 }] `shouldEqual`
        sort [{ card: cardA, min: 1, max: 3 }, { card: cardB, min: 1, max: 2 }]
      mergeConditionPattern
        [{ card: cardA, min: 1, max: 3 }] [{ card: cardB, min: 1, max: 2 }, { card: cardC, min: 1, max : 3 }] `shouldEqual`
        sort [{ card: cardA, min: 1, max: 3 }, { card: cardB, min: 1, max: 2 }, { card: cardC, min: 1, max: 3 }]
    it "merges same condition" do
      mergeConditionPattern
        [{ card: cardA, min: 1, max: 3 }] [{ card: cardA, min: 1, max: 3 }] `shouldEqual`
        sort [{ card: cardA, min: 2, max: 3 }]
      mergeConditionPattern
        [{ card: cardA, min: 1, max: 3 }] [{ card: cardA, min: 1, max: 2 }] `shouldEqual`
        sort [{ card: cardA, min: 2, max: 2 }]
      mergeConditionPattern
        [{ card: cardA, min: 2, max: 3 }] [{ card: cardA, min: 2, max: 3 }] `shouldEqual`
        sort [{ card: cardA, min: 4, max: 3 }] -- invalid condition by max < min
    it "merges multiple condition" do
      mergeConditionPattern
        [{ card: cardA, min: 1, max: 3 }, { card: cardB, min: 1, max: 2 }]
        [{ card: cardA, min: 1, max: 3 }, { card: cardC, min: 1, max: 3 }] `shouldEqual`
        sort [{ card: cardA, min: 2, max: 3 }, { card: cardB, min: 1, max: 2 }, { card: cardC, min: 1, max: 3 }]
  where
  sort = Array.sortBy $ comparing _.card.id

buildConditionPatternTest :: Spec Unit
buildConditionPatternTest = do
  describe "buildConditionPattern" do
    it "build normal" do
      test [{ mode: AtLeast, count: 1, cards: [cardA.id] }] `conditionEqual` [[{ card: cardA, min: 1, max: 3 }]]
      test [{ mode: JustDraw, count: 2, cards: [cardA.id] }] `conditionEqual` [[{ card: cardA, min: 2, max: 2 }]]
      test [{ mode: Remains, count: 2, cards: [cardA.id] }] `conditionEqual` [[{ card: cardA, min: 0, max: 1 }]]
    it "build with multiple conditions" do
      test [{ mode: AtLeast, count: 1, cards: [cardA.id] }, { mode: AtLeast, count: 1, cards: [cardB.id] }] `conditionEqual`
        [[{ card: cardA, min: 1, max: 3 }, { card: cardB, min: 1, max: 2 }]]
      test [{ mode: JustDraw, count: 1, cards: [cardA.id] }, { mode: AtLeast, count: 1, cards: [cardA.id] }] `conditionEqual`
        [] -- invalid condition
      test [{ mode: Remains, count: 2, cards: [cardA.id] }, { mode: JustDraw, count: 2, cards: [cardC.id] }] `conditionEqual`
        [[{ card: cardA, min: 0, max: 1 }, { card: cardC, min: 2, max: 2 }]]
    it "expanded pattern" do
      test [{ mode: Choice, count: 1, cards: [cardA.id, cardB.id, cardC.id] }] `conditionEqual`
        [ [{ card: cardA, min: 1, max: 3 }]
        , [{ card: cardB, min: 1, max: 2 }]
        , [{ card: cardC, min: 1, max: 3 }]
        ]
      test [{ mode: Choice, count: 1, cards: [cardA.id, cardB.id] }, { mode: AtLeast, count: 2, cards: [cardC.id] }] `conditionEqual`
        [ [{ card: cardA, min: 1, max: 3 }, { card: cardC, min: 2, max: 3 }]
        , [{ card: cardB, min: 1, max: 2 }, { card: cardC, min: 2, max: 3 }]
        ]
    it "complicated pattern" do
      test [{ mode: Choice, count: 1, cards: [cardA.id, cardB.id] }, { mode: Choice, count: 2, cards: [cardB.id, cardC.id, cardD.id] }] `conditionEqual`
        [ [{ card: cardA, min: 1, max: 3 }, { card: cardB, min: 1, max: 2 }, { card: cardC, min: 1, max: 3 }] -- A + BC
        , [{ card: cardA, min: 1, max: 3 }, { card: cardB, min: 1, max: 2 }, { card: cardD, min: 1, max: 1 }] -- A + BD
        , [{ card: cardA, min: 1, max: 3 }, { card: cardC, min: 1, max: 3 }, { card: cardD, min: 1, max: 1 }] -- A + CD
        , [{ card: cardB, min: 1 + 1, max: 2 }, { card: cardC, min: 1, max: 3 }] -- B + BC
        , [{ card: cardB, min: 1 + 1, max: 2 }, { card: cardD, min: 1, max: 1 }] -- B + BD
        , [{ card: cardB, min: 1, max: 2 }, { card: cardC, min: 1, max: 3 }, { card: cardD, min: 1, max: 1 }] -- B + CD
        ]
      test [{ mode: AtLeast, count: 1, cards: [cardD.id] }, { mode: Choice, count: 2, cards: [cardB.id, cardC.id, cardD.id] }] `conditionEqual`
        [ [{ card: cardD, min: 1, max: 1 }, { card: cardB, min: 1, max: 2 }, { card: cardC, min: 1, max: 3 }] -- D + BC
        -- , [{ card: cardD, min: 1 + 1, max: 1 }, { card: cardB, min: 1, max: 2 }] -- D + BD invalid min < max
        -- , [{ card: cardD, min: 1 + 1, max: 1 }, { card: cardC, min: 1, max: 3 }] -- D + CD invalid min < max
        ]
  where
  test = foldMap (buildConditionPattern testCards) <<< NE.fromArray

conditionEqual :: Array ConditionPattern -> Array ConditionPattern -> Aff Unit
conditionEqual = shouldEqual `on` (Array.sort <<< map Array.sort)

calculateTest :: Spec Unit
calculateTest = do
  describe "calculate" do
    it "check some results" do
      let deck = { cards: testCards, others: 11, hand: 5 }
      let cond01 = buildCondition [[{ cards: [cardA.id], count: 1, mode: AtLeast }]]
      test deck cond01 9316
      test deck { others = 0 } cond01 120
      let cond02 = buildCondition [[{ cards: [cardA.id], count: 1, mode: JustDraw }]]
      test deck cond02 7140
      test deck { others = 0 } cond02 45
      let cond03 = buildCondition [[{ cards: [cardA.id, cardB.id, cardC.id], count: 5, mode: AtLeast }]]
      test deck cond03 56
      test deck { others = 0 } cond03 56
      let cond04 = buildCondition [[{ cards: [cardA.id, cardB.id, cardC.id], count: 2, mode: Choice }]]
      test deck cond04 9080
      test deck { others = 0 } cond04 126
      let cond05 = buildCondition [[{ cards: [cardB.id, cardC.id, cardD.id], count: 4, mode: JustDraw }]]
      test deck cond05 210
      test deck { others = 0 } cond05 45
      let cond06 = buildCondition [[{ cards: [cardA.id], count: 1, mode: AtLeast }, { cards: [cardB.id], count: 1, mode: AtLeast }]]
      test deck cond06 3751
      test deck { others = 0 } cond06 99
      let cond07 = buildCondition [[{ cards: [cardA.id], count: 2, mode: JustDraw }, { cards: [cardB.id], count: 1, mode: AtLeast }]]
      test deck cond07 675
      test deck { others = 0 } cond07 48
      let cond08 = buildCondition [[{ cards: [cardD.id], count: 1, mode: AtLeast }, { cards: [cardA.id, cardB.id, cardC.id, cardD.id], count: 2, mode: Choice }]]
      test deck cond08 1819
      test deck { others = 0 } cond08 70
      let cond09 = buildCondition [[{ cards: [cardD.id], count: 1, mode: AtLeast }, { cards: [cardA.id, cardB.id, cardC.id], count: 2, mode: Choice }]]
      test deck cond09 1819
      test deck { others = 0 } cond09 70
      let cond10 = buildCondition [[{ cards: [cardA.id], count: 0, mode: JustDraw }]]
      let deck10 = { cards: [cardA, cardB], others: 5, hand: 5 }
      test deck10 cond10 21
      test deck10 { others = 2 } cond10 0
      let cond11 = buildCondition [[{ cards: [cardA.id, cardB.id, cardC.id, cardD.id], count: 3, mode: Choice }]]
      test deck cond11 3352
      test deck { others = 0 } cond11 118
      let cond12 = buildCondition [[{ cards: [cardA.id, cardB.id], count: 1, mode: JustDraw }, { cards: [cardC.id, cardD.id], count: 1, mode: JustDraw }]]
      test deck cond12 3300
      test deck { others = 0 } cond12 0
      let cond13 = buildCondition [[{ cards: [cardA.id, cardB.id], count: 1, mode: JustDraw }], [{ cards: [cardC.id, cardD.id], count: 1, mode: JustDraw }]]
      test deck cond13 10805
      test deck { others = 0 } cond13 25
      let cardE = { id: mkId "eee", name: "E", count: 0 }
      let cond14 = buildCondition [[{ cards: [cardE.id], count: 0, mode: LeftAll }]]
      let deck14 = deck { cards = [cardE] }
      test deck14 cond14 462
      let deck15 = deck { cards = deck.cards <> [cardE] }
      let cond15 = buildCondition [[{ cards: [cardA.id, cardB.id, cardE.id], count: 1, mode: LeftAll }]]
      test deck15 cond15 15504
      let deck16 = deck15
      let cond16 = buildCondition [[{ cards: [cardA.id, cardB.id, cardE.id], count: 2, mode: LeftAll }]]
      test deck16 cond16 11753
  where
  test deck cond expected = calculate (normalizeDeck deck cond) cond `shouldEqual` BigInt.fromInt expected
  buildCondition = Array.mapMaybe NE.fromArray

----------------------------------------------------------------

testCards :: Cards
testCards = [cardA, cardB, cardC, cardD]

cardA :: Card
cardA = { id: mkId "aaa", name: "A", count: 3 }
cardB :: Card
cardB = { id: mkId "bbb", name: "B", count: 2 }
cardC :: Card
cardC = { id: mkId "ccc", name: "C", count: 3 }
cardD :: Card
cardD = { id: mkId "ddd", name: "D", count: 1 }
