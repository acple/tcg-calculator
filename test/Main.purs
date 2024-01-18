module Test.Main where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NE
import Data.BigInt as BigInt
import Effect (Effect)
import Effect.Aff (launchAff_)
import TcgCalculator (buildConditionPattern, calculate, mergeConditionPattern, mkConditionPattern, normalizeDeck)
import TcgCalculator.Math (combinationNumber, combinations, partitionNumber, permutations)
import TcgCalculator.Types (Card, Cards, Condition(..), ConditionMode(..), mkId)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
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

  partitionNumberTest = do
    partitionNumber 4 `shouldEqual` [[4], [3, 1], [2, 2], [2, 1, 1], [1, 1, 1, 1]]
    partitionNumber 2 `shouldEqual` [[2], [1, 1]]
    partitionNumber 1 `shouldEqual` [[1]]
    partitionNumber 0 `shouldEqual` [[]]

  permutationsTest = do
    permutations [1, 2, 3] `shouldEqual` [[1, 2, 3], [2, 1, 3], [2, 3, 1], [1, 3, 2], [3, 1, 2], [3, 2, 1]]
    permutations ['b', 'a'] `shouldEqual` [['b', 'a'], ['a', 'b']]
    permutations ["x"] `shouldEqual` [["x"]]
    permutations ([] :: Array Unit) `shouldEqual` [[]]

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
  where

  mkConditionPatternAtLeastTest = do
    mkConditionPattern (Condition { mode: AtLeast, count: 1, cards: [cardA] }) `shouldEqual` [[{ card: cardA, min: 1, max: 3 }]]
    mkConditionPattern (Condition { mode: AtLeast, count: 2, cards: [cardA] }) `shouldEqual` [[{ card: cardA, min: 2, max: 3 }]]
    mkConditionPattern (Condition { mode: AtLeast, count: 4, cards: [cardA] }) `shouldEqual` []
    mkConditionPattern (Condition { mode: AtLeast, count: 1, cards: [cardA, cardB] }) `shouldEqual` [[{ card: cardA, min: 1, max: 3 }], [{ card: cardB, min: 1, max: 2 }]]
    mkConditionPattern (Condition { mode: AtLeast, count: 2, cards: [cardA, cardB] }) `shouldEqual`
      [ [{ card: cardA, min: 2, max: 3 }]
      , [{ card: cardB, min: 2, max: 2 }]
      , [{ card: cardA, min: 1, max: 3 }, { card: cardB, min: 1, max: 2 }]
      ]
    mkConditionPattern (Condition { mode: AtLeast, count: 3, cards: [cardA, cardB] }) `shouldEqual`
      [ [{ card: cardA, min: 3, max: 3 }]
      , [{ card: cardA, min: 2, max: 3 }, { card: cardB, min: 1, max: 2 }]
      , [{ card: cardA, min: 1, max: 3 }, { card: cardB, min: 2, max: 2 }]
      ]

  mkConditionPatternJustDrawTest = do
    mkConditionPattern (Condition { mode: JustDraw, count: 1, cards: [cardA] }) `shouldEqual` [[{ card: cardA, min: 1, max: 1 }]]
    mkConditionPattern (Condition { mode: JustDraw, count: 2, cards: [cardA] }) `shouldEqual` [[{ card: cardA, min: 2, max: 2 }]]
    mkConditionPattern (Condition { mode: JustDraw, count: 4, cards: [cardA] }) `shouldEqual` []
    mkConditionPattern (Condition { mode: JustDraw, count: 1, cards: [cardA, cardB] }) `shouldEqual`
      [ [{ card: cardA, min: 1, max: 1 }, { card: cardB, min: 0, max: 0 }]
      , [{ card: cardA, min: 0, max: 0 }, { card: cardB, min: 1, max: 1 }]
      ]
    mkConditionPattern (Condition { mode: JustDraw, count: 2, cards: [cardA, cardB] }) `shouldEqual`
      [ [{ card: cardA, min: 2, max: 2 }, { card: cardB, min: 0, max: 0 }]
      , [{ card: cardA, min: 0, max: 0 }, { card: cardB, min: 2, max: 2 }]
      , [{ card: cardA, min: 1, max: 1 }, { card: cardB, min: 1, max: 1 }]
      ]
    mkConditionPattern (Condition { mode: JustDraw, count: 3, cards: [cardA, cardB] }) `shouldEqual`
      [ [{ card: cardA, min: 3, max: 3 }, { card: cardB, min: 0, max: 0 }] -- [3, 0]
      , [{ card: cardA, min: 2, max: 2 }, { card: cardB, min: 1, max: 1 }] -- [2, 1]
      , [{ card: cardA, min: 1, max: 1 }, { card: cardB, min: 2, max: 2 }] -- [1, 2]
      ]
    mkConditionPattern (Condition { mode: JustDraw, count: 3, cards: [cardA, cardB, cardC] }) `shouldEqual`
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
    mkConditionPattern (Condition { mode: Remains, count: 1, cards: [cardA] }) `shouldEqual` [[{ card: cardA, min: 0, max: 3 - 1 }]]
    mkConditionPattern (Condition { mode: Remains, count: 2, cards: [cardA] }) `shouldEqual` [[{ card: cardA, min: 0, max: 3 - 2 }]]
    mkConditionPattern (Condition { mode: Remains, count: 4, cards: [cardA] }) `shouldEqual` []
    mkConditionPattern (Condition { mode: Remains, count: 1, cards: [cardA, cardB] }) `shouldEqual`
      [ [{ card: cardA, min: 0, max: 3 }, { card: cardB, min: 0, max: 1 }]
      , [{ card: cardA, min: 0, max: 2 }, { card: cardB, min: 0, max: 2 }]
      ]
    mkConditionPattern (Condition { mode: Remains, count: 2, cards: [cardA, cardB] }) `shouldEqual`
      -- cardA.count + cardB.count - 2 == 3
      [ [{ card: cardA, min: 0, max: 3 }, { card: cardB, min: 0, max: 0 }]
      , [{ card: cardA, min: 0, max: 2 }, { card: cardB, min: 0, max: 1 }]
      , [{ card: cardA, min: 0, max: 1 }, { card: cardB, min: 0, max: 2 }]
      ]
    mkConditionPattern (Condition { mode: Remains, count: 3, cards: [cardA, cardB, cardC] }) `shouldEqual`
      -- [3, 2]
      [ [{ card: cardA, min: 0, max: 3 }, { card: cardB, min: 0, max: 2 }, { card: cardC, min: 0, max: 0 }]
      , [{ card: cardA, min: 0, max: 3 }, { card: cardB, min: 0, max: 0 }, { card: cardC, min: 0, max: 2 }]
      -- , [{ card: cardA, min: 0, max: 0 }, { card: cardB, min: 0, max: 3 }, { card: cardC, min: 0, max: 2 }] -- cardB.count < 3
      -- , [{ card: cardA, min: 0, max: 2 }, { card: cardB, min: 0, max: 3 }, { card: cardC, min: 0, max: 0 }] -- cardB.count < 3
      , [{ card: cardA, min: 0, max: 2 }, { card: cardB, min: 0, max: 0 }, { card: cardC, min: 0, max: 3 }]
      , [{ card: cardA, min: 0, max: 0 }, { card: cardB, min: 0, max: 2 }, { card: cardC, min: 0, max: 3 }]
      -- [3, 1, 1]
      , [{ card: cardA, min: 0, max: 3 }, { card: cardB, min: 0, max: 1 }, { card: cardC, min: 0, max: 1 }]
      -- , [{ card: cardA, min: 0, max: 1 }, { card: cardB, min: 0, max: 3 }, { card: cardC, min: 0, max: 1 }] -- cardB.count < 3
      , [{ card: cardA, min: 0, max: 1 }, { card: cardB, min: 0, max: 1 }, { card: cardC, min: 0, max: 3 }]
      -- [2, 2, 1]
      , [{ card: cardA, min: 0, max: 2 }, { card: cardB, min: 0, max: 2 }, { card: cardC, min: 0, max: 1 }]
      , [{ card: cardA, min: 0, max: 2 }, { card: cardB, min: 0, max: 1 }, { card: cardC, min: 0, max: 2 }]
      , [{ card: cardA, min: 0, max: 1 }, { card: cardB, min: 0, max: 2 }, { card: cardC, min: 0, max: 2 }]
      ]

  mkConditionPatternJustRemainsTest = do
    -- Condition JustRemains n cards == Condition JustDraw (cardCount - n) cards
    mkConditionPattern (Condition { mode: JustRemains, count: 1, cards: [cardA] }) `shouldEqual` [[{ card: cardA, min: 2, max: 2 }]]
    mkConditionPattern (Condition { mode: JustRemains, count: 2, cards: [cardA] }) `shouldEqual` [[{ card: cardA, min: 1, max: 1 }]]
    mkConditionPattern (Condition { mode: JustRemains, count: 3, cards: [cardA] }) `shouldEqual` [[{ card: cardA, min: 0, max: 0 }]]
    mkConditionPattern (Condition { mode: JustRemains, count: 4, cards: [cardA] }) `shouldEqual` []
    mkConditionPattern (Condition { mode: JustRemains, count: 2, cards: [cardA, cardB] }) `shouldEqual`
      [ [{ card: cardA, min: 3, max: 3 }, { card: cardB, min: 0, max: 0 }]
      , [{ card: cardA, min: 2, max: 2 }, { card: cardB, min: 1, max: 1 }]
      , [{ card: cardA, min: 1, max: 1 }, { card: cardB, min: 2, max: 2 }]
      ]

  mkConditionPatternChoiceTest = do
    mkConditionPattern (Condition { mode: Choice, count: 1, cards: [cardA] }) `shouldEqual` [[{ card: cardA, min: 1, max: 3 }]]
    mkConditionPattern (Condition { mode: Choice, count: 1, cards: [cardA, cardB] }) `shouldEqual` [[{ card: cardA, min: 1, max: 3 }], [{ card: cardB, min: 1, max: 2 }]]
    mkConditionPattern (Condition { mode: Choice, count: 2, cards: [cardA, cardB] }) `shouldEqual` [[{ card: cardA, min: 1, max: 3 }, { card: cardB, min: 1, max: 2 }]]
    mkConditionPattern (Condition { mode: Choice, count: 1, cards: [cardA, cardB, cardC] }) `shouldEqual`
      [ [{ card: cardA, min: 1, max: 3 }]
      , [{ card: cardB, min: 1, max: 2 }]
      , [{ card: cardC, min: 1, max: 3 }]
      ]
    mkConditionPattern (Condition { mode: Choice, count: 2, cards: [cardA, cardB, cardC] }) `shouldEqual`
      [ [{ card: cardA, min: 1, max: 3 }, { card: cardB, min: 1, max: 2 }]
      , [{ card: cardA, min: 1, max: 3 }, { card: cardC, min: 1, max: 3 }]
      , [{ card: cardB, min: 1, max: 2 }, { card: cardC, min: 1, max: 3 }]
      ]

mergeConditionPatternTest :: Spec Unit
mergeConditionPatternTest = do
  describe "mergeConditionPattern" do
    it "empty" do
      mergeConditionPattern [] [] `shouldEqual` []
    it "merges many condition" do
      mergeConditionPattern
        [{ card: cardA, min: 1, max: 3 }] [{ card: cardB, min: 1, max: 2 }] `shouldEqual`
        sort' [{ card: cardA, min: 1, max: 3 }, { card: cardB, min: 1, max: 2 }]
      mergeConditionPattern
        [{ card: cardA, min: 1, max: 3 }] [{ card: cardB, min: 1, max: 2 }, { card: cardC, min: 1, max : 3 }] `shouldEqual`
        sort' [{ card: cardA, min: 1, max: 3 }, { card: cardB, min: 1, max: 2 }, { card: cardC, min: 1, max: 3 }]
    it "merges same condition" do
      mergeConditionPattern
        [{ card: cardA, min: 1, max: 3 }] [{ card: cardA, min: 1, max: 3 }] `shouldEqual`
        [{ card: cardA, min: 2, max: 3 }]
      mergeConditionPattern
        [{ card: cardA, min: 1, max: 3 }] [{ card: cardA, min: 1, max: 2 }] `shouldEqual`
        [{ card: cardA, min: 2, max: 2 }]
      mergeConditionPattern
        [{ card: cardA, min: 2, max: 3 }] [{ card: cardA, min: 2, max: 3 }] `shouldEqual`
        [{ card: cardA, min: 4, max: 3 }] -- invalid condition by max < min
    it "merges multiple condition" do
      mergeConditionPattern
        [{ card: cardA, min: 1, max: 3 }, { card: cardB, min: 1, max: 2 }]
        [{ card: cardA, min: 1, max: 3 }, { card: cardC, min: 1, max: 3 }] `shouldEqual`
        sort' [{ card: cardA, min: 2, max: 3 }, { card: cardB, min: 1, max: 2 }, { card: cardC, min: 1, max: 3 }]
  where
  sort' = Array.sortBy $ comparing _.card.id

buildConditionPatternTest :: Spec Unit
buildConditionPatternTest = do
  describe "buildConditionPattern" do
    it "build normal" do
      buildConditionPattern (NE.singleton (Condition { mode: AtLeast, count: 1, cards: [cardA] })) `shouldEqual` [[{ card: cardA, min: 1, max: 3 }]]
      buildConditionPattern (NE.singleton (Condition { mode: JustDraw, count: 2, cards: [cardA] })) `shouldEqual` [[{ card: cardA, min: 2, max: 2 }]]
      buildConditionPattern (NE.singleton (Condition { mode: Remains, count: 2, cards: [cardA] })) `shouldEqual` [[{ card: cardA, min: 0, max: 1 }]]
    it "build with multiple conditions" do
      buildConditionPattern (NE.cons' (Condition { mode: AtLeast, count: 1, cards: [cardA] }) [Condition { mode: AtLeast, count: 1, cards: [cardB] }]) `shouldEqual`
        [sort' [{ card: cardA, min: 1, max: 3 }, { card: cardB, min: 1, max: 2 }]]
      buildConditionPattern (NE.cons' (Condition { mode: JustDraw, count: 1, cards: [cardA] }) [Condition { mode: AtLeast, count: 1, cards: [cardA] }]) `shouldEqual`
        [] -- invalid condition
      buildConditionPattern (NE.cons' (Condition { mode: Remains, count: 2, cards: [cardA] }) [Condition { mode: JustDraw, count: 2, cards: [cardC] }]) `shouldEqual`
        [sort' [{ card: cardA, min: 0, max: 1 }, { card: cardC, min: 2, max: 2 }]]
    it "expanded pattern" do
      buildConditionPattern (NE.singleton (Condition { mode: Choice, count: 1, cards: [cardA, cardB, cardC] })) `shouldEqual`
        [ [{ card: cardA, min: 1, max: 3 }]
        , [{ card: cardB, min: 1, max: 2 }]
        , [{ card: cardC, min: 1, max: 3 }]
        ]
      buildConditionPattern (NE.cons' (Condition { mode: Choice, count: 1, cards: [cardA, cardB] }) [Condition { mode: AtLeast, count: 2, cards: [cardC] }]) `shouldEqual`
        [ sort' [{ card: cardA, min: 1, max: 3 }, { card: cardC, min: 2, max: 3 }]
        , sort' [{ card: cardB, min: 1, max: 2 }, { card: cardC, min: 2, max: 3 }]
        ]
    it "complicated pattern" do
      buildConditionPattern (NE.cons' (Condition { mode: Choice, count: 1, cards: [cardA, cardB] }) [Condition { mode: Choice, count: 2, cards: [cardB, cardC, cardD] }]) `shouldEqual`
        [ sort' [{ card: cardA, min: 1, max: 3 }, { card: cardB, min: 1, max: 2 }, { card: cardC, min: 1, max: 3 }] -- A + BC
        , sort' [{ card: cardA, min: 1, max: 3 }, { card: cardB, min: 1, max: 2 }, { card: cardD, min: 1, max: 1 }] -- A + BD
        , sort' [{ card: cardA, min: 1, max: 3 }, { card: cardC, min: 1, max: 3 }, { card: cardD, min: 1, max: 1 }] -- A + CD
        , sort' [{ card: cardB, min: 1 + 1, max: 2 }, { card: cardC, min: 1, max: 3 }] -- B + BC
        , sort' [{ card: cardB, min: 1 + 1, max: 2 }, { card: cardD, min: 1, max: 1 }] -- B + BD
        , sort' [{ card: cardB, min: 1, max: 2 }, { card: cardC, min: 1, max: 3 }, { card: cardD, min: 1, max: 1 }] -- B + CD
        ]
      buildConditionPattern (NE.cons' (Condition { mode: AtLeast, count: 1, cards: [cardD] }) [Condition { mode: Choice, count: 2, cards: [cardB, cardC, cardD] }]) `shouldEqual`
        [ sort' [{ card: cardD, min: 1, max: 1 }, { card: cardB, min: 1, max: 2 }, { card: cardC, min: 1, max: 3 }] -- D + BC
        -- , sort' [{ card: cardD, min: 1 + 1, max: 1 }, { card: cardB, min: 1, max: 2 }] -- D + BD invalid min < max
        -- , sort' [{ card: cardD, min: 1 + 1, max: 1 }, { card: cardC, min: 1, max: 3 }] -- D + CD invalid min < max
        ]
  where
  sort' = Array.sortBy $ comparing _.card.id

calculateTest :: Spec Unit
calculateTest = do
  describe "calculate" do
    it "check some results" do
      let deck = { cards: testCards, others: 11, hand: 5 }
      let cond01 = buildCondition [[{ cards: [cardA], count: 1, mode: AtLeast }]]
      runTest deck cond01 9316
      runTest deck { others = 0 } cond01 120
      let cond02 = buildCondition [[{ cards: [cardA], count: 1, mode: JustDraw }]]
      runTest deck cond02 7140
      runTest deck { others = 0 } cond02 45
      let cond03 = buildCondition [[{ cards: [cardA, cardB, cardC], count: 5, mode: AtLeast }]]
      runTest deck cond03 56
      runTest deck { others = 0 } cond03 56
      let cond04 = buildCondition [[{ cards: [cardA, cardB, cardC], count: 2, mode: Choice }]]
      runTest deck cond04 9080
      runTest deck { others = 0 } cond04 126
      let cond05 = buildCondition [[{ cards: [cardB, cardC, cardD], count: 4, mode: JustDraw }]]
      runTest deck cond05 210
      runTest deck { others = 0 } cond05 45
      let cond06 = buildCondition [[{ cards: [cardA], count: 1, mode: AtLeast }, { cards: [cardB], count: 1, mode: AtLeast }]]
      runTest deck cond06 3751
      runTest deck { others = 0 } cond06 99
      let cond07 = buildCondition [[{ cards: [cardA], count: 2, mode: JustDraw }, { cards: [cardB], count: 1, mode: AtLeast }]]
      runTest deck cond07 675
      runTest deck { others = 0 } cond07 48
      let cond08 = buildCondition [[{ cards: [cardD], count: 1, mode: AtLeast }, { cards: [cardA, cardB, cardC, cardD], count: 2, mode: Choice }]]
      runTest deck cond08 1819
      runTest deck { others = 0 } cond08 70
      let cond09 = buildCondition [[{ cards: [cardD], count: 1, mode: AtLeast }, { cards: [cardA, cardB, cardC], count: 2, mode: Choice }]]
      runTest deck cond09 1819
      runTest deck { others = 0 } cond09 70
      let cond10 = buildCondition [[{ cards: [cardA], count: 0, mode: JustDraw }]]
      let deck10 = { cards: [cardA, cardB], others: 5, hand: 5 }
      runTest deck10 cond10 21
      runTest deck10 { others = 2 } cond10 0
      let cond11 = buildCondition [[{ cards: [cardA, cardB, cardC, cardD], count: 3, mode: Choice }]]
      runTest deck cond11 3352
      runTest deck { others = 0 } cond11 118
      let cond12 = buildCondition [[{ cards: [cardA, cardB], count: 1, mode: JustDraw }, { cards: [cardC, cardD], count: 1, mode: JustDraw }]]
      runTest deck cond12 3300
      runTest deck { others = 0 } cond12 0
      let cond13 = buildCondition [[{ cards: [cardA, cardB], count: 1, mode: JustDraw }], [{ cards: [cardC, cardD], count: 1, mode: JustDraw }]]
      runTest deck cond13 10805
      runTest deck { others = 0 } cond13 25
      let cardE = { id: mkId "eee", name: "E", count: 0 }
      let cond14 = buildCondition [[{ cards: [cardE], count: 0, mode: LeftAll }]]
      let deck14 = deck { cards = [cardE] }
      runTest deck14 cond14 462
      let deck15 = deck { cards = deck.cards <> [cardE] }
      let cond15 = buildCondition [[{ cards: [cardA, cardB, cardE], count: 1, mode: LeftAll }]]
      runTest deck15 cond15 15504
      let deck16 = deck15
      let cond16 = buildCondition [[{ cards: [cardA, cardB, cardE], count: 2, mode: LeftAll }]]
      runTest deck16 cond16 11753

  where
  runTest deck cond expected = calculate (normalizeDeck deck cond) cond `shouldEqual` BigInt.fromInt expected
  buildCondition = Array.mapMaybe NE.fromArray <<< map (map Condition)

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
