module Test.Main where

import Prelude hiding (map)

import Data.Natural (intToNat)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Test.QuickCheck (class Testable)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck')
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

import Data.HashSet.Multi

quickCheck'' :: forall t2. Testable t2 => t2 -> Aff Unit
quickCheck'' = quickCheck' 2000

main :: Effect Unit
main = run [consoleReporter] do
  describe "Unit tests" do
    it "`insert` works" do
      insert "b" (insert "a" empty) `shouldEqual` fromFoldable [ "a", "b" ]
    it "`insertN` works" do
      insertN (intToNat 2) "b" empty `shouldEqual` fromFoldable [ "b", "b" ]
    it "`count` works for `empty`" do
      count "a" empty `shouldEqual` intToNat 0
    it "`count` works for non-empty" do
      count "a" (fromFoldable [ "a", "b", "a" ]) `shouldEqual` intToNat 2
    it "`delete` works for non-zero" do
      delete "a" (fromFoldable [ "a", "b", "a" ]) `shouldEqual` fromFoldable [ "a", "b" ]
    it "`delete` works for zero" do
      delete "b" (fromFoldable [ "a", "b", "a" ]) `shouldEqual` fromFoldable [ "a", "a" ]
    it "`toRepeatingArray` works" do
      toRepeatingArray (fromFoldable [ "a", "b", "a", "b", "c" ]) `shouldEqual` [ "a", "a", "b", "b", "c" ]
    it "`toArray` works" do
      toArray (fromFoldable [ "a", "b", "a", "b", "c" ]) `shouldEqual` [ Tuple "a" (intToNat 2), Tuple "b" (intToNat 2), Tuple "c" (intToNat 1) ]
    it "`map` works for collapse" do
      map (_ == "a") (fromFoldable [ "a", "b", "c", "d", "a" ]) `shouldEqual` fromFoldableWithCounts [ Tuple true (intToNat 2), Tuple false (intToNat 3) ]
    it "`map` works with identity" do
      map identity (fromFoldable [ "a", "b", "c", "d", "a" ]) `shouldEqual` fromFoldable [ "a", "b", "c", "d", "a" ]
    it "`mapWithCount` works for collapse" do
      mapWithCount (\a n -> n >= intToNat 2) (fromFoldable [ "a", "b", "c", "d", "a" ]) `shouldEqual` fromFoldableWithCounts [ Tuple true (intToNat 2), Tuple false (intToNat 3) ]
    it "`mapWithCount` works with identity" do
      mapWithCount const (fromFoldable [ "a", "b", "c", "d", "a" ]) `shouldEqual` fromFoldable [ "a", "b", "c", "d", "a" ]
    it "`filter` works" do
      filter (_ /= "a") (fromFoldable [ "a", "b", "c", "d", "c", "a" ]) `shouldEqual` fromFoldable [ "b", "c", "d", "c" ]
    it "`filterWithCount` works" do
      filterWithCount (\a n -> n == intToNat 2) (fromFoldable [ "a", "b", "c", "d", "c", "a" ]) `shouldEqual` fromFoldable [ "a", "c", "c", "a" ]
    it "`union` works" do
      union (fromFoldable [ "a", "b", "a", "c" ]) (fromFoldable [ "a", "b", "d" ]) `shouldEqual` fromFoldable [ "a", "b", "a", "c", "a", "b", "d" ]
    it "`intersection` works" do
      intersection (fromFoldable [ "a", "b", "a", "c" ]) (fromFoldable [ "a", "b", "d" ]) `shouldEqual` fromFoldable [ "a", "b", "a", "a", "b" ]
    it "`difference` works" do
      difference (fromFoldable [ "a", "b", "a", "c" ]) (fromFoldable [ "a", "b", "d" ]) `shouldEqual` fromFoldable [ "a", "c" ]
      difference (fromFoldable [ "a", "b", "a", "c" ]) (fromFoldable [ "b" ]) `shouldEqual` fromFoldable [ "a", "a", "c" ]
      difference (fromFoldable [ "a", "b", "a", "c" ]) (fromFoldable [ "a", "a", "a", "b" ]) `shouldEqual` fromFoldable [ "c" ]
    it "`size` works" do
      size (fromFoldable [ "a", "b", "c", "a" ]) `shouldEqual` intToNat 4
