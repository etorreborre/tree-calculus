{-# language BlockArguments #-}

module Test.PairSpec where

import Tree
import Pair
import Test.Generators
import Test.Tasty.Extensions hiding (eval)
import Prelude hiding ((*), and, or, not)

test_first = prop "first" do
  (x, y) <- forAll genPair
  eval (first (pair * x * y)) === eval x

test_second = prop "second" do
  (x, y) <- forAll genPair
  eval (second (pair * x * y)) === eval y

-- futile attempts to find different combinations for pairs
test_find_pair_combination = prop "find pair combination" do
  (c1, c2) <- forAll genPair
  tryFirst <- forAll genTree
  trySecond <- forAll genTree
  tuples <- forAll genPairs
  --let pairs = (\(x, y) -> c1 * x * c2 * y) <$> tuples
  --let pairs = (\(x, y) -> x * y * c1) <$> tuples
  --let pairs = (\(x, y) -> x * c1 * y * c2) <$> tuples
  let pairs = (\(x, y) -> c1 * x * y * c2) <$> tuples

  let actual = (\p -> (eval (tryFirst * p), eval (trySecond * p))) <$> pairs
  let expected = (\(x, y) -> (eval x, eval y)) <$> tuples

  actual /== expected
