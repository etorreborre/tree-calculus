{-# language BlockArguments #-}

module Test.NumberSpec where

import Tree
import Number
import Logic
import Test.Checks
import Test.Generators
import Test.Tasty.Extensions hiding (eval)
import Prelude hiding ((*), and, or, not)

test_is_zero = prop "is zero" do
  positive <- forAll genStrictlyPositiveNumber

  checkEval (isZero * Node) true
  checkEval (isZero * positive) false

test_is_zeroN = prop "is zeroN" do
  positive <- forAll genStrictlyPositiveNumber
  
  checkEval (isZeroN Node) true
  checkEval (isZeroN positive) false
