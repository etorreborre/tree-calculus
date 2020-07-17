{-# language BlockArguments #-}

module Test.TreeSpec where

import Tree
import Protolude hiding ((*))
import Test.Tasty.Extensions hiding (eval)
import Prelude hiding ((*), putStrLn, print)
import Test.Checks
import Test.Generators

test_display = test "display" do
  checkDisplay k "∆∆"
  checkDisplay i "∆(∆∆)(∆∆)"
  checkDisplay d "∆(∆∆)(∆∆∆)"
  checkDisplay (k * d) "∆∆(∆(∆∆)(∆∆∆))"
  checkDisplay s "∆(∆(∆∆(∆(∆∆)(∆∆∆))))(∆(∆(∆∆))(∆∆(∆(∆∆)(∆∆∆))))"

test_k = minTestsOk 20 $ prop "K = ∆∆" do
  (x, y) <- forAll genPair
  checkEval (k * x * y) x

test_i = minTestsOk 20 $ prop "I = ∆(∆∆)(∆∆)" do
  x <- forAll genTree
  checkEval (i * x) x

test_d = minTestsOk 20 $ prop "D = ∆(∆∆)(∆∆∆)" do
  (x, y, z) <- forAll genTriple
  checkEval (d * x * y * z) ((y * z) * (x * z))

test_s = minTestsOk 20 $ prop "S = ∆(∆(KD))(∆(∆K)(KD))" do
  (x, y, z) <- forAll genTriple
  checkEval (s * x * y * z) ((x * z) * (y * z))
