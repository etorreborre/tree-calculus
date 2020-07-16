{-# language BlockArguments #-}

module Test.TreeSpec where

import Tree
import Data.Text as T
import Protolude hiding ((*))
import Test.Tasty.Extensions hiding (eval)
import Prelude hiding ((*), putStrLn, print)

test_display = test "display" do
  checkDisplay k "∆∆"
  checkDisplay i "∆(∆∆)(∆∆)"
  checkDisplay d "∆(∆∆)(∆∆∆)"
  checkDisplay (k * d) "∆∆(∆(∆∆)(∆∆∆))"
  checkDisplay s "∆(∆(∆∆(∆(∆∆)(∆∆∆))))(∆(∆(∆∆))(∆∆(∆(∆∆)(∆∆∆))))"

test_k = minTestsOk 20 $ prop "K = ∆∆" do
  (x, y) <- forAll genPair
  checkEvaluation (k * x * y) x

test_i = minTestsOk 20 $ prop "I = ∆(∆∆)(∆∆)" do
  x <- forAll genTree
  checkEvaluation (i * x) x

test_d = minTestsOk 20 $ prop "D = ∆(∆∆)(∆∆∆)" do
  (x, y, z) <- forAll genTriple
  checkEvaluation (d * x * y * z) ((y * z) * (x * z))

test_s = minTestsOk 20 $ prop "S = ∆(∆(KD))(∆(∆K)(KD))" do
  (x, y, z) <- forAll genTriple
  checkEvaluation (s * x * y * z) ((x * z) * (y * z))

-- * HELPERS
checkEvaluation :: Tree -> Tree -> PropertyT IO ()
checkEvaluation actualTree expectedTree = do
  let actual = eval actualTree
  let expected = eval expectedTree
  annotate ("expected " <> showTree expected)
  annotate ("got " <> showTree actual)
  actual === expected

genPair :: Gen (Tree, Tree)
genPair = (,) <$> genTree <*> genTree

genTriple :: Gen (Tree, Tree, Tree)
genTriple = (,,) <$> genTree <*> genTree <*> genTree

checkDisplay :: Tree -> Text -> PropertyT IO ()
checkDisplay tree expected = do
  let actual = display tree
  showDeltas actual === showDeltas expected

-- I can't find how to display unicode characters with hedgehog
showDeltas = T.replace "∆" "^"

showTree = toS . showDeltas . display @Tree

genTree :: Gen Tree
genTree = sized genSizedTree

genSizedTree :: Size -> Gen Tree
genSizedTree size =
  choice [
    pure Node,
    App <$> genSizedTree (size `div` 2) <*> genSizedTree (size `div` 2)]
