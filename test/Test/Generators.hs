module Test.Generators where

import Test.Tasty.Extensions hiding (eval)
import Hedgehog.Gen as Gen
import Hedgehog.Range as Range
import Tree
import Prelude hiding ((*))

genTree :: Gen Tree
genTree = sized genSizedTree

genSizedTree :: Size -> Gen Tree
genSizedTree size =
  choice [
    pure Node,
    App <$> genSizedTree (size `div` 3) <*> genSizedTree (size `div` 3)]

genPairs :: Gen [(Tree, Tree)]
genPairs = list (linear 5 10) genPair

genPair :: Gen (Tree, Tree)
genPair = (,) <$> genTree <*> genTree

genTriple :: Gen (Tree, Tree, Tree)
genTriple = (,,) <$> genTree <*> genTree <*> genTree

genStrictlyPositiveNumber :: Gen Tree
genStrictlyPositiveNumber = do
  n <- Gen.enum 1 10
  genStrictlyPositiveNumberOfSize n

genStrictlyPositiveNumberOfSize :: Size -> Gen Tree
genStrictlyPositiveNumberOfSize (Size 1) = pure (k * Node)
genStrictlyPositiveNumberOfSize (Size n) = (k *) <$> genStrictlyPositiveNumberOfSize (Size $ n - 1)
