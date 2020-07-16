module Test.Generators where

import Test.Tasty.Extensions hiding (eval)
import Hedgehog.Range as Range
import Tree

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
