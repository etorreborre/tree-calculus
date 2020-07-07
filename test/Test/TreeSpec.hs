{-# language BlockArguments #-}

module Test.TreeSpec where

import TreeCalculus
import Data.Text as T
import Protolude hiding ((*))
import Test.Tasty.Extensions
import Prelude hiding ((*), putStrLn)

test_display = test "display" do
  checkDisplay k "∆∆"
  checkDisplay i "∆(∆∆)(∆∆)"
  checkDisplay d "∆(∆∆)(∆∆∆)"
  checkDisplay (k * d) "∆∆(∆(∆∆)(∆∆∆))"
  checkDisplay s "∆(∆(∆∆(∆(∆∆)(∆∆∆))))(∆(∆(∆∆))(∆∆(∆(∆∆)(∆∆∆))))"

test_s = minTestsOk 20 $ prop "S = ∆(∆(KD))(∆(∆K)(KD))" do
  x <- forAll genTree
  y <- forAll genTree
  z <- forAll genTree
  let sxyz = s * x * y * z
  let xz_yz = (x * z) * (y * z)

  sxyz === xz_yz

-- * HELPERS

checkDisplay :: Tree -> Text -> PropertyT IO ()
checkDisplay tree expected = do
  let actual = display tree
  -- I can't find how to display unicode characters with hedgehog
  let showDeltas = T.replace "∆" "^"
  showDeltas actual === showDeltas expected

-- ∆(∆(KD))(∆(∆K)(KD))
s = Fork (Stem (k * d)) (Fork (Stem k) (k * d))

genTree :: Gen Tree
genTree = sized genSizedTree

genSizedTree :: Size -> Gen Tree
genSizedTree size =
  choice [
    pure Leaf,
    Stem <$> genSizedTree (size `div` 2),
    Fork <$> genSizedTree (size `div` 2) <*> genSizedTree (size `div` 2)]
