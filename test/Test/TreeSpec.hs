module Test.TreeSpec where

import TreeCalculus
import Test.Tasty.Extensions

test_display = test "display" $ do
  display k === "∆∆"
  display i === "∆(∆∆)(∆∆)"
  display d === "∆(∆∆)(∆∆∆)"
