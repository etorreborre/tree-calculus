module Test.Checks where

import qualified Data.Text as T
import Protolude
import Test.Tasty.Extensions hiding (eval)
import Tree

checkEval :: HasCallStack => Tree -> Tree -> PropertyT IO ()
checkEval actualTree expectedTree = withFrozenCallStack $ do
  let actual = eval actualTree
  let expected = eval expectedTree
  annotate ("expected " <> showTree expected)
  annotate ("got " <> showTree actual)
  actual === expected

checkDisplay :: Tree -> Text -> PropertyT IO ()
checkDisplay tree expected = withFrozenCallStack $ do
  let actual = display tree
  showDeltas actual === showDeltas expected

-- I can't find how to display unicode characters with hedgehog
showDeltas = T.replace "∆" "^"

showTree = toS . showDeltas . display @Tree
