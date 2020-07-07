module TreeCalculus where

import Protolude

data Tree =
   Leaf
 | Stem Tree
 | Fork Tree Tree
 deriving (Eq, Show)

class Display a where
  display :: a -> Text

instance Display Tree where
  display Leaf = "∆"
  display (Stem Leaf) = "∆∆"
  display (Stem x) = "∆(" <> display x <> ")"
  display (Fork Leaf Leaf) = "∆∆∆"
  display (Fork x Leaf) = "∆(" <> display x <> ")∆"
  display (Fork Leaf x) = "∆∆(" <> display x <> ")"
  display (Fork x y) = "∆(" <> display x <> ")(" <> display y <> ")"

delta :: Tree -> Tree -> Tree
delta (Fork Leaf y) _z = y
delta (Fork (Stem x) y) z = delta (delta y z) (delta x z)
delta (Fork (Fork w x) _y) z = delta (delta z w) x
delta Leaf x = Stem x
delta (Stem x) y = Fork x y

infixl 3 *

(*) :: Tree -> Tree -> Tree
(*) = delta

l :: Tree
l = Leaf

k :: Tree
k = Stem Leaf

i :: Tree
i = Fork k k

d :: Tree
d = Fork k (Fork Leaf Leaf)
