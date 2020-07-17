{-# LANGUAGE PatternSynonyms #-}

module Tree where

import Protolude as P hiding ((*), (:*:))
import Prelude hiding ((*))

data Tree =
   Node
 | App Tree Tree
 deriving (Eq, Show)

pattern (:*:) :: Tree -> Tree -> Tree
pattern a :*: b = App a b

class Display a where
  display :: a -> Text

instance Display Tree where
  display Node = "∆"
  display (App Node Node) = "∆∆"
  display (App (App Node Node) Node) = "∆∆∆"
  display (App Node x) = "∆(" <> display x <> ")"
  display (App x Node) =    display x <> "∆"
  display (App x y) = display x <> "(" <> display y <> ")"

isProgram :: Tree -> Bool
isProgram Node = True
isProgram (App Node m) = isProgram m
isProgram (App (App Node m) n) = isProgram m && isProgram n
isProgram  _ = False

eval :: Tree -> Tree
{-
∆∆yz = y
Node @ Node @ y @ z =
   ∆
  / \  -> y
 ∆  y

-}
eval (Node :*: Node :*: y :*: _z) = eval y
{-
∆(∆x)yz = (yz)(xz)
Node @ (Node @ x) @ y @ z = y@z@(x@z).
Node @ (Node @ x) @ y @ z = App (App (App Node (App Node x)) y) z

y@z@(x@z) = App (App y z) (App x z)

     ∆          y
    / \ \       \ \
   ∆  y  z ->   z x
  /                \
 x                 z

-}
eval (Node :*: (Node :*: x) :*: y :*: z) = eval $ y * z * (x * z)
{-
∆(∆wx)yz = zwx

 Node @ (Node @ w @ x) @ y @ z = z@w@x.
 App (App (App Node (App (App Node w) x)) y) z = App (App z w) x

     ∆               z
    / \ \            \ \
   ∆  y  z  ->       w  x
  / \
 w  x

-}
eval (Node :*: (Node :*: w :*: x) :*: _y :*: z) = eval (z * w * x)
eval Node = Node
eval (Node :*: m) = Node * eval m
eval (Node :*: m :*: n) = Node * eval m * eval n
eval (App x y) =  eval $ eval x * eval y

infixl 3 *

(*) :: Tree -> Tree -> Tree
(*) = App

-- Common combinations

l :: Tree
l = Node

-- ∆∆
k :: Tree
k = Node * Node

kn :: Int -> Tree
kn 0 = Node
kn 1 = Node * Node
kn n = k * kn (n - 1)

-- ∆∆(∆(∆∆)(∆∆))
ki :: Tree
ki = k * i

-- ∆(∆∆)(∆∆)
i :: Tree
i = Node * k * k

-- ∆(∆∆)(∆∆∆)
-- d x y z = ∆∆∆x(∆x)yz
-- d x y z = ∆(∆x)yz
-- d x y z = yz(xz)
d :: Tree
d = Node * k * (Node * Node * Node)

-- ∆(∆(KD))(∆(∆K)(KD))
s :: Tree
s = Node * (Node * (k * d)) * (Node * (Node * k) * (k * d))
