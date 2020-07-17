module Number where

import Prelude hiding ((*), not)
import Tree

zero :: Tree
zero = Node

one :: Tree
one = k * zero

successor :: Tree -> Tree
successor n = k * n

isZero :: Tree
isZero = Node * (Node * (k * (k * (k * (k * i))))) * (Node * (Node * (k * k)) * Node)

isZeroN :: Tree -> Tree
isZeroN n = Node * n * k * (k * (k * (k * i)))

-- isZeroN Node
--  Node * Node * k * (kn 3 * i)
-- by leaf rule
--  k

-- isZeroN 1
--  Node * (Node * Node * Node) * k * (k * (k * (k * i)))
-- by fork rule
--  k * (k * (k * i))) * Node * Node
--  k * (k * (k * i * Node) * Node)
--  k * (k * i * Node)
--  k * i
