
module Logic where

import Prelude hiding ((*), not)
import Tree

true :: Tree
true = k

false :: Tree
false = k * i

-- ∆(∆(K(KI)))
and :: Tree
and = Node * (Node * (k * (k * i)))

-- ∆(∆(KK)I)
or :: Tree
or = Node * (Node * (k * k)) * i

-- ∆(∆(KK))
implies :: Tree
implies = Node * (Node * (k * k))

-- ∆(∆(KK))(∆(∆(K(KI)))I)
not :: Tree
not = Node * (Node * (k * k)) * (Node * (Node * (k * (k * i))) * i)

-- ∆(∆Inot)∆
iff :: Tree
iff = Node * (Node * i * not) * Node
