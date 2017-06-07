module Y2017.M06.D07.Exercise where

{--
So, here's one of the questions Amazon asks developers to test their under-
standing of data structures.

You have a binary tree of the following structure:

        A
       / \
      /   \
     B     C
    / \   / \
   D   E F   G

1. create the BinaryTree type and materialize the above value.
--}

data Sym = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O
   deriving (Eq, Ord, Enum, Bounded, Show)

data Node a = Bin (Node a) (Node a) | Leaf a
   deriving (Eq, Show)

abcdefg :: Node Sym
abcdefg = undefined

{--
Now, you want to traverse the external nodes in a clockwise direction. For the
above binary tree, a clockwise Edge Node traversal will return:
--}

clockwiseSmallTree :: [Sym]
clockwiseSmallTree = [A, C, G, F, E, D, B]

-- define

clockwiseEdgeTraversal :: Node a -> [a]
clockwiseEdgeTraversal bintree = undefined

{--
such that:

>>> clockwiseEdgeTraversal abcdefg == clockwiseSmallTree
True
--}

{-- BONUS -----------------------------------------------------------------

Simple enough, eh?

Now, does it work for the larger tree?

                     A
                   /   \
                  /     \
                 /       \
               B           C
             /   \       /   \
            D     E     F     G
           / \   / \   / \   / \
          H   I J   K L   M N   O
--}

largerTree :: Node Sym
largerTree = undefined

-- the EDGE clockwise traversal is thus:

largerClockwiseTraversal :: [Sym]
largerClockwiseTraversal = [A, C, G, O, N, M, L, K, J, I, H, D, B]

{-- 
n.b.: as E and F are internal nodes, not edge nodes, they are NOT part of
the traversal.

so:

>>> clockwiseTraversal largerTree == largerClockwiseTraversal
True

Got it?
--}
