module Y2020.M08.D10.Exercise where

{--
Heaps! Heaps are useful. A heap is a collection of values where the highest-
priority value (most frequent, highest value, what have you) is the element
at the head of the heap, followed by the next most-valuable, followed by the
next, etc. Heaps are also called Priority Queues.

Heaps are used all over the place. You got sorting? It may be a heap. You got
prioritization? It may be a heap. You got a heap? It may be a heap.

That last one is 'a bit' of a tautology.

So, today: your mission, should you decide to accept it, is to define a heap
or a priority queue. We'll find uses in later exercises.

I found a good read and a good implementation of heaps at

http://typeocaml.com/2015/03/12/heap-leftist-tree/

where the author argued that heaps are all about merge operations. Fun!
--}

data Heap t = Something t
  deriving (Eq, Ord, Show)

rank :: Heap t -> Int
rank heap = undefined

merge :: Ord t => Heap t -> Heap t -> Heap t
merge h0 h1 = undefined

insert :: Ord t => t -> Heap t -> Heap t
insert = merge <$> singleton

singleton :: Ord t => t -> Heap t
singleton = undefined

list2heap :: Ord t => [t] -> Heap t
list2heap = undefined

heap2list :: Heap t -> [t]
heap2list = undefined

getMin :: Heap t -> t
getMin heap = undefined

deleteMin :: Heap t -> Heap t
deleteMin head = undefined

{--
>>> list2heap [2,7,17,3,19,100,36,25,1]
Node Leaf 1 (Node (Node Leaf 7 (Node Leaf 17 Leaf 1) 1) ...) 1

...  so this answer gives some hint to the underlying structure of a heap.
--}
