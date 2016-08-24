module Data.Relation where

class Node a where
   asNode :: a -> String

-- example: asNode (Nd f) = "Node { name: '" ++ show f ++ "' }"

class Show a => Edge a where
   asEdge :: a -> String

-- an example for asEdge would be "USING" or "RETWEETS"

data Relation a rel b = Rel a rel b deriving (Eq, Show)

-- Now we have a directional relation: a dart, that expresses how this datum
-- (tweet) is related to other data.

data Direction = GoingTo | ComingFrom
   deriving (Eq, Ord)

instance Show Direction where
   show GoingTo = ">"
   show ComingFrom = "<"

type Label = String

data Dart a = Drt Direction Label a
   deriving (Eq, Ord)

instance Show a => Show (Dart a) where
   show (Drt GoingTo lbl val) = '-':lbl ++ ">" ++ show val
   show (Drt ComingFrom lbl val) = '<':lbl ++ "-" ++ show val
