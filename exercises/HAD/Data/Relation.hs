{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Relation where

-- defines relations as a "thing." So we have nodes, relations, and from those,
-- we can model anything, right? ... well, constructively, that is.

import Data.Text (Text)
import qualified Data.Text as T

-- below import available via 1HaskellADay git repository

class Node a where
   asNode :: a -> Text

-- example: asNode (Nd f) = T.pack ("Node { name: '" ++ show f ++ "' }")

type Attribute a = (Text, a)

weave :: [Text] -> Text
weave = T.intercalate ","

-- constructs nodes and edges

constr :: forall a. Show a => Text -> [Attribute a] -> Text
constr typ attrs = T.concat [typ, " { ", weave (map shower attrs), " }"]

shower :: Show a => Attribute a -> Text
shower (x,y) = T.concat [x, ": ", T.pack (show y)]

-- so you can now define node and edge instances as

-- asNode x = constr "Node" xsattributes (whatever they are)

class Edge a where
   asEdge :: a -> Text

-- an example for asEdge would be "USING" or "RETWEETS"

data Relation a rel b = Rel a rel b
   deriving (Eq, Ord, Show)

-- Now we have a directional relation: a dart, that expresses how this datum
-- (tweet) is related to other data.

data Direction = GoingTo | ComingFrom
   deriving (Eq, Ord)

instance Show Direction where
   show GoingTo = ">"
   show ComingFrom = "<"

type Label = Text

data Dart a = Drt Direction Label a
   deriving (Eq, Ord)

instance Show a => Show (Dart a) where
   show (Drt GoingTo lbl val) = '-':(T.unpack lbl) ++ ">" ++ show val
   show (Drt ComingFrom lbl val) = '<':(T.unpack lbl) ++ "-" ++ show val

-- RELATABLE -----------------------------------------------------------------

class Relatable a b rel | a b -> rel where
   relate :: a -> b -> Relation a rel a

-- what we are saying here is that our relation-type is uniquely-determined
-- by the types a and b, but, not only that, but for each relation a -> b
-- there is some relation determined by the values a and b
