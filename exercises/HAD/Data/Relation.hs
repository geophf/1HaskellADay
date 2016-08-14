{-# LANGUAGE OverloadedStrings #-}

module Data.Relation where

class Node a where
   asNode :: a -> String

class Show a => Edge a where
   asEdge :: a -> String

data Relation a rel b = Rel a rel b deriving (Eq, Show)

-- moved all of the Cypher-related stuff to Graph.JSON.Cypher
