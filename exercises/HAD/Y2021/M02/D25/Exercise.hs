{-# LANGUAGE OverloadedStrings #-}

module Y2021.M02.D25.Exercise where

{--
Did you like yesterday off? Back to work!

The day before yesterday, we saved out the metaphones for wineries, today we
will save those metaphones to the graph-store. Now, I suppose one could look
at the metaphones as a kind of alias for a winery? But not me.

Before we save these metaphones to the graph-store, we must read them (back)
in from the file, parsing them appropriately.
--}

import Data.Map (Map)
import Data.Set (Set)

import Data.Aeson hiding (KeyValue)
import Data.Aeson.WikiDatum (Name)

import Graph.Query
import Graph.JSON.Cypher

import Data.Relation

import qualified Data.ByteString.Lazy.Char8 as BL

import Y2021.M02.D23.Solution
import Y2021.M01.D21.Solution (Idx)

import Control.Map (snarf)

instance (FromJSON a, FromJSON b) => FromJSON (KeyValue a b) where
   parseJSON = undefined

instance FromJSON Metaphone where
   parseJSON = undefined

instance (FromJSON a, FromJSON b) => FromJSON (IxKeyValue a b) where
   parseJSON = undefined

-- with the above instance definitions, we can do this:

fetchWineryMetaphones :: FilePath -> IO [IxKeyValue Name Metaphone]
fetchWineryMetaphones = undefined

-- With the indexed-named metaphones we can build a mapping:

data IxWinery = WN Name Idx
   deriving (Eq, Ord, Show)

type WineriesMetaphones = Map Metaphone (Set IxWinery)

metaphones :: [IxKeyValue Name Metaphone] -> WineriesMetaphones
metaphones = undefined

-- How many unique metaphones are there for the fetched wineries? Which
-- metaphone has the most related wineries?

-- Now, let's load these to the graph.

instance Node Metaphone where
   asNode = undefined

instance Node IxWinery where
   asNode = undefined

data METAPHONE = METAPHONE
   deriving (Eq, Ord, Show)

instance Edge METAPHONE where
   asEdge = undefined

-- Now you have everything to relate metaphones to wineries in the graph-store

type WineMetaphone = Relation Metaphone METAPHONE IxWinery

wineryMetaphoneRels :: WineriesMetaphones -> [WineMetaphone]
wineryMetaphoneRels = undefined

-- how many relations did you generate?

-- with this, you can now `cyphIt` to the graph. Do that.
