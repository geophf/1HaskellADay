{-# LANGUAGE OverloadedStrings #-}

module Y2021.M02.D25.Solution where

{--
Did you like yesterday off? Back to work!

The day before yesterday, we saved out the metaphones for wineries, today we
will save those metaphones to the graph-store. Now, I suppose one could look
at the metaphones as a kind of alias for a winery? But not me.

Before we save these metaphones to the graph-store, we must read them (back)
in from the file, parsing them appropriately.
--}

import Data.List (sortOn)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe (fromMaybe)

import Data.Ord    -- for Down

import Data.Set (Set)
import qualified Data.Set as Set

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
   parseJSON = withObject "KeyValue" $ \v ->
      KV <$> v .: "key" <*> v .: "value"

data M' = M' String String
   deriving (Eq, Ord, Show)

instance FromJSON M' where
   parseJSON = withObject "Metaphone" $ \v ->
      v .: "metaphone" >>= \z -> M' <$> z .: "primary" <*> z .: "secondary"

{--
We don't need this ...

m2meta :: M' -> Metaphone
m2meta (M' a b) = Meta (a, b) 
--}

instance (FromJSON a, FromJSON b) => FromJSON (IxKeyValue a b) where
   parseJSON = withObject "IxKeyValue" $ \v ->
      IxKV <$> v .: "index" <*> v .: "key-value"

-- with the above instance definitions, we can do this:

fetchWineryMetaphones :: FilePath -> IO [IxKeyValue Name M']
fetchWineryMetaphones file = fromMaybe [] . decode <$> BL.readFile file

{--
>>> fetchWineryMetaphones (todaysDir ++ "graph-wineries-metaphones.json")
[...]
>>> let graphws = it
>>> length it
16632
--}

-- With the indexed-named metaphones we can build a mapping:

data IxWinery = WN Name Idx
   deriving (Eq, Ord, Show)

type WineriesMetaphones = Map M' (Set IxWinery)

metaphones :: [IxKeyValue Name M'] -> WineriesMetaphones
metaphones = snarf ix2wm

ix2wm :: IxKeyValue Name M' -> Maybe (M', IxWinery)
ix2wm (IxKV i (KV n m)) = Just (m, WN n i)

{--
>>> let mph = metaphones graphws

How many unique metaphones are there for the fetched wineries? 

>>> length mph
14209

Which metaphone has the most related wineries?

>>> head (sortOn (Down . Set.size . snd) (Map.toList mph))
(M' "KL" "",fromList [WN "Cali 351" 6920,WN "Callaway" 1348,WN "Callia" 7178,
                      WN "Chloe" 5948,WN "Coelho" 1267,WN "Cola" 6230,
                      WN "Coli" 2632,WN "Colio" 6757,WN "Gaul" 7170,
                      WN "Goul\233e" 5558,WN "Kaella" 5537,WN "Kale" 3379,
                      WN "Klee" 13220,WN "Kohl" 843,WN "Koyle" 772,
                      WN "Qualia" 16270])
--}

-- Now, let's load these to the graph.

instance Node M' where
   asNode (M' pri sec) = constr "Metaphone" [("primary", pri),("secondary", sec)]

instance Node IxWinery where
   asNode (WN w idx) = constr "Winery" [("name", w)] -- ids are functions, odd.

data METAPHONE = METAPHONE
   deriving (Eq, Ord, Show)

instance Edge METAPHONE where
   asEdge = const "METAPHONE"

-- Now you have everything to relate metaphones to wineries in the graph-store

type WineMetaphone = Relation M' METAPHONE IxWinery

wineryMetaphoneRels :: WineriesMetaphones -> [WineMetaphone]
wineryMetaphoneRels =
   map relIt . (>>= traverse Set.toList) . Map.toList
      where relIt (met, win) = Rel met METAPHONE win

{-- 
>>> let wmph = wineryMetaphoneRels mph

How many relations did you generate?

>>> length wmph
16632

>>> take 2 wmph
[Rel (M' "" "") METAPHONE (WN "1+1=3" 8695),
 Rel (M' "" "") METAPHONE (WN "181" 3129)]
--}

{-- with this, you can now `cyphIt` to the graph. Do that.

>>> graphEndpoint >>= flip cyphIt wmph
"...{\"columns\":[],\"data\":[]}],\"errors\":[]}\n"
--}
