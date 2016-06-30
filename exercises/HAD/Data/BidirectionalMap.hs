module Data.BidirectionalMap where

-- So, we want to look up then look back

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid

data BidirectionalMap k v = BDMap (Map k v) (Map v k)
   deriving (Eq, Ord) -- a quotient type would help here

instance (Show k, Show v) => Show (BidirectionalMap k v) where
   show (BDMap m1 _) = "BDMap " ++ show m1

empty :: BidirectionalMap k v
empty = BDMap Map.empty Map.empty

insert :: (Ord k, Ord v) => 
   k -> v -> BidirectionalMap k v -> BidirectionalMap k v
insert k v (BDMap m1 m2) = BDMap (Map.insert k v m1) (Map.insert v k m2)

fromList :: (Ord k, Ord v) => [(k, v)] -> BidirectionalMap k v
fromList = foldr (uncurry insert) empty

toList :: BidirectionalMap k v -> [(k, v)]
toList (BDMap m1 _) = Map.toList m1
 
lookup :: Ord k => k -> BidirectionalMap k v -> Maybe v
lookup k (BDMap m1 _) = Map.lookup k m1

lookback :: Ord v => v -> BidirectionalMap k v -> Maybe k
lookback v (BDMap _ m2) = Map.lookup v m2

keys :: BidirectionalMap k v -> [k]
keys (BDMap m _) = Map.keys m

-- Monoid definition needs work, however:

instance (Ord k, Ord v) => Monoid (BidirectionalMap k v) where
   mempty = empty
   (BDMap ma1 ma2) `mappend` (BDMap mb1 mb2) =
      BDMap (ma1 `mappend` mb1) (ma2 `mappend` mb2)

{-- because:

*Main> mempty :: BidirectionalMap Int Char ~> BDMap fromList [] -- fine, but:
*Main> cats `mappend` fromList [("la-di-dah", Other)] ~>
BDMap fromList [("Confirm Debit",Confirm Debit),("Debit Denied",Debit Denied),
                ("Debit Scarcity",Debit Scarcity),("Deleted",Deleted),
                ("Difference Engine",Difference Engine),
                ("Flare Infons",Flare Infons),("Heat Debit",Heat Debit),
                ("Other",Other),("Universal Suggestion",Universal Suggestion),
                ("la-di-dah",Other)]

-- duplicate (reversed) Other value-key.
--}
