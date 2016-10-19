module Y2016.M10.D17.Solution where

import Control.Arrow ((&&&), (>>>))
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (getSum)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time

import Analytics.Trading.Scan.Top5s -- http://lpaste.net/1443717939034324992

-- below import availabe from 1HaskellADay git repository

import Control.Logic.Frege (adjoin, assert, (<<-))
import Data.Bag

{--
In observing the top 5s of the stock markets, I've started to see some patterns
in securities over time that may be interesting.

Do you see the same things?

Today's Haskell exercise. There is a file of the top5s securities of the stock
markets included here at this directory as top5s.csv (it's not truly csv) or
at the URL:

https://raw.githubusercontent.com/geophf/1HaskellADay/master/exercises/HAD/Y2016/M10/D17/top5s.csv

Today, we're interested in looking at the leaders and the losers in the Price
category over a period of days.

Read in the file, extract the stocks for the prices, losers and leaders, and
answer the below
--}

type Stock = String

data Prices = Prices { leaders, losers :: Set Stock }
   deriving Show

-- so, read in the price line, remembering the date for it (a few lines up)
-- and then output a map of days -> prices

type Hist = Map Day Prices

readPrices :: FilePath -> IO Hist
readPrices = fmap (Map.map onlyPrices) . top5sHistory 

onlyPrices :: Top5s -> Prices
onlyPrices = uncurry Prices . adjoin Set.fromList . (Map.! Price)

-- *Y2016.M10.D17.Solution> readPrices "Y2016/M10/D17/top5s.csv" ~> hist

-- Now.

-- what is the distribution of stocks and their appearances on the top5s prices?

type NStocks = Int
type NDays = Integer

dist :: Hist -> (NStocks, NDays)
dist =
   length . Set.unions . map (losers &&& leaders >>> uncurry Set.union) . Map.elems &&&
   uncurry diffDays . (maximum &&& minimum) . Map.keys

-- dist is really two functions, but, okay, whatever.

-- *Y2016.M10.D17.Solution> dist hist ~> (1165,519)
-- 1165 unique stocks collected over 519 days

-- What stocks show up on the leaders one day and the losers the next day?

leadThenLose :: Hist -> [Stock]
leadThenLose = iter leaders losers . Map.elems

-- well, let's break this problem down. What is a leader yesterday that
-- is a loser today?

yesterdayToday :: Ord a => Set a -> Set a -> [a]
yesterdayToday yester to = 

-- Let's break that down even further. For a given stock, does it show up
-- in the next day's opposite set?

-- This is an easy problem to solve: it's Set.member. So, then we have a foreach

   toList yester >>= assert (flip Set.member to)

-- my iteration function is as follows:

iter :: Ord a => (Prices -> Set a) -> (Prices -> Set a) -> [Prices] -> [a]
iter _ _ [_] = []
iter f g (h:t@(n:_)) = yesterdayToday (f h) (g n) ++ iter f g t

{--
We don't really need the following function. All we do is iterate through the
history as a (chronologically ascending) list

-- Of course, you need a 'next trading day' defined, because Saturdays, Sundays
-- and holidays (some of them) are not traded, therefore not recorded

-- (but this may be handled by Map's LT GT lookup functions)

nextTradingDay :: Hist -> Day -> Day
nextTradingDay hist today = undefined
--}

{--
*Y2016.M10.D17.Solution> leadThenLose hist ~>
["CLLS","GSAT","IRCP","MCRB","LTRPB","LINE","MOG.B","ESPR","VXX","VXX",
 "THRX","HRTX","CXRX","NAV-D","BOFI","WTW","NM-G","KMT","EPE","WMB","MOG.B",
 "LMCB","SDRL","ZBRA","RLYP","NUGT","SID","AMD","DNR","LC","ACIA","BETR",
 "SID","SCWX","GSAT","SWN","HOG","DNR","SAGE","UWTI","YRD","CHRS","YRD",
 "NAV-D","TPL","SRPT","CVRR","CYH","HMY","SDRL","NTNX"]

Interesting (to me) is some of these values recur.
--}

-- Opposite question: which stocks show up on the losers bracket then the winners?

loseThenLead :: Hist -> [Stock]
loseThenLead = iter losers leaders . Map.elems

{--
*Y2016.M10.D17.Solution> iter losers leaders $ Map.elems hist
["NBG","SHI","BCOM","NK","CXRX","CXRX","CROX","AXON","BLUE","WMB","AVP",
 "LPLA","ZBRA","VNET","SCWX","SM","STRZB","VXX","FMSA","TSRO","HIMX","AG",
 "NUGT","YRD","GEO","UWTI","SDRL","CLVS"]

Some of the same values. I wonder if, e.g. NUGT lead then lost then lead?
--}

-- what is the frequency of appearance of these flippin' stocks?

showings :: Bag Stock -> Stock -> Int
showings = getSum <<- (Map.!)

-- the easiest way for me to go abouot this is to convert hist into a bag

hist2bag :: Hist -> Bag Stock
hist2bag = foldr add emptyBag
         . concatMap (losers &&& leaders >>> uncurry Set.union >>> toList)
         . Map.elems

-- and then just get the bag count for the stock, eh?

-- that is to say, does a leadThenLose show up frequently on the top5s list?
-- How about a loseThenLead stock?

{--
*Y2016.M10.D17.Solution> Set.fromList (map (id &&& showings bag) (leadThenLose hist)) ~>
fromList [("ACIA",12),("AMD",12),("BETR",2),("BOFI",4),("CHRS",5),("CLLS",6),
          ("CVRR",5),("CXRX",16),("CYH",6),("DNR",11),("EPE",21),("ESPR",6),
          ("GSAT",15),("HMY",9),("HOG",3),("HRTX",5),("IRCP",15),("KMT",2),
          ("LC",7),("LINE",9),("LMCB",26),("LTRPB",22),("MCRB",7),("MOG.B",15),
          ("NAV-D",25),("NM-G",14),("NTNX",4),("NUGT",39),("RLYP",4),("SAGE",5),
          ("SCWX",3),("SDRL",12),("SID",18),("SRPT",11),("SWN",13),("THRX",2),
          ("TPL",3),("UWTI",22),("VXX",24),("WMB",7),("WTW",5),("YRD",15),
          ("ZBRA",11)]

*Y2016.M10.D17.Solution> Set.fromList (map (id &&& showings bag) (loseThenLead hist)) ~>
fromList [("AG",7),("AVP",8),("AXON",13),("BCOM",11),("BLUE",9),("CLVS",8),
          ("CROX",2),("CXRX",16),("FMSA",6),("GEO",2),("HIMX",11),("LPLA",3),
          ("NBG",4),("NK",9),("NUGT",39),("SCWX",3),("SDRL",12),("SHI",6),
          ("SM",4),("STRZB",12),("TSRO",4),("UWTI",22),("VNET",3),("VXX",24),
          ("WMB",7),("YRD",15),("ZBRA",11)]
--}
