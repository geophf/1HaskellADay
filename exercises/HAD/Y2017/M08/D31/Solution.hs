module Y2017.M08.D31.Solution where

import Control.Arrow ((&&&))
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord
import Data.Ratio ((%))
import Network.HTTP.Conduit

-- below imports available via 1HaskellADay git repository

import Data.Percentage

-- Today's exercise comes by way of the tweep, @ahnqir.

url :: FilePath
url = "https://raw.githubusercontent.com/geophf/1HaskellADay/master/exercises/HAD/Y2017/M08/D31/fb-users.txt"

{--
The above URL contains the top 10 facebook user-counts by country.

Read in the above file, parse it, then output the percentage of facebook
users per country (with the narrowed focus that the top 10 countries comprise
'all' (most, actually) of the facebook users, so we're 'smapling' the entire
(100%) set.
--}

type Country = String
type Count = Integer
type FBusers = Map Country

parseMillions :: String -> Integer
parseMillions = (* 1000000) . read . init

readFBusers :: FilePath -> IO (FBusers Count)
readFBusers = fmap (Map.fromList
                  . map ((head &&& parseMillions . last) . words)
                  . drop 3 . lines . BL.unpack)
            . simpleHttp

{--
>>> readFBusers url
fromList [("Brazil",139000000),("India",241000000),("Indonesia",126000000),
          ("Mexico",85000000),("Philippines",69000000),("Thailand",57000000),
          ("Turkey",56000000),("UK",44000000),("USA",240000000),
          ("Vietnam",64000000)]
--}

percentageFBusersByCountry :: FBusers Count -> FBusers Percentage
percentageFBusersByCountry counts =
   let total = sum (Map.elems counts) in
   Map.map (P . (% total)) counts

{--
>>> percentageFBusersByCountry <$> readFBusers url
fromList [("Brazil",12.39%),("India",21.49%),("Indonesia",11.23%),
          ("Mexico",7.58%),("Philippines",6.15%),("Thailand",5.08%),
          ("Turkey",4.99%),("UK",3.92%),("USA",21.40%),("Vietnam",5.70%)]
--}

{-- BONUS -----------------------------------------------------------------

Using whatever charting software you like, chart the facebook users (count)
by country.

--}

-- one way to do this is to output it as CSV and let your spreadsheet chart it

chartFBusers :: Ord a => Show a => FilePath -> FBusers a -> IO ()
chartFBusers outputfile =
   writeFile outputfile
           . unlines . ("Country,FB Users":)
           . map tup2line . sortOn (Down . snd)
           . Map.toList
      where tup2line (country,count) = country ++ (',':show count)

{--
>>> readFBusers url >>= chartFBusers "Y2017/M08/D31/chart.csv"
--}
