module Y2017.M08.D31.Exercise where

import Data.Map (Map)
import Network.HTTP.Conduit

-- below import available via 1HaskellADay git repository

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
type Count = Int
type FBusers = Map Country

readFBusers :: FilePath -> IO (FBusers Count)
readFBusers url = undefined

percentageFBusersByCountry :: FBusers Count -> FBusers Percentage
percentageFBusersByCountry counts = undefined

{-- BONUS -----------------------------------------------------------------

Using whatever charting software you like, chart the facebook users (count)
by country.

--}

chartFBusers :: FBusers Count -> IO ()
chartFBusers counts = undefined
