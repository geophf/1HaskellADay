module CryptoCoin.CoinMarketCap.TestBed where

import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Time

import CryptoCoin.CoinMarketCap.Reporter
import CryptoCoin.CoinMarketCap.Ranker

foo :: ()
foo = ()

{--
>>> mapM (fetchMap . ccmapJSON)
         (map (take 10 . show)
              ([read "2021-02-22" .. read "2021-02-28"] :: [Day])) >>=
    writeMatrix "CryptoCoin/CoinMarketCap/State/RankMatrix.hs" 
              . foldl matrix Map.empty . catMaybes 

$ ghci CryptoCoin/CoinMarketCap/State/RankMatrix.hs

>>> length rankMatrix 
5
>>> Map.keys rankMatrix 
[2021-02-22,2021-02-24,2021-02-25,2021-02-26,2021-02-28]
>>> Map.map length rankMatrix 
fromList [(2021-02-22,4132),(2021-02-24,4176),(2021-02-25,4182),
          (2021-02-26,4194),(2021-02-28,4199)]

So, it looks like we need to add newCurrencies to our reporting tool,
which now also needs to know about rankMatrix.
--}
