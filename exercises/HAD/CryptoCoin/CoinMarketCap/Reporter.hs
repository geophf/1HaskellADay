{-# LANGUAGE OverloadedStrings #-}

module CryptoCoin.CoinMarketCap.Reporter where

import Data.Aeson

import qualified Data.ByteString.Lazy.Char8 as BL

import Data.List (sortOn)

import Data.Maybe (mapMaybe)
import qualified Data.Set as Set

import Data.Time

import Data.CryptoCurrency.Types
import Data.CryptoCurrency.Graph

import Data.Relation

import Data.XHTML

import Graph.Query
import Graph.JSON.Cypher

import CryptoCoin.CoinMarketCap.Types

ccdir :: FilePath
ccdir = "CryptoCoin/CoinMarketCap/rankings/2021/"

ccmapJSON :: String -> FilePath
ccmapJSON date = ccdir ++ "coins-" ++ date ++ ".json"

fetchMap :: FilePath -> IO (Maybe MetaData)
fetchMap file = decode <$> BL.readFile file

{--
>>> fetchMap (ccmapJSON "2021-02-22")
...
>>> let (Just (MD stats coins)) = it
>>> stats
Status "2021-02-22T19:57:10.205Z" 0 Nothing 24 1 Nothing

>>> take 3 coins 
[Coin' {id = 1, name = "Bitcoin", symbol = "BTC", slug = "bitcoin", rank = 1, 
        is_active = 1, first_historical_data = "2013-04-28T18:47:21.000Z", 
        last_historical_data = "2021-02-22T19:54:37.000Z", platform = Nothing},
 Coin' {id = 2, name = "Litecoin", symbol = "LTC", slug = "litecoin", rank = 8, 
        is_active = 1, first_historical_data = "2013-04-28T18:47:22.000Z", 
        last_historical_data = "2021-02-22T19:54:03.000Z", platform = Nothing},
 Coin' {id = 3, name = "Namecoin", symbol = "NMC", slug = "namecoin", 
        rank = 541, is_active = 1, 
        first_historical_data = "2013-04-28T18:47:22.000Z", 
        last_historical_data = "2021-02-22T19:54:02.000Z", platform = Nothing}]
>>> take 3 $ mapMaybe platform coins
[CR' 1839 "0xba2ae424d960c26247dd6c32edc70b295c744c43",
 CR' 1027 "0x2e98a6804e4b6c832ed0ca876a943abd3400b224",
 CR' 1027 "0x63f88a2298a5c4aee3c216aa6d926b184a4b2437"]
>>> length coins
4132

>>> length $ filter ((== 1) . is_active) coins
4132

>>> length $ filter ((/= Nothing) . platform ) coins
2870

Okay, so we have tokens and we have coins, and we have ranking of both,
and the ranking may change daily (?) ... let's graph these data.
--}

-- But, first! Let's do a Top-10, shall we?

instance Rasa ECoin where
   printRow (C (Coin ci)) = tr (ci2tr ci ++ [S "Coin"])
   printRow (T (Token ci _ _)) = tr (ci2tr ci ++ [S "Token"])

ci2tr :: CoinInfo -> [Content]
ci2tr (CoinInfo _i name sym _slug _activ rank _dur) =
   [S (show rank), S name, S sym]

go :: IO ()
go = getCurrentTime >>= ranking . take 10 . show

ranking :: String -> IO ()
ranking date =
   fetchMap (ccmapJSON date) >>= \metadata ->
   let (Just (MetaData stats ecoins)) = metadata
   in  header date >> report (take 10 (sortOn rank ecoins))

header :: String -> IO ()
header date =
   putStrLn (unwords ["<p>The top-10 e-coins for",date,
                      "(ranked by",
                      "<a href='https://coinmarketcap.com'>coinmarketcap.com</a>)",
                      "are:</p>"])

report :: [ECoin] -> IO ()
report =
   flip printContent 3 . E
      . tabulate [Attrib "border" "1"] [thdrs (words "Rank Name Symbol Type")]

{--
>>> let date = "2021-02-22"
>>> let ecoins = map raw2coin coins
>>> ranking date ecoins
<p>The top-10 e-coins for 2021-02-22 (ranked by 
   <a href='https://coinmarketcap.com'>coinmarketcap.com</a> ) are:</p>
   <table border="1">
    <tr><th align="left">Rank</th><th align="left">Name</th>
        <th align="left">Symbol</th><th align="left">Type</th>
    </tr>
    <tr><td>1</td><td>Bitcoin</td><td>BTC</td><td>Coin</td></tr>
...
--}
