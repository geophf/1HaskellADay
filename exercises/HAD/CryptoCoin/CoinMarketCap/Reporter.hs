{-# LANGUAGE OverloadedStrings #-}

module CryptoCoin.CoinMarketCap.Reporter where

import Data.Aeson (decode)

import Data.List (sortOn)

import qualified Data.ByteString.Lazy.Char8 as BL

import Data.Time (getCurrentTime)

import Data.CryptoCurrency.Types
import Data.XHTML

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
>>> let (Just (MetaData stats coins)) = it
>>> stats
Status 2021-02-25 0 Nothing 22 1 Nothing

>>> take 3 coins 
[C (Coin (CoinInfo 1 "Bitcoin" "BTC" "bitcoin" True 1 
         (Duration {first = 2013-04-28, last = 2021-02-25}))),
 C (Coin (CoinInfo 2 "Litecoin" "LTC" "litecoin" True 8 
         (Duration {first = 2013-04-28, last = 2021-02-25}))),
 C (Coin (CoinInfo 3 "Namecoin" "NMC" "namecoin" True 536 
         (Duration {first = 2013-04-28, last = 2021-02-25})))]
>>> take 3 $ filter isToken coins
[T (Token {coininf = CoinInfo 74 "Dogecoin" "DOGE" "dogecoin" True 14 
                              (Duration {first = 2013-12-15, last = 2021-02-25}), 
           coinRef = 1839, token = "0xba2ae424d960c26247dd6c32edc70b295c744c43"}),
 T (Token {coininf = CoinInfo 217 "Bela" "BELA" "belacoin" True 1921 
                              (Duration {first = 2014-03-20, last = 2021-02-25}), 
           coinRef = 1027, token = "0x2e98a6804e4b6c832ed0ca876a943abd3400b224"}),
 T (Token {coininf = CoinInfo 576 "GameCredits" "GAME" "gamecredits" True 694 
                     (Duration {first = 2014-09-01, last = 2021-02-25}), 
           coinRef = 1027, token = "0x63f88a2298a5c4aee3c216aa6d926b184a4b2437"})]
>>> length coins
4132

>>> length $ filter ((== 1) . is_active) coins
4132

>>> length $ filter isToken coins
2914

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
