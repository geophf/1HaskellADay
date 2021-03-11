{-# LANGUAGE ViewPatterns #-}

module CryptoCoin.CoinMarketCap.Reporter where

import Data.Aeson (decode)

import qualified Data.ByteString.Lazy.Char8 as BL

import Data.List (sortOn, splitAt)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Time

import System.Environment (getEnv)
import System.Process (readProcess)

import Data.CryptoCurrency.Types
import Data.XHTML

import CryptoCoin.CoinMarketCap.Types
import CryptoCoin.CoinMarketCap.State.RankMatrix (rankMatrix)
import CryptoCoin.CoinMarketCap.Analytics.N00Bs (noobsFor, N00B(N00B))

ccfile :: FilePath
ccfile = "rankings/2021/coins-"

ccmapJSON :: Day -> IO FilePath
ccmapJSON (take 10 . show -> date) =
   (++ ('/':ccfile ++ date ++ ".json")) <$> getEnv "COIN_MARKET_CAP_DIR"

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
ci2tr c@(CoinInfo _i name sym slug _activ rank _dur) =
   [S (show rank), link c, S sym]

link :: CoinInfo -> Content
link (CoinInfo _i name _sym slug _activ _rank _dur) =
   a (coinmarketcapcoinlink slug) name

a :: String -> String -> Content
a href = E . Elt "a" [Attrib "href" href] . return . S

coinmarketcaphref :: String
coinmarketcaphref = "https://coinmarketcap.com"

coinmarketcapcoinlink :: String -> String
coinmarketcapcoinlink = ((coinmarketcaphref ++ "/currencies/") ++) . (++ "/")

go :: IO ()
go = getCurrentTime >>= \date ->
     let day = utctDay date
     in  ranking day >>= tweet day >> title day

ranking :: Day -> IO [ECoin]
ranking date =
   ccmapJSON date >>=
   fetchMap >>= \metadata ->
   let (Just md@(MetaData stats ecoins)) = metadata
   in  header date                                      >>
       report (take 10 (sortOn rank $ Map.elems ecoins)) >>
       newCoins md date

header :: Day -> IO ()
header (take 10 . show -> date) =
   printContent (p [S (unwords ["The top-10 e-coins for",date,"(ranked by"]),
                    a coinmarketcaphref "coinmarketcap.com",
                    S "are:"]) 0

report :: [ECoin] -> IO ()
report =
   flip printContent 3 . E
      . tabulate [Attrib "border" "1"] [thdrs (words "Rank Name Symbol Type")]

newCoins :: MetaData -> Day -> IO [ECoin]
newCoins mdata date =
   let coins  = noobsFor mdata rankMatrix date
       sz     = Set.size coins
       header = concat ["There are ",show sz," new coin",plural sz," today:"]
       ranked = sortOn rank $ map (\(N00B coin _) -> coin) (Set.toList coins)
   in  printContent (p [S header]) 0 >>
       report ranked                 >>
       return ranked

plural :: Int -> String
plural 1 = ""
plural _ = "s"

p :: [Content] -> Content
p = E . Elt "p" []

{--
>>> let date = (read "2021-02-22") :: Day
>>> ranking date coins
<p>The top-10 e-coins for 2021-02-22 (ranked by 
   <a href='https://coinmarketcap.com'>coinmarketcap.com</a> ) are:</p>
   <table border="1">
    <tr><th align="left">Rank</th><th align="left">Name</th>
        <th align="left">Symbol</th><th align="left">Type</th>
    </tr>
    <tr><td>1</td><td>Bitcoin</td><td>BTC</td><td>Coin</td></tr>
...

need to add a twitter poster here, too.
--}

tweet :: Day -> [ECoin] -> IO ()
tweet today newCoins =
   let url = "http://logicalgraphs.blogspot.com/2021/03/top-10-e-coins-for-"
       day = show today
       urlday = url ++ day ++ ".html"
       ncoins = length newCoins
   in  putStrLn (unwords ["The top-10 e-coins for", day, "with",
                          show ncoins, "new coin" ++ plural ncoins, "for",
                          "today are archived at", urlday, "#cryptocurrency "])

title :: Day -> IO ()
title = putStrLn . unwords . ("Top-10 E-coins for":) . return . show 
