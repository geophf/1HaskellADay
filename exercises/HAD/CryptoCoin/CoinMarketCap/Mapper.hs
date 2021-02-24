{-# LANGUAGE OverloadedStrings #-}

module CryptoCoin.CoinMarketCap.Mapper where

import Data.Aeson

import qualified Data.ByteString.Lazy.Char8 as BL

import Data.List (sortOn)

import Data.Maybe (mapMaybe)
import qualified Data.Set as Set

import Data.Time

import Data.CryptoCurrency.Types
import Data.CryptoCurrency.Graph

import Data.Relation

import Data.XHTML hiding (Name)

import Graph.Query
import Graph.JSON.Cypher

ccdir :: FilePath
ccdir = "CryptoCoin/CoinMarketCap/rankings/2021/"

ccmapJSON :: String -> FilePath
ccmapJSON date = ccdir ++ "coins-" ++ date ++ ".json"

data MetaData = MD Status [Coin']
   deriving (Eq, Ord, Show)

instance FromJSON MetaData where
   parseJSON = withObject "Metadata" $ \v ->
      MD <$> v .: "status" <*> v .: "data"

data Status = Status String Int (Maybe String) Int Int (Maybe String)
   deriving (Eq, Ord, Show)

instance FromJSON Status where
   parseJSON = withObject "Status" $ \v ->
      Status <$> v .: "timestamp" <*> v .: "error_code"
             <*> v .:? "error_message" <*> v .: "elapsed"
             <*> v .: "credit_count" <*> v .:? "notice"

data Coin' = Coin' { id :: Idx, name, symbol, slug :: String,
                     rank', is_active :: Int,
                     first_historical_data, last_historical_data :: String,
                     platform :: Maybe CoinRef' }
   deriving (Eq, Ord, Show)

instance FromJSON Coin' where
   parseJSON = withObject "Raw coin" $ \v ->
      Coin' <$> v .: "id" <*> v .: "name" <*> v .: "symbol" <*> v .: "slug"
            <*> v .: "rank" <*> v .: "is_active"
            <*> v .: "first_historical_data" <*> v .: "last_historical_data"
            <*> v .:? "platform"

type TokenAddress = String

data CoinRef' = CR' Idx TokenAddress           -- indicating Token?
   deriving (Eq, Ord, Show)

instance Indexed CoinRef' where
   idx (CR' i _) = i

instance FromJSON CoinRef' where
   parseJSON = withObject "ref" $ \v ->
      CR' <$> v .: "id" <*> v .: "token_address"

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

data Duration = Duration { first, last :: Day }
   deriving (Eq, Ord, Show)

mkdur :: String -> String -> Duration
mkdur f l = Duration (read f) (read l)

data CoinInfo = CoinInfo Idx Name Symbol String Bool Int Duration
   deriving (Eq, Ord, Show)

class CoinData a where
   info :: a -> CoinInfo

class Rank a where
   rank :: a -> Int

instance Rank CoinInfo where
   rank (CoinInfo _ _ _ _ _ r _) = r

data Coin = Coin CoinInfo
   deriving (Eq, Ord, Show)

data Token = Token { coininf :: CoinInfo, coinRef :: Idx, token :: String }
   deriving (Eq, Ord, Show)

data ECoin = C Coin | T Token
   deriving (Eq, Ord, Show)

instance CoinData ECoin where
   info (C (Coin cd)) = cd
   info (T (Token cd _ _)) = cd

instance Rank ECoin where
   rank = rank . info

instance Rasa ECoin where
   printRow (C (Coin ci)) = tr (ci2tr ci ++ [S "Coin"])
   printRow (T (Token ci _ _)) = tr (ci2tr ci ++ [S "Token"])

ci2tr :: CoinInfo -> [Content]
ci2tr (CoinInfo _i name sym _slug _activ rank _dur) =
   [S (show rank), S name, S sym]

raw2coin :: Coin' -> ECoin
raw2coin c@(Coin' _id _name _sym _slug _rank _activ _frist _lst Nothing) =
   C (Coin (mkci c))
raw2coin c@(Coin' _id _name _sym _slg _rnk _actv _fst _lst (Just (CR' i tok))) =
   T (Token (mkci c) i tok)

mkci :: Coin' -> CoinInfo
mkci (Coin' id name sym slug rank activ frist lst _) =
   CoinInfo id name sym slug (activ == 1) rank (mkdur frist lst)

-- But, first! Let's do a Top-10, shall we?

ranking :: String -> [ECoin] -> IO ()
ranking date coins =
   header date >> report (take 10 (sortOn rank coins))

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
<p>The top-10 e-coins for 2021-02-22 (ranked by <a href='https://coinmarketcap.com'>coinmarketcap.com</a> ) are:</p>
   <table border="1">
    <tr>
     <th align="left">
      Rank
     </th>
     <th align="left">
      Name
     </th>
     <th align="left">
      Symbol
     </th>
     <th align="left">
      Type
     </th>
    </tr>
    <tr>
     <td>
      1
     </td>
     <td>
      Bitcoin
     </td>
     <td>
      BTC
     </td>
     <td>
      Coin
     </td>
    </tr>
...
--}
