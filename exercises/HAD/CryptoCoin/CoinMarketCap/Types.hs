{-# LANGUAGE OverloadedStrings #-}

module CryptoCoin.CoinMarketCap.Types where

import Data.Aeson
import Data.Time

import Data.CryptoCurrency.Types
import CryptoCoin.CoinMarketCap.Types.Internal

import Data.XHTML

data MetaData = MetaData Status [ECoin]   -- used to be Coin'
   deriving (Eq, Ord, Show)

instance FromJSON MetaData where
   parseJSON = withObject "Metadata" $ \v ->
      MetaData <$> v .: "status" <*> (map raw2coin <$> v .: "data")

data Status = Status String Int (Maybe String) Int Int (Maybe String)
   deriving (Eq, Ord, Show)

instance FromJSON Status where
   parseJSON = withObject "Status" $ \v ->
      Status <$> v .: "timestamp" <*> v .: "error_code"
             <*> v .:? "error_message" <*> v .: "elapsed"
             <*> v .: "credit_count" <*> v .:? "notice"

{--
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
--}

data Duration = Duration { first, last :: Day }
   deriving (Eq, Ord, Show)

mkdur :: String -> String -> Duration
mkdur f l = Duration (read f) (read l)

data CoinInfo = CoinInfo Idx Name Symbol String Bool Int Duration
   deriving (Eq, Ord, Show)

class CoinData a where
   info :: a -> CoinInfo

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

{--
instance Rasa ECoin where
   printRow (C (Coin ci)) = tr (ci2tr ci ++ [S "Coin"])
   printRow (T (Token ci _ _)) = tr (ci2tr ci ++ [S "Token"])

ci2tr :: CoinInfo -> [Content]
ci2tr (CoinInfo _i name sym _slug _activ rank _dur) =
   [S (show rank), S name, S sym]
--}

raw2coin :: Coin' -> ECoin
raw2coin c@(Coin' _id _name _sym _slug _rank _activ _frist _lst Nothing) =
   C (Coin (mkci c))
raw2coin c@(Coin' _id _name _sym _slg _rnk _actv _fst _lst (Just (CR' i tok))) =
   T (Token (mkci c) i tok)

mkci :: Coin' -> CoinInfo
mkci (Coin' id name sym slug rank activ frist lst _) =
   CoinInfo id name sym slug (activ == 1) rank (mkdur frist lst)

