{-# LANGUAGE OverloadedStrings #-}

module CryptoCoin.CoinMarketCap.Types where

import Data.Aeson
import Data.Time

import Data.CryptoCurrency.Types
import CryptoCoin.CoinMarketCap.Types.Internal

import Data.XHTML (Name)

data MetaData = MetaData Status [ECoin]
   deriving (Eq, Ord, Show)

instance FromJSON MetaData where
   parseJSON = withObject "Metadata" $ \v ->
      MetaData <$> v .: "status" <*> (map raw2coin <$> v .: "data")

data Status = Status Day Int (Maybe String) Int Int (Maybe String)
   deriving (Eq, Ord, Show)

instance FromJSON Status where
   parseJSON = withObject "Status" $ \v ->
      Status <$> (readDate <$> v .: "timestamp") <*> v .: "error_code"
             <*> v .:? "error_message" <*> v .: "elapsed"
             <*> v .: "credit_count" <*> v .:? "notice"

readDate :: String -> Day
readDate = read . take 10

data Duration = Duration { first, last :: Day }
   deriving (Eq, Ord, Show)

mkdur :: String -> String -> Duration
mkdur f l = Duration (readDate f) (readDate l)

data CoinInfo = CoinInfo Idx Name Symbol String Bool Int Duration
   deriving (Eq, Ord, Show)

class CoinData a where
   info :: a -> CoinInfo

instance Rank CoinInfo where
   rank (CoinInfo _ _ _ _ _ r _) = r

instance Named CoinInfo where
   namei (CoinInfo _ n _ _ _ _ _) = n

instance Cymbal CoinInfo where
   sym (CoinInfo _ _ s _ _ _ _) = s

instance Indexed CoinInfo where
   idx (CoinInfo i _ _ _ _ _ _) = i

data Coin = Coin CoinInfo
   deriving (Eq, Ord, Show)

data Token = Token { coininf :: CoinInfo, coinRef :: Idx, token :: String }
   deriving (Eq, Ord, Show)

data ECoin = C Coin | T Token
   deriving (Eq, Ord, Show)

isToken :: ECoin -> Bool
isToken (T _) = True
isToken _     = False

instance CoinData ECoin where
   info (C (Coin cd)) = cd
   info (T (Token cd _ _)) = cd

instance Rank ECoin where
   rank = rank . info

instance Named ECoin where
   namei = namei . info

instance Indexed ECoin where
   idx = idx . info

instance Cymbal ECoin where
   sym = sym . info

raw2coin :: Coin' -> ECoin
raw2coin c@(Coin' _id _name _sym _slug _rank _activ _frist _lst Nothing) =
   C (Coin (mkci c))
raw2coin c@(Coin' _id _name _sym _slg _rnk _actv _fst _lst (Just (CR' i tok))) =
   T (Token (mkci c) i tok)

mkci :: Coin' -> CoinInfo
mkci (Coin' id name sym slug rank activ frist lst _) =
   CoinInfo id name sym slug (activ == 1) rank (mkdur frist lst)