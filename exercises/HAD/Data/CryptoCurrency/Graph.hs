module Data.CryptoCurrency.Graph where

import System.Environment (getEnv)

import Graph.Query

coinGraphEndpoint :: IO Endpoint
coinGraphEndpoint = getEnv "COIN_DB_ACCESS"
