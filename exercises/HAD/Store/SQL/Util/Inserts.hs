module Store.SQL.Util.Inserts where

-- A set of utility functions around inserting rows into a SQL store.

import Control.Monad (void)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Map (Map)
import qualified Data.Map as Map
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField

-- below import available via 1HaskellADay git repository

import Control.Logic.Frege ((<<-))

-- Your standard "just insert these rows"-function

inserter :: ToRow a => Connection -> Query -> [a] -> IO ()
inserter conn = void <<- executeMany conn

-- And conversion of byte strings for database insertion:

byteStr :: ByteString -> String
byteStr = BL.unpack

-- a couple of toField functions

look r m = toField . Map.lookup r . m
byt f    = toField . byteStr . f
