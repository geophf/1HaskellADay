module Store.SQL.Connection where

{--
We're basically rewriting Store.SQL.Connection. Good module, if you only have
one database to manage, but now I have multiple SQL databases, so I have to
make all these functions confirgurable.
--}

import Database.PostgreSQL.Simple
import System.Environment

-- let's codify which databases we're talking about here:

data Database = WPJ | PILOT | ARCHIVE | ENTITIES | ECOIN
   deriving (Eq, Show)

-- these functions get your database's information from the environment

dbUserName, dbPassword, dbmsServer, dbName :: Database -> IO String
dbUserName = ge "USERNAME"
dbPassword = ge "PASSWORD"
dbmsServer = ge "SERVER_URL"
dbName = ge "DB_NAME"

ge :: String -> Database -> IO String
ge str dbname = getEnv ("SQL_DAAS_" ++ str ++ ('_':show dbname))

dbPort :: Database -> IO Int
dbPort = fmap read . ge "SERVER_PORT"

-- and with those we can do this:

connectInfo :: Database -> IO ConnectInfo
connectInfo dbname = ConnectInfo <$> dbmsServer dbname
                          <*> (fromIntegral <$> dbPort dbname)
                          <*> dbUserName dbname <*> dbPassword dbname
                          <*> dbName dbname

-- and with that we can do this:

withConnection :: Database -> (Connection -> IO a) -> IO ()
withConnection db fn =
   connectInfo db >>= connect >>= \conn -> fn conn >> close conn

-- with all that now connect to your database and do something cutesies
-- ('cutesies' is a technical term)

-- this module will replace Store.SQL.Connection
