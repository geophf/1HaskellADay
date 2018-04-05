module Y2018.M04.D05.Exercise where

{--
We're basically rewriting Store.SQL.Connection. Good module, if you only have
one database to manage, but now I have multiple SQL databases, so I have to
make all these functions confirgurable.
--}

import Control.Monad ((>=>))
import Database.PostgreSQL.Simple
import System.Environment

-- these functions get your database's information from the environment

dbUserName, dbPassword, dbmsServer, dbName :: String -> IO String
dbUserName = undefined
dbPassword = undefined
dbmsServer = undefined
dbName = undefined

-- this helper function may help ...

ge :: String -> String -> IO String
ge str dbname = getEnv ("SQL_DAAS_" ++ str ++ ('_':dbname))

dbPort :: String -> IO Int
dbPort = undefined

-- and with those we can do this:

connectInfo :: String -> IO ConnectInfo
connectInfo dbname = ConnectInfo <$> dbmsServer dbname
                          <*> (fromIntegral <$> dbPort dbname)
                          <*> dbUserName dbname <*> dbPassword dbname
                          <*> dbName dbname

-- and with that we can do this:

withConnection :: String -> (Connection -> IO a) -> IO ()
withConnection dbname fn = undefined

-- with all that now connect to your database and do something cutesies
-- ('cutesies' is a technical term)
