module Y2017.M06.D16.Solution where

import System.Environment

{--
Okay, we're going to start talking with the outside world, that is, beyond
the age-old putStrLn and print.

So, data-stores, DaaS, all that. But part of that whole infrastructure is
putting your username and password in place where people don't see that in
your source code.

Today's Haskell problem.

1. If you don't have a SQL DaaS, start thinking about getting one, or, alter-
natively, use your local SQL database for next week, if you have one of those.

2. Download and install the Persistent cabal framework for Haskell.

3. For today, pretend you have a username and password and a connection URL
written down on a post-it note, because that's Industry-standard.

In your OS environment, create the variables SQL_DAAS_USERNAME,
SQL_DAAS_PASSWORD, SQL_DAAS_SERVER_URL, and SQL_DAAS_DB_NAME, populating the 
values.
--}

dbUserName, dbPassword, dbmsServer, dbName :: IO String
dbUserName = getEnv "SQL_DAAS_USERNAME"
dbPassword = getEnv "SQL_DAAS_PASSWORD"
dbmsServer = getEnv "SQL_DAAS_SERVER_URL"
dbName = getEnv "SQL_DAAS_DB_NAME"

dbPort :: IO Int
dbPort = read <$> getEnv "SQL_DAAS_SERVER_PORT"

{-- 
>>> dbUserName
"geophf"
>>> dbPassword
"youWish"
>>> dbmsServer
"pellefant.db.elephantsql.com"
>>> dbName
"1HaskellADay"
>>> dbPort
5432

Okay, using the above results, if your connection string is in the format:

postgres://user:pass@server:5432/dbname

have the below function take the above as arguments and output that format.
--}

connectionString :: String -> String -> String -> Int -> String -> String
connectionString username pass server port db =
  "postgres://" ++ username ++ (foldr (\(a,b) c -> a:b ++ c) ""
         $ zip ":@:/" [pass, server, show port, db])

{--
>>> connectionString <$> dbUserName <*> dbPassword <*> dbmsServer <*> pure 5432 <*> dbName
"postgres://geophf:youWish@pellefant.db.elephantsql.com:5432/1HaskellADay"

Next week we'll be connecting to and interacting with a SQL DBMS using 
Database.Persist.
--}

connectionStringFromEnv :: IO String
connectionStringFromEnv =
   connectionString <$> dbUserName <*> dbPassword <*> dbmsServer
                                   <*> dbPort     <*> dbName

-- moving this solution to Database.Connection
