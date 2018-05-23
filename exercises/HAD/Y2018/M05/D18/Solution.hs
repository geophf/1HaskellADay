{-# LANGUAGE OverloadedStrings #-}

module Y2018.M05.D18.Solution where

import Data.Aeson
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Time.Clock

import Database.PostgreSQL.Simple

import Network.HTTP.Conduit

-- below imports available via 1HaskellADay git repository

import Data.LookupTable

import Store.SQL.Connection
import Store.SQL.Util.Logging

-- Before we insert new articles, let's step back in time for a moment. Recall

import Y2018.M04.D06.Solution (cats2lk)
import Y2018.M04.D09.Solution

{--
where we uploaded tags, we uploaded a sample set of 10 tags. There are a lot
more tags than 10 for the World Policy Journal. Today we're going to find out
how many.

The REST endpoint for the tags is as follows:
--}

import Y2018.M04.D11.Solution (PageNumber)
import Y2018.M04.D13.Solution (Tries)

tagEndpoint :: PageNumber -> FilePath
tagEndpoint pn = "https://worldpolicy.org/wp-json/wp/v2/tags?per_page=100&page="
   ++ show pn

-- The JSON format we already know and parse (see above exercise) and the
-- tags are exhausted when the Value returned is the empty list.

-- How many tags are there for this REST endpoint?

downloadTags :: IO [Tag]
downloadTags = tr' 0 [] 1

-- You may want to use some pagination function to help define downloadTags

-- hint: see definitions in Y2018.M04.D13 for downloading packets and use
-- a similar approach

tr' :: Tries -> [Tag] -> PageNumber -> IO [Tag]
tr' tries accum pn =
   if   tries > 3
   then error ("Tried three times at offset " ++ show pn ++ "; quitting")
   else tagReader 60 pn >>=
        either (accumTags (succ pn) accum) (logErr pn (succ tries) accum)

tagReader :: Int -> PageNumber -> IO (Either [Tag] String)
tagReader secs pn =
   newManager tlsManagerSettings >>= \mgr ->
   parseRequest (tagEndpoint pn) >>= \req ->
   let req' = req { responseTimeout = responseTimeoutMicro (secs * 1000000) } in
   httpLbs req' mgr >>= return . eitherify decode . responseBody

eitherify :: (ByteString -> Maybe [Tag]) -> ByteString
          -> Either [Tag] String
eitherify f str = case f str of
   Just tags -> Left tags
   Nothing   -> Right (BL.unpack str)

accumTags :: PageNumber -> [Tag] -> [Tag] -> IO [Tag]
accumTags n accum [] = return accum
accumTags n accum tags@(_:_) = tr' 0 (accum ++ tags) n

logErr :: PageNumber -> Tries -> [Tag] -> String -> IO [Tag]
logErr pn tr accum msg =
   putStrLn ("Failed on on tag-loader page " ++ show pn ++ ", try: " ++ show tr
             ++ ", with string " ++ msg) >>
   tr' tr accum pn

{--
>>> tags <- downloadTags

... (after 30 seconds) ...

>>> length tags
5916

>>> mapM_ (print . (idx &&& mouse . val)) (take 5 tags)
(8813,"#MeToo")
(8511,"#OneArctic Symposium")
(3904,"12x12")
(4095,"18th National Congress")
(4133,"18th Party Congress")
--}

{-- BONUS -----------------------------------------------------------------

Create an application that downloads all the tags from the REST endpoint and
then uploads those tags to a PostgreSQL data table as described in the module
Y2018.M04.D09.
--}

main' :: [String] -> IO ()
main' [] =
   putStrLn "Initialzing tag-loader for WPJ" >>
   downloadTags >>= \tags ->
   withConnection WPJ (\conn -> initLogger conn >>= uploadTags conn tags) >>
   putStrLn "Populated database with WPJ tags."

uploadTags :: Connection -> [Tag] -> LookupTable -> IO ()
uploadTags conn tags sevlk =
   roff' conn ("Downloaded " ++ show (length tags)
                     ++ " tags from WPJ REST enpoint") >>
   insertTags conn (cats2lk tags)
      where roff' = mkInfo "Tag uploader" "Y2018.M05.D18.Solution" sevlk

go :: IO ()
go = getCurrentTime >>= \start ->
     main' [] >>
     getCurrentTime >>=
     putStrLn . ("Executed in " ++) . (++ " seconds") . show
              . flip diffUTCTime start

{-- BONUS-BONUS ------------------------------------------------------------

Create an application that does the same thing, but for categories this time.
See Y2018.M04.D06 for information on categories (very (VERY)) much like tags.
--}
