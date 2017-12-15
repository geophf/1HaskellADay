{-# LANGUAGE OverloadedStrings #-}

module Y2017.M12.D15.Solution where

import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP (urlEncode)
import System.Directory

{--
New topic!

Today we're going to create an oembed service where we serve some simple
resources, like, goats, ... because I like kids; they are SO CUTE!

So, there are three things that make up a kids oembed service:

1. the resources themselves. They are here at this directory
--}

-- below import available via 1HaskellDay git repository

import Control.DList
import Data.XHTML

goatsDir :: FilePath
goatsDir = "Y2017/M12/D13/goats/"

{--
... but, actually, you can defer that to the resource, itself, if you have
a repository of your assets, so, let's make that today's exercise, instead.

Create a repository of the goats at that inode so that it returns your URL
concatenated with the oembed request for the specific resource.

... so that also means you have to know how to extract files from an inode.

--}

goatsFiles :: FilePath -> IO [FilePath]
goatsFiles = fmap (filter ((/= '.') . head)) . getDirectoryContents

{--
>>> goatsFiles goatsDir 
["grace-n-goats.jpg","goat-small-town.jpg","CjPcyoUUoAAo5li.jpg",
 "CjPcyySWUAE2-E1.jpg","goats3.png","goats2.png","goats1.png","goats-deux.png",
 "goat-kid-sweater.jpg","ChIBH-DU0AE7Qzt.jpg","too-close.jpg",
 "CYVG4BoWwAAKfSt.jpg","goatz.jpg","CFsge-vUMAIUmW4.jpg","CFujXdtUIAAk_HW.jpg",
 "goat-india.jpg"]
--}

-- return the path to the goats files prepended to only the goats files,
-- themselves (so, no "." or "..", etc, if you please)

{--
Now from those files, create a webpage that says something fetching, like,

HERE ARE THE GOATS!

And then list the links to the assets that, when selected, makes the oembed
request to your web server. At your URL. That you (will) have.

What does an oembed request look like. Let's look at a sample:

http://www.flickr.com/services/oembed/?format=json&url=http%3A//www.flickr.com/photos/bees/2341623661/

A request is composed of several parts. 
--}

-- 1. the uri of the (specific) oembed service

rootURI :: FilePath
rootURI = "http://127.0.0.1:8080/"

oembedService :: FilePath
oembedService = rootURI ++ "services/oembed.php"

-- 2. the query string which includes the url and which (may or may not)
--    include the format

data Format = JSON
   deriving Eq

instance Show Format where
   show = const "json"

query :: Maybe Format -> FilePath -> String
query format relativeURI =
   '?': fromJust (((++ "&") . ("format=" ++) . show <$> format) <> Just "")
   ++ "url=" ++ urlEncode (rootURI ++ relativeURI)

{--
>>> query (Just JSON) "bleh"
"?format=json&url=http://127.0.0.1:8080/bleh"
>>> query Nothing "bleh"
"?url=http://127.0.0.1:8080/bleh"
--}

-- and with that, you can create your webpage with your goatsFiles links:

goatsWebPage :: [FilePath] -> Document Element Element
goatsWebPage goats =
  Doc [] 
    [Elt "center" [] [E (Elt "h2" [] [S "Goats Я Awesome!"])],
     table -- we'll tile the goats 2 to a row
          (tileContent 2 (map (E . linkedImg) goats))]

{--
>>> goatsFiles goatsDir >>= putStrLn . pprint . rep . goatsWebPage 

generates your html
--}

tileContent :: Int -> [a] -> [[a]]
tileContent grp = -- um, list function?
   tile grp grp emptyDL

tile :: Int -> Int -> DList a -> [a] -> [[a]]
tile totes cnt accm [] = [dlToList accm]
tile totes cnt accm (h:t) =
   if cnt == 0 then dlToList accm:tile totes totes emptyDL t
   else                           tile totes (pred cnt) (accm <| h) t

-- creating a table seems a common enough thing in HTML, we'll move this
-- to Data.XHTML when we've got it working here.

table :: [[Content]] -> Element
table = Elt "table" [Attrib "border" "1"] . map (E . Elt "tr" [] . mapEachTR) 

mapEachTR :: [Content] -> [Content]
mapEachTR = map (E . Elt "td" [] . pure)

-- but first let's generate the entire URL from an asset:

assetURL :: FilePath -> String
assetURL asset = oembedService ++ query (Just JSON) (goatsDir ++ asset)

{--
>>> take 2 . map assetURL <$> goatsFiles goatsDir 
["http://127.0.0.1:8080/services/oembed/?format=json&url=http://127.0.0.1:8080/Y2017/M12/D13/goats/grace-n-goats.jpg",
 "http://127.0.0.1:8080/services/oembed/?format=json&url=http://127.0.0.1:8080/Y2017/M12/D13/goats/goat-small-town.jpg"]
--}

-- and also the image

linkedImg :: FilePath -> Element
linkedImg = linkURL <*> E . imageURL

imageURL :: FilePath -> Element
imageURL = flip (Elt "img") [] . pure . Attrib "src"
              . ((rootURI ++ goatsDir) ++)

-- and also the link to the oembed service that contains some content
-- (which may be the image)

linkURL :: FilePath -> Content -> Element
linkURL asset =
   Elt "a" [Attrib "href" (oembedService ++ query (Just JSON) asset)] . pure

{-- BONUS -----------------------------------------------------------------

Of course, who wants to look at raw oembed requests? You can hide those 
requests in the tiled images, right? Or something like that.

Get fancy with your output that creates your web page.

Now.

Create an application that, given a directory of goat-assets, outputs a
'User Friendly' goats web page.
--}

main' :: [String] -> IO ()
main' [dir] = goatsFiles dir >>= putStrLn . pprint . rep . goatsWebPage
main' _ = putStrLn (unlines ["", "oembedder <dir>", "",
   "\twhere dir is the directory of the images to oembed",""])

-- We'll look at creating the oembed response tomorrow
