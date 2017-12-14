{-# LANGUAGE OverloadedStrings #-}

module Y2017.M12.D15.Exercise where

import Data.Text
import System.Directory

{--
New topic!

Today we're going to create an oembed service where we serve some simple
resources, like, goats, ... because I like kids; they are SO CUTE!

So, there are three things that make up a kids oembed service:

1. the resources themselves. They are here at this directory
--}

-- below import available via 1HaskellADay git repository

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
goatsFiles dir = undefined

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

oembedService :: FilePath
oembedService = "http://127.0.0.1:8080/services/oembed/"

-- 2. the query string which includes the url and which (may or may not)
--    include the format

data Format = JSON
   deriving Eq

instance Show Format where
   show = const "json"

query :: Maybe Format -> FilePath -> String
query format relativeURI = undefined

-- and with that, you can create your webpage with your goatsFiles links:

goatsWebPage :: [FilePath] -> Text
goatsWebPage goats = undefined

{-- BONUS -----------------------------------------------------------------

Of course, who wants to look at raw oembed requests? You can hide those 
requests in the tiled images, right? Or something like that.

Get fancy with your output that creates your web page.

Now.

Create an application that, given a directory of goat-assets, outputs a
'User Friendly' goats web page.
--}

main' :: [String] -> IO ()
main' dir = undefined

-- We'll look at creating the oembed response tomorrow
