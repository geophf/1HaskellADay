{-# LANGUAGE OverloadedStrings #-}

module Y2017.M09.D05.Exercise where

import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server

{--
Yesterday, I said to write a function that retrieved the resources at an
URL (directory) ... of course, it's not that simple. Or it is, insofar as you
build a service that provides that information.

Today's Haskell problem is to do that.

Using a web-service framework [here we use Snap, but you may use a framework
you prefer], build a service that returns the directory contents at the
specified URL.

For today use the contents at the directories under Y2017/M09/D04/articles/

[n.b.: it's D04, or yesterday, where the articles are stored.]

The Snap tutorials are helpful in building a web service and querying 
directories on the server.
--}

start :: IO ()
start = quickHttpServe site

site :: Snap ()
site = undefined

-- hint: you can't just kick this app off with a ghci-invocation.
