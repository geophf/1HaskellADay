{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Y2020.M08.D26.Solution where

import Y2020.M08.D25.Solution

{--
Yesterday, we read in the top-downloaded project gutenberg books with their
associated URLs to the book-pages, but, if you followed any of those links
you see those links aren't the books, themselves, but are the anchor-pages
to links to different readable kinds of those books.

Huh.

Today, we're going to take those links and download the plain-text versions
of those books. Why the plain-text versions? Because eh, that's why.

(Also, I like the plain text versions for vectorizing, but that's just 
moiself (that is French) (no, it's not)).

Okay, so where is the plain text version of the book?

We have:

"Pride and Prejudice by Jane Austen" at 

url: http://www.gutenberg.org/ebooks/1342

The plain-text version is at: http://www.gutenberg.org/files/1342/1342-0.txt

And ... probably (?) ... the other books follow suit? unless they're 
multivolume 'How Rome was Built in One Day (psych!)' and who reads that?

(I've just insulted all the history-buffs in the world, but oh, well.)

SO!

With that tidbit, today's #haskell exercise is to download the top-100
gutenberg books into your very own Library.
--}

import Control.Arrow ((&&&))
import Control.Monad (join)

import qualified Data.Text as T

import Data.List (stripPrefix)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

import Network.HTTP (simpleHTTP, getResponseBody, getRequest)

type Title = String
type URL = FilePath
type BookInfo = (Title, URL)
type Text = String

type Library = Map BookInfo Text

importBook :: BookInfo -> IO Text
importBook (_title, url) =
   let reurl = uncurry reurlify (extractBookInfo url)
   in simpleHTTP (getRequest reurl) >>= getResponseBody

{--
>>> take 65 <$> importBook ("Pride and Prejudice",
                            "http://www.gutenberg.org/ebooks/1342")
"\239\187\191\r\nThe Project Gutenberg EBook of Pride and Prejudice, by Jane "
--}

importLibrary :: BookIndex -> IO Library
importLibrary idx =
   Map.fromList 
   <$> sequence (map (traverse importBook . join (,)) (Map.toList idx))

{--
The expression:

traverse importBook . join (,)

is a one-liner solution to simplifying:

\info -> importBook info >>= return . (info,)

via @noaheasterly
--}

{--
>>> let idx = gutenbergIndex (workingDir ++ gutenbergTop100Index)
>>> length <$> idx
100

>>> let lib = idx >>= importLibrary
>>> let truncatePrint = putStrLn . take 60
>>> let listofy ((a,b),c) = [a,b,c]
>>> take 3 . Map.toList <$> lib
> take 3 . Map.toList <$> lib
[(("A Christmas Carol in Prose; Being a Ghost Story of Christmas by "...,
   "http://www.gutenberg.org//ebooks/46"),
  "\239\187\191The Project Gutenberg EBook of A Christmas Carol, by" ...),...]
--}

-- so, we need the book id and the root url

extractBookInfo :: FilePath -> (FilePath, String)
extractBookInfo = (head &&& last)
                  . map T.unpack
                  . T.splitOn "/"
                  . T.pack
                  . (fromMaybe <*> stripPrefix "http://")

{--
>>> extractBookInfo "http://www.gutenberg.org/ebooks/1342"
("www.gutenberg.org","1342")
--}

-- now we need to reurlify the url. Yeah. I said that. Fite meh.

reurlify :: FilePath -> String -> FilePath
reurlify base bookid = 
   concat ["http://", base, "/files/", bookid, '/':bookid, "-0.txt"]

{--
>>> uncurry reurlify $ extractBookInfo "http://www.gutenberg.org/ebooks/1342"
"www.gutenberg.org/files/1342/1342-0.txt"

Si buano.
--}

