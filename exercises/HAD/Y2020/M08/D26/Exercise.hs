module Y2020.M08.D26.Exercise where

import Y2020.M08.D25.Exercise

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

import Data.Map (Map)

type Title = String
type URL = FilePath
type BookInfo = (Title, URL)
type Text = String

type Library = Map BookInfo Text

importBook :: BookInfo -> IO Text
importBook bookInfo = undefined

importLibrary :: BookIndex -> IO Library
importLibrary bookinfos = undefined
