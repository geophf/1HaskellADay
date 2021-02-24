module Data.XHTML where

import Control.Monad (void)

{--

Provides the protocol for XML representation, and then provides an XHTML-
centric presentation of instances of that protocol. VERY much an unofficial
hack. That doesn't mean that it's not useful. It just means that if you're
looking for a spec-referenced implementation, ... look elsewhere.

--}

class XML a where
   rep :: a -> Element
   kind :: a -> String

nb :: Content  -- because we use this so much. I suppose there should be
               -- a standard way of representing XML entities.
nb = S "&nbsp;" 

type Name = String
type Value = String

data Attribute = Attrib Name Value
   deriving (Eq, Ord)

td :: Value -> Content -> Element
td val cont = Elt "td" [Attrib "class" val] [cont]

instance Show Attribute where
   show (Attrib name value) = name ++ "=\"" ++ value ++ "\""

type VersionNumber = String

data ProcessingInstruction = PI VersionNumber
   deriving (Eq, Ord)

instance Show ProcessingInstruction where
   show (PI ver) =  concat ["<?xml version=\"", ver, "\" encoding=\"UTF-8\"?>"]

data Content = E Element | S String | P ProcessingInstruction
   deriving (Eq, Ord)

instance Show Content where
   show (E elt) = show elt
   show (S str) = str
   show (P p) = show p

data Element = Elt Name [Attribute] [Content]
   deriving (Eq, Ord)

instance XML Element where
   rep = id
   kind elt = "elt"

instance Show Element where
   show elt@(Elt _ _ []) = emptyElement elt
   show elt@(Elt _ _ cont) = 
     openTag elt ++ concatMap show cont ++ closeTag elt

pprint :: Element -> String
pprint elt@(Elt _ _ []) = show elt
pprint elt@(Elt _ _ cont) = openTag elt ++ ('\n':unlines (map show cont))
   ++ closeTag elt

openTag, closeTag, emptyElement :: Element -> String
emptyElement (Elt name attribs _) = startTag name attribs ++ "/>"
openTag (Elt name attribs content) = tag name attribs

closeTag (Elt name _ _ ) = tag ('/':name) []

tag, startTag :: Name -> [Attribute] -> String
startTag name attribs = '<' : name ++ showAttribs attribs 
tag name attribs = startTag name attribs ++ ">"

showAttribs :: [Attribute] -> String
showAttribs = concatMap ((:) ' ' . show)

printXML :: XML xml => xml -> IO Element
printXML = (\elt -> printElementWithOffset elt 0 >> return elt) . rep

printElementWithOffset :: Element -> Int -> IO ()
printElementWithOffset elt@(Elt _ _ content) indent =
   putStrLn (spaces indent ++ openTag elt) >>
     mapM_ (flip printContent (succ indent)) content >>
   putStrLn (spaces indent ++ closeTag elt)

spaces :: Int -> String
spaces = flip replicate ' '

printContent :: Content -> Int -> IO ()
printContent (E elt) n = printElementWithOffset elt n
printContent (S str) n = putStrLn (spaces n ++ str)
printContent (P p) _ = putStrLn (show p)

data XMLDoc elt = XDoc ProcessingInstruction elt

printXMLDoc :: XML elt => XMLDoc elt -> IO ()
printXMLDoc (XDoc pi elt) = printContent (P pi) 0 >> void (printXML elt)

data HTML heads bodyElts = Doc [heads] [bodyElts]

instance (XML headElts, XML bodyElts) => XML (HTML headElts bodyElts) where
   rep (Doc heads bods) =
      Elt "html" [] [E (Elt "head" [] (map (E . rep) heads)),
                     E (Elt "body" [] (map (E . rep) bods))]
   kind doc = "xhtmldocument"

-- Now we need a way to represent rows of some data type as an HTML table:

class Rasa a where
   printRow :: a -> Element

{--
Great. Now that you have that, define a function tabluate that takes a list of
Attributes, a list of Elements that preceed the table body, and a list of Rasa
values and returns an HTML TABLE element that packages them all together.

So, if BlockInfo were a Rasa instance, e.g.:

instance Rasa BlockInfo where
   printRow (BlockInfo ht hash time) = 
      Elt "tr" [] (map (Elt "td" [] . pure)
                       [E (Elt "a" [Attrib "href" ("http://127.0.0.1/block/" ++ hash)] [S hash]),
                        S . show $ est2time t])

tabulate [Attrib "border" "1"] [table header for columns Hash and Time]
         [BlockInfo 13 "adkfjkjhfa" 1399482, BlockInfo 227 "adfjkhdf" 1400122]

would give an HTML table that would output the BlockInfo values in rows with
their data in the appropriately labeled columns.
--}

tabulate :: Rasa a => [Attribute] -> [Element] -> [a] -> Element
tabulate attribs headers rows =
   Elt "table" attribs (map E (headers ++ map printRow rows))

-- this is a useful function to give a table a set of column headers:

thdrs :: [String] -> Element
thdrs = Elt "tr" [] . map (E . Elt "th" [Attrib "align" "left"] . pure . S)

-- and for making table rows...

tr :: [Content] -> Element
tr = Elt "tr" [] . map (E . Elt "td" [] . pure)
