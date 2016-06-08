module Data.XHTML where

-- http://lpaste.net/113385

import Control.Arrow

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

data Content = E Element | S String -- for now
   deriving (Eq, Ord)

instance Show Content where
   show (E elt) = show elt
   show (S str) = str

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
printXML xml = (\elt -> printElementWithOffset elt 0 >> return elt) (rep xml)

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

data Document heads bodyElts = Doc [heads] [bodyElts]

instance (XML headElts, XML bodyElts) => XML (Document headElts bodyElts) where
   rep (Doc heads bods) =
      Elt "html" [] [E (Elt "head" [] (map (E . rep) heads)),
                     E (Elt "body" [] (map (E . rep) bods))]
   kind doc = "xmldocument"
