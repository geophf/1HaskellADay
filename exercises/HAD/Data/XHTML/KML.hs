module Data.XHTML.KML where

-- KML/Keyhole Markup Language: Representing geodesic mappings as XML

{--
We want to model, or to output, the follow kinds of documents:

<?xml version="1.0" encoding="UTF-8"?>
<kml xmlns="http://earth.google.com/kml/2.0"> 
<Document>
<open>1</open>
<Folder>
 <open>1</open>
  <name>New Folder</name>
 <description>
    This is a new folder. No, really. I'm not kidding. Oh, and go to 
    the <a href="http://logicaltypes.blogspot.com/">Haskell Problems page</a>.
 </description>
 <Placemark>  <visibility>0</visibility>
   <name> Near Oecho Nika.</name>
   <description>The southern fields of a farm</description>
 <Point>
   <coordinates>135.2, 35.4, 0. </coordinates>
 </Point></Placemark>
 <Placemark>
   <name>Nihonheso Park</name>
   <description>Strolls and play in a popular family spot</description>
 <Point>
   <coordinates>135.0, 35.0, 0. </coordinates>
 </Point></Placemark>
 <Folder>
  <name>A Folder in a Folder</name>
  <description>
    This is a folder, within a folder. Like a donut hole, in a donut's hole.
  </description>
 </Folder>
</Folder>
</Document> 
</kml>
--}

import Data.Maybe (maybeToList)

import Data.XHTML hiding (P)

data KML = KML [Key]
   deriving Show

data Key = F Folder | P Placemark
   deriving (Eq, Show)

type Description = String

data Folder = Folder Name (Maybe Description) [Key]
   deriving (Eq, Show)

data Placemark = Placemark Name (Maybe Description) [PointOrLine]
   deriving (Eq, Show)

data PointOrLine = Pt Point | Ln Line
   deriving (Eq, Show)

data Line = Line [Point]
   deriving (Eq, Show)

data Point = Coord Latitude Longitude Height
   deriving (Eq, Show)

type Latitude = Float
type Longitude = Float
type Height = Float

-- so that means we need an XML-representation of the above types

instance XML KML where
   rep (KML content) =
      Elt "kml" [Attrib "xmlns" "http://earth.google.com/kml/2.0"]
          [E $ Elt "Document" [] (map (E . rep) content)]
   kind = const "Keyhole Markup Language root"

instance XML Key where 
   rep (F folder) = rep folder
   rep (P mark) = rep mark
   kind (F f) = kind f
   kind (P mark) = kind mark 

instance XML Folder where
   rep = enXMLification . F
   kind = const "Folder"

instance XML Placemark where
   rep = undefined
   kind = undefined

instance XML PointOrLine where
   rep = undefined
   kind = undefined

instance XML Point where
   rep = undefined
   kind = undefined

instance XML Line where
   rep = undefined
   kind = undefined

-- internal function to convert KML values to XML

type Tag = String
data Thunk = Thunk Tag Name (Maybe Description) [PointOrKey]
data PointOrKey = PoL PointOrLine | K Key
   
instance XML PointOrKey where
   rep (PoL p) = rep p
   rep (K k)  = rep k
   kind = const "Thingie"

enThunkify :: Key -> Thunk
enThunkify (F (Folder n md ks)) = Thunk "Folder" n md (map K ks)
enThunkify (P (Placemark n md pts)) = Thunk "Placemark" n md (map PoL pts)
   
enXMLification :: Key -> Element
enXMLification = e' . enThunkify 
   
e' :: Thunk -> Element
e' (Thunk tag n md as) =
   Elt tag [] (map E ((mkElt "name" n):md1 ++ map rep as))
      where md1 = maybeToList (Elt "Description" [] . return . S <$> md)
   
mkElt :: String -> String -> Element
mkElt tag content = Elt tag [] [S content]

skeletonKML :: FilePath -> KML -> IO ()
skeletonKML outfile = printXMLDoc . XDoc (PI "1.0")

{--
... and, in case anyone was wondering, the KML reference card is here:

https://developers.google.com/kml/documentation/kmlreference
--}
