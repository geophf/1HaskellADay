{-# LANGUAGE OverloadedStrings #-}

module Y2020.M11.D13.Solution where

{--
HI! So, we got it working! Yay! And we have the 'daters' in the 'datersbase.'

YAY!

So, now, let's represent these 'daters' visually.

Hello, Google Earth.

Google Earth is, like, the Earth, man.

(wow, that was amazing, that introduction to Google Earth, it was!)

Now, in Google Earth, then all you to share your stories, by zooming in to
particular points on the Earth, but you can also provide annotations, screen-
shots, and group these things into folders.

Perfect.

KML is an XML ... language? (extensible markup language-language? YES!) and
the documentation ... is there! And you can go to Google Earth and follow
the tutorials to create your own projects.

For us to create XML, we can use any XML library we so desire. I'm going 
with my own-rolled one.
--}

import Data.Maybe (maybeToList)

import Data.XHTML hiding (P)

-- What XML are we creating? One such sample project I created by hand is HERE:

kmlDir :: FilePath
kmlDir = "Y2020/M11/D13/"

kmlSampleProject :: FilePath
kmlSampleProject = "airpowers-and-alliances.kml"

{--
But this has all the additional annotations that Google puts in there that
are very ... 'meaningful' (?) to ... somebody (?) but we don't have to 
imitate, necessarily, ourselves.

So, what's the minimum we need to do to creat our own KML-file of airbases
and alliances?

Good question!

Today's #haskell problem.

Fortunately, the Internet exists (because I invented the thing), and has
some kind soul providing some bare-minimum examples of KML files:

http://dagik.org/kml_intro/E/folder.html

Let's take this story, implementing airbases and alliances, step-by-step
in a piecemeal approach to our finished result, eh?

step uno: Create a KML document that has two folders:

1. Countries by Alliance
2. Airbases by Country

write out this KML-document to a file. What does that document look like?

It looks something like this strawman:

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

This file is archived at this directory as kml-playground.kml.

... but let's not do anything with Placemark at this juncture
--}

kmlPlayground :: FilePath
kmlPlayground = "kml-playground.kml"

type Description = String

data KML = KML [Key]
   deriving Show

data Key = F Folder | P Placemark
   deriving (Eq, Show)

data Folder = Folder Name (Maybe Description) [Key]
   deriving (Eq, Show)

data Placemark = Placemark Name (Maybe Description) Point
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
   kind = undefined

-- for Folder/Placemark enXMLification:

type Tag = String
data Thunk = Thunk Tag Name (Maybe Description) [PointOrKey]
data PointOrKey = Pt Point | K Key

instance XML PointOrKey where
   rep (Pt p) = rep p
   rep (K k)  = rep k
   kind = const "Thingie"

enThunkify :: Key -> Thunk
enThunkify (F (Folder n md ks)) = Thunk "Folder" n md (map K ks)
enThunkify (P (Placemark n md pt)) = Thunk "Placemark" n md ([Pt pt])

enXMLification :: Key -> Element
enXMLification = e' . enThunkify

e' :: Thunk -> Element
e' (Thunk tag n md as) =
   Elt tag [] (map E ((mkElt "name" n):md1 ++ map rep as))
      where md1 = maybeToList (Elt "Description" [] . return . S <$> md)

mkElt :: String -> String -> Element
mkElt tag content = Elt tag [] [S content]

-- You don't have to implement the below instance declarations:

instance XML Placemark where
   rep = undefined
   kind = undefined

instance XML Point where
   rep = undefined
   kind = undefined

-- but do implement the below:

skeletonKML :: FilePath -> KML -> IO ()
skeletonKML outfile = printXMLDoc . XDoc (PI "1.0") 

-- what does this KML document look like as an XML file?

kmlSample :: KML
kmlSample = KML [F (Folder "Countries by Alliance" Nothing []),
                 F (Folder "Airbases by Country" Nothing [])]

{--
... and, in case anyone was wondering, the KML reference card is here:

https://developers.google.com/kml/documentation/kmlreference

We're going to add lines connecting same-country airbases, but this is a start.
--}
