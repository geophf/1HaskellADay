{-# LANGUAGE OverloadedStrings #-}

module Y2020.M11.D13.Exercise where

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

import Data.Text (Text)

import Data.XHTML

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

... but let's not do anything with Placemark at this juncture
--}

type Description = Text

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
   rep = undefined
   kind = undefined

instance XML Key where
   rep = undefined
   kind = undefined

instance XML Folder where
   rep = undefined
   kind = undefined

instance XML Placemark where
   rep = undefined
   kind = undefined

instance XML Point where
   rep = undefined
   kind = undefined

skeletonKML :: FilePath -> KML -> IO ()
skeletonKML outfile = undefined

-- what does this KML document look like as an XML file?

kmlSample :: KML
kmlSample = KML [F (Folder "Countries by Alliance" Nothing []),
                 F (Folder "Airbases by Country" Nothing [])]

{--
... and, in case anyone was wondering, the KML reference card is here:

https://developers.google.com/kml/documentation/kmlreference

We're going to add lines connecting same-country airbases, but this is a start.
--}
