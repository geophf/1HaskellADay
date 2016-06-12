module Data.Graphics.SVG where

-- a solution to the SVG-problem posted at http://lpaste.net/4776707050010836992

import Control.Logic.Frege (adjoin)  -- http://lpaste.net/111101
import Data.Graphics.BoundingBox     -- http://lpaste.net/2865245555871711232
import Data.XHTML                    -- http://lpaste.net/113385

svgDoctype :: String
svgDoctype = "<!DOCTYPE svg PUBLIC '-//W3C//DTD SVG 1.1//EN' " ++
  "'http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd'>"

-- Now it gets fun. We declare our svg-playing field, then we play in that
-- field with whatever shape we're creating

svgField :: Int -> Int -> [Element] -> Element
svgField width height =
   svgFromBB (makeBoundingBox (1, 10) (0,0)
             (adjoin ((100.0 *) . fromIntegral) (width, height)))

svgFromBB :: BoundingBox -> [Element] -> Element
svgFromBB bb =
   let bounds (Bounder (BBinternal (BB _ org end) _ _)) = (org, end)
       ((startx, starty), (width, height)) = bounds bb
       range = unwords (map (show . floor) [startx, starty, width, height]) in
   Elt "svg" [Attrib "viewbox" range,
              Attrib "xmlns" "http://www.w3.org/2000/svg",
              Attrib "version" "1.1"]
              . (E (Elt "desc" [] [S "Playin' in da SVG-hood!"]):)
              . map E

ell :: Int -> Int -> String -> [Attribute] -> Element
ell xAxis yAxis colour attrs =
   Elt "ellipse" 
       ([attr "rx" xAxis, attr "ry" yAxis, Attrib "fill" colour] ++ attrs) []

attr :: Show a => String -> a -> Attribute
attr nm = Attrib nm . show

attr_ :: (Show a, RealFrac a) => String -> a -> Attribute
attr_ nm = attr nm . floor

circ :: Int -> Int -> Int -> String -> [Attribute] -> Element
circ cx cy rad colour attrs =
   Elt "circle" 
       (attr "cx" cx:attr "cy" cy:attr "r" rad:Attrib "fill" colour:attrs)
       []

{-- Here's the example from w3:

<text x="250" y="150" 
        font-family="Verdana" font-size="55" fill="blue" >
    Hello, out there
  </text>

So you can add the font and color attributes if you'd like
--}

text, txt :: Point2D -> String -> [Attribute] -> Element
text (x,y) txt attrs =
   Elt "text" (attr "x" (floor x):attr "y" (floor y):attrs) [S txt]

font :: [Attribute]
font = [attr "font-size" 14, Attrib "font-family" "Verdana"]

txt (x,y) textme = text (x,y) textme . ((Attrib "fill" "black":font) ++)

line :: Int -> Int -> Int -> Int -> Int -> String -> Element
line x1 y1 x2 y2 sw colour =
   Elt "line" (Attrib "stroke" colour:map (uncurry attr)
       [("x1",x1),("y1",y1),("x2",x2),("y2",y2),("stroke-width",sw)]) []

rect :: Int -> Int -> Int -> Int -> String -> [Attribute] -> Element
rect orgx orgy width height colour attrs =
   Elt "rect" ((Attrib "fill" colour:
               map (uncurry attr) [("x", orgx), ("y", orgy), 
                     ("width", width), ("height", height)]) ++ attrs) []

-- now if you want to show the oval somewhere other than origin:

trans :: Int -> Int -> [Element] -> Element
trans x y = Elt "g" [Attrib "transform" ("translate(" ++ show x ++ 
                 (' ' : show y) ++ ")")] . map E

-- AND THEN show off your ellipse ... or ellipses! ... to the world!

ellipses :: FilePath -> IO ()
ellipses file =
   writeFile file $ unlines [ {-- pi, --} svgDoctype, 
         show $ svgField 20 10 -- in cm
                  [trans 150 150 [ell 100 100 "blue" []], -- a blue circle
                   trans 450 100 [ell  50 75 "none" [Attrib "stroke" "green",
                                                     attr "stroke-width" 22]],
                             -- a green thingie
                   trans 350 270 [ell 100  50 "red" []]]]  -- a red oval

-- *Data.SVG> ellipses "scircs.svg" ~> a file with shapes in it
