module Y2020.M07.D23.Exercise where

{--
How can there be too many flowers?

This problem looks at the problem of lots of flowers... how many? We're not
going to answer that question today, but we are going to load an image file
of flowers. That's a good starter.
--}

import Codec.Picture (readImage)
import Codec.Picture.Jpg

flahz :: FilePath
flahz = "Y2020/M07/D23/echinacea.jpeg"

-- so, some image processors only work with TIFFs. Convert the above image
-- (the JPEG file) and save it as a TIFF.

convertToTiff :: FilePath -> FilePath -> IO ()
convertToTiff jpgFileIn tiffFileOut = undefined

-- reminder, you have to read in the JPG file, first, then write out the TIFF
-- so there's a couple of things going on in this function.
