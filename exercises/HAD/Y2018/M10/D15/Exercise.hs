module Y2018.M10.D15.Exercise where

{--
You have a set of assets on your iPod, a piece of hardware older than your
daughters, let's say, hypothetically, and you have a sets of assets on your
thumbdrives. You want to move the assets off your iPod before the needle wrecks
your hard drive. I'm not kidding.

With the assets defined on your iPod and then the assets defined on each of
your thumbdrive, what assests do you need to move off your iPod without
moving assets that you already have on your thumbdrives?
--}

type Asset = String

iPod, thumb1, thumb2, exDir :: FilePath
exDir = "Y2018/M10/D15/"
iPod = "iPod.sha"
thumb1 = "thumb1.sha"
thumb2 = "thumb2.sha"

-- Given the files with the assets, what assets need to be moved off the iPod?

assetsToMove :: FilePath -> FilePath -> FilePath -> IO [Asset]
assetsToMove ipod th1 th2 = undefined
