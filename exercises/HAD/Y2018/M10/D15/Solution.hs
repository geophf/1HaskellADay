module Y2018.M10.D15.Solution where

{--
You have a set of assets on your iPod, a piece of hardware older than your
daughters, let's say, hypothetically, and you have a sets of assets on your
thumbdrives. You want to move the assets off your iPod before the needle wrecks
your hard drive. I'm not kidding.

With the assets defined on your iPod and then the assets defined on each of
your thumbdrive, what assests do you need to move off your iPod without
moving assets that you already have on your thumbdrives?
--}

import Data.Set (Set)
import qualified Data.Set as Set

type Asset = String

iPod, thumb1, thumb2, exDir :: FilePath
exDir = "Y2018/M10/D15/"
iPod = "iPod.sha"
thumb1 = "thumb1.sha"
thumb2 = "thumb2.sha"

-- Given the files with the assets, what assets need to be moved off the iPod?

assetsToMove :: FilePath -> FilePath -> FilePath -> IO [Asset]
assetsToMove ipod th1 th2 = do
   si <- setRead ipod
   t1 <- setRead th1
   t2 <- setRead th2
   return (Set.toList (Set.difference si (Set.union t1 t2)))

setRead :: FilePath -> IO (Set Asset)
setRead = fmap (Set.fromList . lines) . readFile

{--
>>> assetsToMove (exDir ++ iPod) (exDir ++ thumb1) (exDir ++ thumb2) >>= mapM_ putStrLn
02bd54a68ff9fc905c1cc674ac47a50e838cf92386049dbb83011026afc6908d
1bd22c59e35ee851b125d72a1ff0ef0167a79d997c7667fcbc742f7647eaf3d7
6a441cc045c0930476fd62c823e4f4c53d7ecc66a75f4d9fab3e431e7597a8f8
80a0ac90f639f1476b6422b6cc32856509433adc2a3652cbc0af5bc6f97c375d
865b40a4330e12daa87b7f550f00ac9d602511ce3b428bb4c9432b48b7e236e8
--}
