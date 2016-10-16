module Data.BlockChain.Block.Web where

-- gives web-view to block structures

-- below imports available from 1HaskellADay git repository

import Data.BlockChain.Block.BlockInfo
import Data.BlockChain.Block.Utils (est2time)
import Data.XHTML

instance Rasa BlockInfo where
   printRow (BlockInfo _ h t) =
      Elt "tr" [] (map (E . Elt "td" [] . pure)
                       [E (Elt "a" [Attrib "href" 
                                           ("http://127.0.0.1:8080/block/" ++ h)] [S h]),
                        S . show $ est2time t])

