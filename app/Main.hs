module Main where

import qualified System.Random.MT as MT
import qualified System.Random    as R
import           System.Environment (getArgs)
import           System.Entropy
import           Data.ByteString

main    :: IO ()
main =  do args <- getArgs
           e <- getEntropy 8
           print $ unpack e
           h <- getHardwareEntropy 8
           print $ h
          {- if (head args == "mt")
                                  then (print $ (take 100000 (MT.randoms (MT.mkStdGenMT 1)) :: [Word]))
                                  else (print $ (take 100000 $ R.randoms  (R.mkStdGen 1)    :: [Int]))-}
