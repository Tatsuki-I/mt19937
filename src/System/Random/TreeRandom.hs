module System.Random.TreeRandom where

import Data.Word (Word8)
import Data.Bits
import Control.Parallel.Strategies

data BTree a
    = Leaf
    | Node a (BTree a) (BTree a)
    deriving (Show, Eq)

a :: Word8
a =  1

b :: Word8
b =  1

tr   :: Int      -- ^ Length
     -> Word8   -- ^ Seed
     -> BTree Word8 -- ^ Random numbers
tr l =  tr' (((floor . logBase 2 . fromIntegral) l) + 1)
        where tr'     :: Int
                      -> Word8
                      -> BTree Word8
              tr' 0 _ =  Leaf
              tr' l s =  Node r (tr' (l - 1)  r)
                                (tr' (l - 1) rr)
                         where r  = f s
                               rr = maxBound - r
ptr   :: Int
      -> Word8
      -> BTree Word8
ptr l =  ptr' (((floor . logBase 2 . fromIntegral) l) + 1)
         where ptr'   :: Int
                      -> Word8
                      -> BTree Word8
               ptr' 0 _ =  Leaf
               ptr' l s =  runEval $ do n1 <- (rpar . ptr' (l - 1)) r
                                        n2 <- (rpar . ptr' (l - 1)) rr
                                        return $ Node r n1 n2
                           where r  = f s
                                 rr = maxBound -r

f   :: Word8 -> Word8
f s =  a * s + b

toList   :: BTree a
         -> [a]
toList t =  toList' [t] []
            where toList'      :: [BTree a]
                               -> [a]
                               -> [a]
                  toList' Leaf _ =  []
                  toList' n es   =  toList' () [es ++ e]
                                    where e = map (fst . toList) es

getElem              :: BTree a
                     -> (a, (BTree a, BTree a))
getElem Node e t1 t2 =  (e, (t1, t2))

xorshift :: Word8 -> Word8
xorshift = undefined
