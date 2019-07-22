module System.Random.MT ( randoms
                        , random
                        , randomRs
                        , randomR
                        , randomIOs
                        , randomIO
                        , mkStdGenMT
                        , getSeed
                        , getNum
                        ) where

import qualified Data.Array.IArray as A
import qualified Data.Array.MArray as M
import qualified Data.Array.Repa   as R
import           Data.Bits                        ( (.&.)
                                                  , (.|.)
                                                  , xor
                                                  , shiftR
                                                  , shiftL)
import           Data.Word                        ( Word32
                                                  , Word64 )
import           Data.Int                         ( Int32 )
import           Control.Monad.ST                 ( runST)
import           Data.STRef                       ( newSTRef
                                                  , modifySTRef
                                                  , readSTRef
                                                  , writeSTRef )

import           Data.Array.ST                    ( newArray
                                                  , readArray
                                                  , writeArray, runSTArray)

import           System.CPUTime                   ( getCPUTime )
import           Codec.CBOR.Magic                 ( word32ToWord )
import           Data.Ratio                       ( (%) )
import           Data.BitStream.ContinuousMapping ( wordToInt )


class RandomMT a where
  randoms   :: StdGenMT -> [a]

  random     :: StdGenMT -> (a, StdGenMT)
  random gen =  (randoms gen !! getNum gen, nextGen gen)

  randomIOs  :: IO [a]
  randomIOs  =  (randoms . mkStdGenMT) <$> getCPUTime

  randomIO :: IO a
  randomIO =  head <$> randomIOs

instance RandomMT Word   where
  randoms = genrandInt . getSeed
instance RandomMT Int    where
  randoms gen = map wordToInt (randoms gen :: [Word])
instance RandomMT Double where
  randoms = genrandReal132
instance RandomMT Float  where
  randoms = genrandReal132
instance RandomMT Word32 where
  randoms gen = map tempering $ g 0 $ initGenrandArray32 $ getSeed gen

class (Num a, Ord a, Integral a, RandomMT a) => RandomMTR a where
  randomRs              :: (a, a) -> StdGenMT -> [a]
  randomRs (lo, hi) gen |  lo > hi   = randomRs (hi, lo) gen
                        |  otherwise = map ((+ lo) . (`mod` range)) (randoms gen)
                        where range = hi - lo + 1

  randomR       :: (a, a) -> StdGenMT -> (a, StdGenMT)
  randomR r gen =  (randomRs r gen !! getNum gen, nextGen gen)

instance RandomMTR Int  where
instance RandomMTR Word where

data StdGenMT = StdGenMT Int Seed

nextGen                :: StdGenMT -> StdGenMT
nextGen (StdGenMT i s) =  StdGenMT (i + 1) s

type Seed = Word32

class RandomGenMT g where
  mkStdGenMT :: g -> StdGenMT

instance RandomGenMT Word32  where
  mkStdGenMT = StdGenMT 0
instance RandomGenMT Integer where
  mkStdGenMT i = StdGenMT 0 (fromInteger i)
instance RandomGenMT Int     where
  mkStdGenMT i = StdGenMT 0 (fromIntegral i)


getSeed                :: StdGenMT -> Seed
getSeed (StdGenMT _ s) =  s

getNum                :: StdGenMT -> Int
getNum (StdGenMT i _) =  i

genrandReal132     :: Fractional b => System.Random.MT.StdGenMT -> [b]
genrandReal132 gen =  map (fromRational
                          . (% (if is64bit
                                   then 18446744073709551615
                                   else 4294967295))
                          . toInteger) (randoms gen :: [Word])
{-
(maxBound :: Word32) == 4294967295
(maxBound :: Word64) == 18446744073709551615
-}

is64bit :: Bool
is64bit =  word32ToWord (maxBound :: Word32) /= (maxBound :: Word)

genrandInt :: Seed -> [Word]
genrandInt | is64bit   = genrandInt32
           | otherwise = genrandInt32 -- TODO replace to 64

{- Period parameters -}
n         = 624
m         = 397
matrixA   = 0x9908b0df :: Word32 -- constant vector a
upperMask = 0x80000000 :: Word32 -- most significant w-r bits
lowerMask = 0x7fffffff :: Word32 -- least significant r bits

initGenrand32   :: Seed -> [Word32]
initGenrand32 s =  first : f first [1 .. (n - 1)]
                   where f                    :: Word32 -> [Word32] -> [Word32]
                         f _    []            =  []
                         f prev (curr : next) =  new : f new next
                                                 where new =  ((1812433253 :: Word32) * (prev `xor` (prev `shiftR` 30)) + curr) .&. (0xffffffff :: Word32)
                         first :: Word32
                         first =  s .&. (0xffffffff :: Word32)

initGenrandRepa32   :: Seed -> R.Array R.U R.DIM1 Word32
initGenrandRepa32 s =  R.fromListUnboxed (R.Z R.:. fromIntegral n) $ initGenrand32 s

initGenrandArray32   :: Seed -> A.Array Word32 Word32
initGenrandArray32 s =  A.listArray (0, n - 1) (initGenrand32 s)

genrandInt32      :: Seed -> [Word]
genrandInt32 seed =  map (word32ToWord . tempering) $ g 0 $ initGenrandArray32 seed

{- default seed is 5489-}

mag01 :: [Word32]
mag01 =  [0x0, matrixA]

g       :: Word32 -> A.Array Word32 Word32 -> [Word32]
g i arr
  | i < (n - m) = g (i + 1) $ runSTArray $ do arr' <- M.thaw arr
                                              writeArray arr' i $ (arr A.! (i + m)) `xor`
                                                                        (y `shiftR` 1) `xor`
                                                                        (mag01 !! fromIntegral (y .&. (0x1 :: Word32)))
                                              return arr'
  | (n - m) <= i &&
    (i < n - 1) = g (i + 1) $ runSTArray $ do arr' <- M.thaw arr
                                              writeArray arr' i $ (arr A.! (i + (m - n))) `xor`
                                                                  (y `shiftR` 1) `xor`
                                                                  (mag01 !! fromIntegral (y .&. (0x1 :: Word32)))
                                              return arr'

  | otherwise        = let narr = runSTArray $ do arr' <- M.thaw arr
                                                  writeArray arr' i $ (arr A.! (m - 1)) `xor`
                                                                      (y `shiftR` 1) `xor`
                                                                      (mag01 !! fromIntegral (y .&. (0x1 :: Word32)))
                                                  return arr'
                       in A.elems narr ++ g 0 narr
  where y :: Word32
        y =  ((arr A.! i) .&. upperMask) .|.
             ((arr A.! (if i < (n - 1) then i + 1 else 0))       .&. lowerMask)

tempering   :: Word32 -> Word32
tempering x =  runST $ do res  <- newSTRef (x :: Word32)
                          res' <- readSTRef res
                          modifySTRef res (xor  (res' `shiftR` 11))
                          res' <- readSTRef res
                          modifySTRef res (xor ((res' `shiftL`  7) .&. (0x9d2c5680 :: Word32)))
                          res' <- readSTRef res
                          modifySTRef res (xor ((res' `shiftL` 15) .&. (0xefc60000 :: Word32)))
                          res' <- readSTRef res
                          modifySTRef res (xor  (res' `shiftR` 18))
                          readSTRef res
