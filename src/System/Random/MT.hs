{-# Language FlexibleInstances #-}
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
import           Data.Ratio                       ( Ratio
                                                  , (%) )
import           Data.BitStream.ContinuousMapping ( wordToInt )


class RandomMT a where
  randoms   :: StdGenMT -> [a]

  random     :: StdGenMT -> (a, StdGenMT)
  random gen =  (randoms gen !! getNum gen, nextGen gen)

  randomIOs  :: IO [a]
  randomIOs  =  (randoms . mkStdGenMT) <$> getCPUTime

  randomIO :: IO a
  randomIO =  head <$> randomIOs

instance RandomMT Word            where
  randoms = genrandInt . getSeed
instance RandomMT Int             where
  randoms gen = map wordToInt (randoms gen :: [Word])
instance RandomMT (Ratio Integer) where
  randoms = genrandRational132
instance RandomMT (Ratio Int32)   where
  randoms = genrandRational132
instance RandomMT Double          where
  randoms = genrandReal132
instance RandomMT Float           where
  randoms = genrandReal132
instance RandomMT Word32          where
  randoms gen = map tempering $ mt32 0 $ initGenrandArray32 $ getSeed gen

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

--genrandRational132     :: System.Random.MT.StdGenMT -> [Ratio Integer]
genrandRational132 gen =  map ((% 4294967295)
                              . fromIntegral) (randoms gen :: [Word32])

{- [0, 1] -}
genrandReal132     :: Fractional a => System.Random.MT.StdGenMT -> [a]
genrandReal132 gen =  map (fromRational
                          . (% 4294967295)
                          . toInteger) (randoms gen :: [Word32])

{- [0, 1) -}
genrandReal232     :: Fractional a => System.Random.MT.StdGenMT -> [a]
genrandReal232 gen =  map (fromRational
                          . (% 4294967296)
                          . toInteger) (randoms gen :: [Word32])

{- (0, 1) -}
genrandReal332     :: Fractional a => System.Random.MT.StdGenMT -> [a]
genrandReal332 gen =  undefined
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
n32         = 624
m32         = 397
matrixA32   = 0x9908b0df :: Word32 -- constant vector a
upperMask32 = 0x80000000 :: Word32 -- most significant w-r bits
lowerMask32 = 0x7fffffff :: Word32 -- least significant r bits

initGenrand32   :: Seed -> [Word32]
initGenrand32 s =  first : f first [1 .. (n32 - 1)]
                   where f                    :: Word32 -> [Word32] -> [Word32]
                         f _    []            =  []
                         f prev (curr : next) =  new : f new next
                                                 where new =  ((1812433253 :: Word32) * (prev `xor` (prev `shiftR` 30)) + curr) .&. (0xffffffff :: Word32)
                         first :: Word32
                         first =  s .&. (0xffffffff :: Word32)

initGenrandRepa32   :: Seed -> R.Array R.U R.DIM1 Word32
initGenrandRepa32 s =  R.fromListUnboxed (R.Z R.:. fromIntegral n32) $ initGenrand32 s

initGenrandArray32   :: Seed -> A.Array Word32 Word32
initGenrandArray32 s =  A.listArray (0, n32 - 1) (initGenrand32 s)

genrandInt32      :: Seed -> [Word]
genrandInt32 seed =  map (word32ToWord . tempering) $ mt32 0 $ initGenrandArray32 seed

{- default seed is 5489-}

mag0132 :: [Word32]
mag0132 =  [0x0, matrixA32]

mt32       :: Word32 -> A.Array Word32 Word32 -> [Word32]
mt32 i arr
     | i < (n32 - m32) = mt32 (i + 1) $ runSTArray $ do arr' <- M.thaw arr
                                                        writeArray arr' i $ (arr A.! (i + m32)) `xor`
                                                                            (y `shiftR` 1) `xor`
                                                                            (mag0132 !! fromIntegral (y .&. (0x1 :: Word32)))
                                                        return arr'
     | (n32 - m32) <= i &&
       (i < n32 - 1) = mt32 (i + 1) $ runSTArray $ do arr' <- M.thaw arr
                                                      writeArray arr' i $ (arr A.! (i + (m32 - n32))) `xor`
                                                                          (y `shiftR` 1) `xor`
                                                                          (mag0132 !! fromIntegral (y .&. (0x1 :: Word32)))
                                                      return arr'

  | otherwise        = let narr = runSTArray $ do arr' <- M.thaw arr
                                                  writeArray arr' i $ (arr A.! (m32 - 1)) `xor`
                                                                      (y `shiftR` 1) `xor`
                                                                      (mag0132 !! fromIntegral (y .&. (0x1 :: Word32)))
                                                  return arr'
                       in A.elems narr ++ mt32 0 narr
  where y :: Word32
        y =  ((arr A.! i) .&. upperMask32) .|.
             ((arr A.! (if i < (n32 - 1) then i + 1 else 0))       .&. lowerMask32)

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
