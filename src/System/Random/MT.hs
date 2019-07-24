{-# Language FlexibleInstances #-}
--{-# OPTIONS -Wall -Werror #-}
{-# OPTIONS -Wall -O2 -fllvm #-}

module System.Random.MT ( randoms
                        , random
                        , randomRs
                        , randomR
                        , randomIOs
                        , randomIO
                        , mkStdGenMT
                        , getSeed
                        , getNum
                        , initGenrand64
                        , getSysRandom ) where

import qualified Data.Array.IArray  as A
import qualified Data.Array.MArray  as M
import qualified Data.Array.Repa    as R
import           Data.Array.Unboxed as U  --TODO
import qualified Data.ByteString    as B
import           Data.Bits                        ( (.&.)
                                                  , (.|.)
                                                  , xor
                                                  , shiftR
                                                  , shiftL)
import           Data.Word                        ( Word8
                                                  , Word32
                                                  , Word64 )
import           Data.Int                         ( Int32 )
import           Data.Array.ST                    ( newArray
                                                  , readArray
                                                  , writeArray
                                                  , runSTArray
                                                  , runSTUArray )

import           System.CPUTime                   ( getCPUTime )
import           Codec.CBOR.Magic                 ( word32ToWord )
import           Data.Ratio                       ( Ratio
                                                  , (%) )
import           Data.BitStream.ContinuousMapping ( wordToInt )
import           Data.List                        ( unfoldr )
import           System.Entropy                   ( getEntropy
                                                  , getHardwareEntropy )
import           Data.Maybe                       ( fromMaybe )
import           Data.Function                    ( (&) )


class RandomMT a where
  randoms   :: StdGenMT -> [a]

  random     :: StdGenMT -> (a, StdGenMT)
  random gen =  (randoms gen !! getNum gen, nextGen gen)

  randomIOs  :: IO [a]
  randomIOs  =  randoms . mkStdGenMT <$> getCPUTime

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
  randoms gen = map tempering32 $ mt32 0 $ initGenrandArray32 $ getSeed gen

class (Num a, Ord a, Integral a, RandomMT a) => RandomMTR a where
  randomRs              :: (a, a) -> StdGenMT -> [a]
  randomRs (lo, hi) gen |  lo > hi   = randomRs (hi, lo) gen
                        |  otherwise = map ((+ lo) . (`mod` r)) (randoms gen)
                        where r = hi - lo + 1

  randomR       :: (a, a) -> StdGenMT -> (a, StdGenMT)
  randomR r gen =  (randomRs r gen !! getNum gen, nextGen gen)

instance RandomMTR Int  where
instance RandomMTR Word where

data StdGenMT = StdGenMT Int Seed

nextGen                :: StdGenMT -> StdGenMT
nextGen (StdGenMT i s) =  StdGenMT (i + 1) s

type Seed = Word

class (Integral g) => RandomGenMT g where
  mkStdGenMT :: g -> StdGenMT

instance RandomGenMT Word32  where
  mkStdGenMT i = StdGenMT 0 (fromIntegral i)
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
n32         :: Word32
n32         =  624
m32         :: Word32
m32         =  397
matrixA32   :: Word32
matrixA32   =  0x9908b0df -- constant vector a
upperMask32 :: Word32
upperMask32 =  0x80000000 -- most significant w-r bits
lowerMask32 :: Word32
lowerMask32 =  0x7fffffff -- least significant r bits

n64         :: Word64
n64         =  312
m64         :: Word64
m64         =  156
matrixA64   :: Word64
matrixA64   =  0xB5026F5AA96619E9
upperMask64 :: Word64
upperMask64 =  0xFFFFFFFF80000000
lowerMask64 :: Word64
lowerMask64 =  0x7FFFFFFF

initGenrand32   :: Seed -> [Word32]
initGenrand32 s =  first : f first [1 .. (n32 - 1)]
--unfoldr f' (first, 0)
                   where f                    :: Word32 -> [Word32] -> [Word32]
                         f _    []            =  []
                         f prev (curr : next) =  new : f new next
                                                 where new =  ((1812433253 :: Word32) * (prev `xor` (prev `shiftR` 30)) + curr) .&. (0xffffffff :: Word32)
{-                       f' = \(prev, i) -> if i < n32
                                               then Just ( prev
                                                         , (((1812433253 :: Word32) *
                                                           (prev `xor`
                                                           (prev `shiftR` 30)) + i) .&.
                                                           (0xffffffff :: Word32), i + 1 ))
                                               else Nothing-}
                         first :: Word32
                         first =  fromIntegral s .&. (0xffffffff :: Word32)

initGenrand64   :: Seed -> [Word64]
initGenrand64 s =  unfoldr (\(prev, i) ->
                             if i < n64
                                then Just ( prev
                                          , ((6364136223846793005 :: Word64) *
                                            (prev `xor`
                                            (prev `shiftR` 62)) + i
                                            , i + 1))
                                else Nothing)
                           (fromIntegral s, 0)

initGenrandRepa32   :: Seed -> R.Array R.U R.DIM1 Word32
initGenrandRepa32 s =  R.fromListUnboxed (R.Z R.:. fromIntegral n32) $ initGenrand32 s

initGenrandArray32   :: Seed -> A.Array Word32 Word32
initGenrandArray32 s =  A.listArray (0, n32 - 1) (initGenrand32 s)

genrandInt32      :: Seed -> [Word]
genrandInt32 seed =  map (word32ToWord . tempering32) $ mt32 0 $ initGenrandArray32 seed

randomishIntArray :: R.Shape sh => Seed -> R.Array R.U sh Int32
randomishIntArray =  undefined

{- default seed is 5489 -}

mt32       :: Word32 -> A.Array Word32 Word32 -> [Word32]
mt32 i arr = (if i < n32 - 1
                 then id
                 else (A.elems narr ++)) (mt32 nexti narr)
             where y :: Word32
                   y =  ((arr A.!     i) .&. upperMask32) .|.
                        ((arr A.! nexti) .&. lowerMask32)
                   mag01 :: [Word32]
                   mag01 =  [0x0, matrixA32]
                   (param, nexti) |  i < (n32 - m32) = (i + m32,         i + 1)
                                  |  i < (n32 - 1)   = (i + (m32 - n32), i + 1)
                                  |  otherwise       = (m32 - 1,             0)
                   narr =  runSTArray $ do arr' <- M.thaw arr
                                           writeArray arr' i $ (arr A.! param) `xor`
                                                               (y `shiftR` 1)  `xor`
                                                               (mag01 !! fromIntegral (y .&. 0x1))
                                           return arr'

{-
initGenrand64   :: Seed -> [Word64]
initGenrand64 s =  unfoldr (\(prev, i) ->
                             if i < n64
                                then Just ( prev
                                          , ((6364136223846793005 :: Word64) *
                                            (prev `xor`
                                            (prev `shiftR` 62)) + i
                                            , i + 1))
                                else Nothing)
                           (s, 0)
-}

tempering32   :: Word32 -> Word32
tempering32 x =  x & (\v ->  (v `shiftR` 11)                             `xor` v)
                   & (\v -> ((v `shiftL`  7) .&. (0x9d2c5680 :: Word32)) `xor` v)
                   & (\v -> ((v `shiftL` 15) .&. (0xefc60000 :: Word32)) `xor` v)
                   & (\v ->  (v `shiftR` 18)                             `xor` v)

tempering64   :: Word64 -> Word64
tempering64 x =  x & (\v -> ((v `shiftR` 29) .&. (0x5555555555555555 :: Word64)) `xor` v)
                   & (\v -> ((v `shiftL` 17) .&. (0x71D67FFFEDA60000 :: Word64)) `xor` v)
                   & (\v -> ((v `shiftL` 37) .&. (0xFFF7EEE000000000 :: Word64)) `xor` v)
                   & (\v ->  (v `shiftR` 43)                                     `xor` v)

bsToWord :: B.ByteString -> Word
bsToWord =  B.foldr (flip ((. ((`shiftL` 8) . fromIntegral)) . (.|.))) 0


--test = ((0 :: Word) (.|.) . (`shiftL` 8) . fromIntegral) 1

test2 :: Word8 -> Word -> Word
test2 r l = ((l .|.) . (flip shiftL 8) . (fromIntegral)) r

getSysRandom :: IO Word
getSysRandom =  do e <- getEntropy 8
                   ((bsToWord . fromMaybe e) <$> getHardwareEntropy 8)
