{-# Language FlexibleInstances #-}
--{-# OPTIONS -Wall -Werror #-}
--{-# OPTIONS -Wall -O2 -fllvm #-}
{-# OPTIONS -Wall -O2 #-}
{-|
Module      : System.Random.MT
Description : Mersenne Twister 19937 in Haskell.
Copyright   : (c) Tatsuki-I, 2019
License     : BSD3
Maintainer  : tatsuki.devel@gmail.com

Here is a longer description of this module, containing some
commentary with @some markup@.
-}


module System.Random.MT ( randoms
                        , random
                        , randomRs
                        , randomR
                        , randomIOs
                        , randomIO
                        , mkStdGenMT
                        , getSeed
                        , getNum
                        , initGenrandVector64
                        , initGenrand64
                        , getSysRandom
                        , test5
                        , mt64Repa
                        , genrandWordRepa
                        , genrandWordRepa'
                        , genrandWordRepaD
                        , initGenrandRepa64
                        , genrandWordRepaParallel
                        , mt64Repa' ) where

import qualified Data.Array.IArray                as A
import qualified Data.Array.MArray                as M
import qualified Data.Array.Repa                  as R
import           Data.Array.Unboxed               as U  --TODO
import qualified Data.ByteString                  as B
import qualified Data.Vector.Unboxed              as UV
import qualified Data.Vector.Unboxed.Mutable      as MUV
import           Control.Monad                    ( forM_
                                                  , forM )
import           Control.Monad.ST                 ( runST )
import           Data.Bits                        ( (.&.)
                                                  , (.|.)
                                                  , xor
                                                  , shiftR
                                                  , shiftL )
import           Data.Word                        ( Word8
                                                  , Word32
                                                  , Word64 )
import           Data.Int                         ( Int32 )
import           Data.Array.ST                    ( writeArray
                                                  , runSTArray
                                                  , runSTUArray )

import           System.CPUTime                   ( getCPUTime )
import           Codec.CBOR.Magic                 ( word8ToWord
                                                  , word32ToWord
                                                  , word64ToWord )
import           Data.Ratio                       ( Ratio
                                                  , (%) )
import           Data.BitStream.ContinuousMapping ( wordToInt )
import           Data.List                        ( unfoldr )
import           System.Entropy                   ( getEntropy
                                                  , getHardwareEntropy )
import           Data.Maybe                       ( fromMaybe )
import           Data.Function                    ( (&) )
import           Control.Monad.Primitive          ( PrimMonad
                                                  , PrimBase
                                                  , PrimState
                                                  , unsafePrimToIO )
import           Control.Parallel.Strategies      ( runEval
                                                  , rpar)

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
genrandInt | is64bit   = genrandInt64
           | otherwise = genrandInt32

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
                         first :: Word32
                         first =  fromIntegral s .&. (0xffffffff :: Word32)
{-
initGenrandVector64   :: Seed -> UV.Vector Word64
initGenrandVector64 s =  first `UV.cons` (f first $ UV.fromList [1 .. (n64 - 1)])
                   where f                    :: Word64 -> UV.Vector Word64 -> UV.Vector Word64
                         f _    []            =  []
                         f prev v =  new `UV.cons` f new next
                                                 where new =  (6364136223846793005 :: Word64) * (prev `xor` (prev `shiftR` 62)) + curr
                                                       curr = UV.head v
                                                       next = UV.tail v
                         first :: Word64
                         first =  fromIntegral s-}

initGenrandVector64   :: Seed -> UV.Vector Word64
initGenrandVector64 s =  runST $ do v <- UV.thaw $ UV.fromList $ fromIntegral s : [1 .. (n64 - 1)]
                                    forM_ [1 ..  (fromIntegral n64 - 1)] $ \i ->
                                          do p <- MUV.read v (i - 1)
                                             MUV.modify v
                                                        ((6364136223846793005 :: Word64) *
                                                        (p `xor` (p `shiftR` 62)) +)
                                                        i
                                    UV.freeze v

initGenrandRepa64   :: Seed -> R.Array R.U R.DIM1 Word64
initGenrandRepa64 s = R.fromUnboxed (R.Z R.:. fromIntegral n64) $ initGenrandVector64 s

initGenrand64   :: Seed -> [Word64]
initGenrand64 s =  first : f first [1 .. (n64 - 1)]
                   where f                    :: Word64 -> [Word64] -> [Word64]
                         f _    []            =  []
                         f prev (curr : next) =  new : f new next
                                                 where new =  (6364136223846793005 :: Word64) * (prev `xor` (prev `shiftR` 62)) + curr
                         first :: Word64
                         first =  fromIntegral s

initGenrand64'   :: Seed -> [Word64]
initGenrand64' s =  unfoldr (\(prev, i) ->
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

initGenrandArray64   :: Seed -> A.Array Word64 Word64
initGenrandArray64 s =  A.listArray (0, n64 - 1) (initGenrand64 s)

genrandInt64      :: Seed -> [Word]
genrandInt64 seed =  map (word64ToWord . tempering64) $ mt64 0 $ initGenrandArray64 seed

genrandInt32      :: Seed -> [Word]
genrandInt32 seed =  map (word32ToWord . tempering32) $ mt32 0 $ initGenrandArray32 seed

genrandWordRepa        :: Int -> Seed -> R.Array R.U R.DIM1 Word
genrandWordRepa n seed =  runST $ R.computeUnboxedP $ R.map (word64ToWord . tempering64) $ head $ mt64Repa 0 0 $ initGenrandRepa64 seed

genrandWordRepa'        :: Int -- ^ length
                        -> Seed -- ^ Seed
                        -> R.Array R.U R.DIM1 Word
genrandWordRepa' n seed =  runST $ R.computeUnboxedP $ R.map (word64ToWord . tempering64) $ mt64Repa' 0 n $ initGenrandRepa64 seed

genrandWordRepaD        :: Int -- ^ length
                        -> Seed -- ^ Seed
                        -> R.Array R.U R.DIM1 Word
genrandWordRepaD n seed =  runST $ R.computeUnboxedP $ R.map (word64ToWord . tempering64) $ mt64RepaD 0 n $ R.delay $ initGenrandRepa64 seed

--genrandWordRepaParallel :: [Seed]
--                        -> [R.Array R.U R.DIM1 Word64]
{-
genrandWordRepaParallel n ss = runST $ R.computeUnboxedP $ Prelude.foldl1 (R.++) $ runEval $
                               forM ss $ \i -> (rpar . R.delay . genrandWordRepaD nn) i
                               where nn = div n $ Prelude.length ss
                            --    return $ s1 R.++ s2 -}

genrandWordRepaParallel n ss = runST $ R.computeUnboxedP $  R.map (word64ToWord . tempering64) $ Prelude.foldl1 (R.++) $ runEval $
                               do r1 <- rpar $ R.delay $ mt64RepaD 0 (div n 2) $ R.delay $ initGenrandRepa64 (ss !! 1)
                                  r2 <- rpar $ R.delay $ mt64RepaD 0 (div n 2) $ R.delay $ initGenrandRepa64 (ss !! 2)
                                  return [r1, r2]


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

test5 seed =  map (UV.map (word64ToWord . tempering64)) $ mt64' 0 $ initGenrandVector64 seed

mt64       :: Word64 -> A.Array Word64 Word64 -> [Word64]
mt64 i arr = (if i < n64 - 1
                 then id
                 else (A.elems narr ++)) (mt64 nexti narr)
             where y :: Word64
                   y =  ((arr A.!     i) .&. upperMask64) .|.
                        ((arr A.! nexti) .&. lowerMask64)
                   mag01 :: [Word64]
                   mag01 =  [0x0, matrixA64]
                   (param, nexti) |  i < (n64 - m64) = (i + m64,         i + 1)
                                  |  i < (n64 - 1)   = (i + (m64 - n64), i + 1)
                                  |  otherwise       = (m64 - 1,             0)
                   narr =  runSTArray $ do arr' <- M.thaw arr
                                           writeArray arr' i $ (arr A.! param) `xor`
                                                               (y `shiftR` 1)  `xor`
                                                               (mag01 !! fromIntegral (y .&. 0x1))
                                           return arr'

mt64'       :: Word64 -> UV.Vector Word64 -> [UV.Vector Word64]
mt64' i v = (if i < n64 - 1
                then id
                else (nv :)) (mt64' (fromIntegral nexti) nv)
             where y :: Word64
                   y =  ((v UV.! fromIntegral i) .&. upperMask64) .|.
                        ((v UV.! nexti) .&. lowerMask64)
                   mag01 :: [Word64]
                   mag01 =  [0x0, matrixA64]
                   (param, nexti) |  i < (n64 - m64) = (fromIntegral $ i + m64, fromIntegral (i + 1))
                                  |  i < (n64 - 1)   = (fromIntegral $ i + (m64 - n64), fromIntegral (i + 1))
                                  |  otherwise       = (fromIntegral m64 - 1,             0)
                   nv :: UV.Vector Word64
                   nv =  runST $ do v' <- UV.thaw v
                                    MUV.write v' (fromIntegral i) $ (v UV.! param) `xor`
                                                         (y `shiftR` 1)  `xor`
                                                         (mag01 !! fromIntegral (y .&. 0x1))
                                    UV.freeze v'

mt64Repa       :: Word64                      -- ^ Iterator
               -> Int                         -- ^ Count of random numbers
               -> R.Array R.U R.DIM1 Word64   -- ^ Init by seed
               -> [R.Array R.U R.DIM1 Word64] -- ^ Random number array
mt64Repa i n v =  (if i < n64 - 1
                then id
                else (nv :)) (mt64Repa (fromIntegral nexti) n nv)
             where y :: Word64
                   y =  ((uv UV.! fromIntegral i) .&. upperMask64) .|.
                        ((uv UV.! nexti) .&. lowerMask64)
                   mag01 :: [Word64]
                   mag01 =  [0x0, matrixA64]
                   (param, nexti) |  i < (n64 - m64) = (fromIntegral $ i + m64, fromIntegral (i + 1))
                                  |  i < (n64 - 1)   = (fromIntegral $ i + (m64 - n64), fromIntegral (i + 1))
                                  |  otherwise       = (fromIntegral m64 - 1,             0)
                   nv :: R.Array R.U R.DIM1 Word64
                   nv =  R.fromUnboxed (R.Z R.:. 312) $
                         runST $ do v' <- UV.thaw uv
                                    MUV.write v' (fromIntegral i) $ (uv UV.! param) `xor`
                                                         (y `shiftR` 1)  `xor`
                                                         (mag01 !! fromIntegral (y .&. 0x1))
                                    UV.freeze v'
                   uv = R.toUnboxed v
                   ln = (div n 312) + 1

mt64Repa'       :: Word64                    -- ^ Iterator
                -> Int                       -- ^ Count of random numbers
                -> R.Array R.U R.DIM1 Word64 -- ^ Init by seed
                -> R.Array R.U R.DIM1 Word64 -- ^ Random number array
mt64Repa' i n v | i < n64 - 1 = R.computeUnboxedS $ R.delay (mt64Repa' (fromIntegral nexti) n nv)
                | n < 312     = R.computeUnboxedS $ R.delay v
                | otherwise   = R.computeUnboxedS $ (R.delay nv R.++) (R.delay (mt64Repa' (fromIntegral nexti) (n - fromIntegral n64) nv))
             where y :: Word64
                   y =  ((uv UV.! fromIntegral i) .&. upperMask64) .|.
                        ((uv UV.! nexti) .&. lowerMask64)
                   mag01 :: [Word64]
                   mag01 =  [0x0, matrixA64]
                   (param, nexti) |  i < (n64 - m64) = (fromIntegral $ i + m64, fromIntegral (i + 1))
                                  |  i < (n64 - 1)   = (fromIntegral $ i + (m64 - n64), fromIntegral (i + 1))
                                  |  otherwise       = (fromIntegral m64 - 1,             0)
                   nv :: R.Array R.U R.DIM1 Word64
                   nv =  R.fromUnboxed (R.Z R.:. 312) $
                         runST $ do v' <- UV.thaw uv
                                    MUV.write v' (fromIntegral i) $ (uv UV.! param) `xor`
                                                         (y `shiftR` 1)  `xor`
                                                         (mag01 !! fromIntegral (y .&. 0x1))
                                    UV.freeze v'
                   uv = R.toUnboxed v

mt64RepaD       :: Word64                    -- ^ Iterator
                -> Int                       -- ^ Count of random numbers
                -> R.Array R.D R.DIM1 Word64 -- ^ Init by seed
                -> R.Array R.D R.DIM1 Word64 -- ^ Random number array
mt64RepaD i n v | i < n64 - 1 = mt64RepaD (fromIntegral nexti) n nv
                | n < 312     = v
                | otherwise   = (nv R.++) (mt64RepaD (fromIntegral nexti) (n - fromIntegral n64) nv)
             where y :: Word64
                   y =  ((uv UV.! fromIntegral i) .&. upperMask64) .|.
                        ((uv UV.! nexti) .&. lowerMask64)
                   mag01 :: [Word64]
                   mag01 =  [0x0, matrixA64]
                   (param, nexti) |  i < (n64 - m64) = (fromIntegral $ i + m64, fromIntegral (i + 1))
                                  |  i < (n64 - 1)   = (fromIntegral $ i + (m64 - n64), fromIntegral (i + 1))
                                  |  otherwise       = (fromIntegral m64 - 1,             0)
                   nv :: R.Array R.D R.DIM1 Word64
                   nv =  R.delay $ R.fromUnboxed (R.Z R.:. 312) $
                         runST $ do v' <- UV.thaw uv
                                    MUV.write v' (fromIntegral i) $ (uv UV.! param) `xor`
                                                         (y `shiftR` 1)  `xor`
                                                         (mag01 !! fromIntegral (y .&. 0x1))
                                    UV.freeze v'
                   uv = (R.toUnboxed . R.computeUnboxedS) v



 -- R.computeUnboxedS $ R.reshape (R.ix2 2 3) $ R.append y y

tempering32   :: Word32 -> Word32
tempering32 x =  x & (\v ->  (v `shiftR` 11) `xor` v)
                   & (\v -> ((v `shiftL`  7) .&.
                             (0x9d2c5680 :: Word32)) `xor` v)
                   & (\v -> ((v `shiftL` 15) .&.
                             (0xefc60000 :: Word32)) `xor` v)
                   & (\v ->  (v `shiftR` 18) `xor` v)

tempering64   :: Word64 -> Word64
tempering64 x =  x & (\v -> ((v `shiftR` 29) .&.
                             (0x5555555555555555 :: Word64)) `xor` v)
                   & (\v -> ((v `shiftL` 17) .&.
                             (0x71D67FFFEDA60000 :: Word64)) `xor` v)
                   & (\v -> ((v `shiftL` 37) .&.
                             (0xFFF7EEE000000000 :: Word64)) `xor` v)
                   & (\v ->  (v `shiftR` 43) `xor` v)

bsToWord :: B.ByteString -> Word
bsToWord =  B.foldr (test2) 0


--test = ((0 :: Word) (.|.) . (`shiftL` 8) . fromIntegral) 1

test2 :: Word8 -> Word -> Word
test2 r l = ((l .|.) . (flip shiftL 8) . (word8ToWord)) r

test3 = (flip ((. ((`shiftL` 8) . word8ToWord)) . (.|.)))

getSysRandom :: IO B.ByteString
getSysRandom =  fromMaybe <$> getEntropy 8
                          <*> getHardwareEntropy 8




{- mwc-random like interface -}
{-
class Variate a where
  uniform  :: (PrimMonad m) => Gen (PrimMonad m) -> m a
  uniformR :: (PrimMonad m) => (a, a) -> Gen (PrimState m) -> m a
-}
