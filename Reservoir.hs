{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
import Control.Monad (when)
import Control.Monad.ST
import Control.Monad.State.Strict
import Data.Vector.Algorithms.Intro (sort)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word
import System.Random.Mersenne.Pure64
import System.Random.Mersenne
import System.Random (randomR)
import System.Random.MWC
import Gauge


--Cuts down a vector to size k by sampling uniformly at random.
--O(v + k log k)
weed :: (V.Unbox a) => Word64 -> Int -> V.Vector a -> V.Vector a
weed seed k v = if k >= l' then v else V.backpermute v $ V.modify sort v'
    where
        l' = V.length v
        v' = sampleNoReplace seed k $ V.enumFromN 0 (l' - 1)

--Cuts down a vector to size k or less by sampling uniformly at random
--O(k log k)
weedApprox :: (V.Unbox a) => Word64 -> Int -> V.Vector a -> V.Vector a
weedApprox seed k v = if k >= l' then v else V.backpermute v $ V.modify sort (V.uniq v')
    where
        l' = V.length v
        v' = sampleReplace seed k $ V.enumFromN 0 (l' - 1)


--sampling without replacement
--O(v)
{-# INLINEABLE sampleNoReplace #-}
sampleNoReplace :: (V.Unbox a) => Word64 -> Int -> V.Vector a -> V.Vector a
sampleNoReplace !seed !n !v = runST $ do
    let (initVals, rest) = V.splitAt n v 
    randVals <- V.thaw initVals --take the first n as the initial sample
    evalStateT (V.mapM_ (go randVals n) (V.indexed rest)) (pureMT seed) --element replacement
    V.unsafeFreeze randVals
    where
        go randVals n (i,x) = do
            --replace a value at random with prob. (n / (n + i))
            g <- get
            let (r, newgen) = randomInt g
                r' = r `mod` (n + i + 1)
            put newgen
            when (r' < n) $
                MV.unsafeWrite randVals r' x

{-# INLINEABLE sampleReplace #-}
--sampling with replacement
--O(n)
sampleReplace :: (V.Unbox a) => Word64 -> Int -> V.Vector a -> V.Vector a
sampleReplace !seed !n !v = V.backpermute v rvs
    where
        l = V.length v
        rvs = evalState (V.replicateM n getR) (pureMT seed)
        getR = do
            gen <- get
            let (r, newgen) = randomInt gen
            put newgen
            return $ r `mod` l

{-# INLINEABLE sampleReplaceMWC #-}
sampleReplaceMWC :: (V.Unbox a) => Word32 -> Int -> V.Vector a -> V.Vector a
sampleReplaceMWC !seed !n !v = runST $ do
    let l = V.length v
    gen <- initialize $ V.singleton seed
    rvs <- V.replicateM n $ uniformR (0, l) gen
    return $ V.backpermute v rvs

{--
 - Uses impure mersenne, slower than pure for some reason
 -
{-# INLINEABLE sampleReplace' #-}
sampleReplace' :: (V.Unbox a) => Word32 -> Int -> V.Vector a -> IO (V.Vector a)
sampleReplace' !seed !n !v = do
    gen <- getStdGen
    v' <- rvs gen
    return $ V.backpermute v v'
    where
        l = V.length v
        rvs g = V.replicateM n (getR g)
        getR g = do
            r <- random g :: IO Int
            return $ r `mod` l
--}


a = V.fromList [1..10000] :: V.Vector Int 
main' = defaultMain [bench "Mersenne Twister" $ whnf (flip (sampleReplace 420) a) 1200
                   ,bench "MWC" $ whnf (flip (sampleReplaceMWC 420) a) 1200
                   ] 

main = do
    let rands = sampleReplaceMWC 420 1200 a
    print rands
