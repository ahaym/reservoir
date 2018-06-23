{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}


import Control.Monad (when)
import Control.Monad.ST
import Control.Monad.State.Strict
import Data.Vector.Algorithms.Intro (sort)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as BV
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MG
import qualified Data.Vector.Mutable as BMV
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word
import System.Random.Mersenne.Pure64
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
    let go randVals n (i,x) = do
        --replace a value at random with prob. (n / (n + i))
        g <- get
        let (r, newgen) = randomInt g
            r' = r `mod` (n + i + 1)
        put newgen
        when (r' < n) $
            MV.unsafeWrite randVals r' x

    evalStateT (V.mapM_ (go randVals n) (V.indexed rest)) (pureMT seed) --element replacement
    V.unsafeFreeze randVals

--sampling without replacement
--O(v)
{-# INLINEABLE sampleNoReplaceB #-}
sampleNoReplaceB :: Word64 -> Int -> BV.Vector a -> BV.Vector a
sampleNoReplaceB !seed !n !v = runST $ do
    let (initVals, rest) = BV.splitAt n v 
    randVals <- BV.thaw initVals --take the first n as the initial sample
    let go randVals n (i,x) = do
        --replace a value at random with prob. (n / (n + i))
        g <- get
        let (r, newgen) = randomInt g
            r' = r `mod` (n + i + 1)
        put newgen
        when (r' < n) $
            BMV.unsafeWrite randVals r' x

    evalStateT (BV.mapM_ (go randVals n) (BV.indexed rest)) (pureMT seed) --element replacement
    BV.unsafeFreeze randVals


--sampling without replacement
--O(v)
{-# INLINEABLE sampleNoReplaceMWC #-}
sampleNoReplaceMWC :: (V.Unbox a) => Word32 -> Int -> V.Vector a -> V.Vector a
sampleNoReplaceMWC !seed !n !v = runST $ do
    gen <- initialize $ V.singleton seed
    let (initVals, rest) = V.splitAt n v 
    randVals <- V.thaw initVals --take the first n as the initial sample
    let go n (i,x) = do
            --replace a value at random with prob. (n / (n + i))
            r <- uniformR (0, n + i) gen
            when (r < n) $
                MV.unsafeWrite randVals r x
    V.mapM_ (go n) (V.indexed rest) --element replacement
    V.unsafeFreeze randVals
 
{-# INLINEABLE sampleReplace #-}
--sampling with replacement
--O(n)
sampleReplace :: (V.Unbox a) => Word64 -> Int -> V.Vector a -> V.Vector a
sampleReplace !seed !n !v = G.backpermute v rvs
    where
        l = G.length v
        rvs = evalState (G.replicateM n getR) (pureMT seed)
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


a = V.fromList [1..1000000] :: V.Vector Int 
b = BV.fromList [1..1000000] :: BV.Vector Int 
main = defaultMain [bench "Mersenne Twister" $ whnf (flip (sampleNoReplace 420) a) 120
                   ,bench "MWC" $ whnf (flip (sampleNoReplaceMWC 420) a) 120
                   ,bench "BV" $ whnf (flip (sampleNoReplaceB 420) b) 120
                   ] 

main' = do
    let rands = sampleNoReplaceMWC 420 1200 a
    print rands
