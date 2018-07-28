{-# LANGUAGE BangPatterns #-}

-- | Jeffrey Vitter's Algorithm R <http://www.cs.umd.edu/~samir/498/vitter.pdf>, suitable for unweighted sampling from a collections of unknown size.
module System.Random.Reservoir 
    ( 
      Res()
    , reservoirSample
    , getSample
    , getSeed
    , setSeed
    , emptyRes
    , emptyRes'
    ) where

import Control.Monad.ST
import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import System.Random
import System.Random.Mersenne.Pure64

data Res g a = Res
    {   resSample :: !(IntMap a)
    ,   numSeen :: !Int
    ,   resSeed :: !g
    } deriving (Show)

instance Functor (Res g) where
    fmap f r@(Res s _ _) = r{ resSample = fmap f s }

getSample :: Res g a -> [a]
getSample = fmap snd . M.toList . resSample

getSeed :: Res g a -> g
getSeed = resSeed

setSeed :: RandomGen g => g -> Res g a -> Res g a
setSeed s res = res{resSeed = s}

emptyRes :: RandomGen g => g -> Res g a
emptyRes = Res M.empty 0

emptyRes' :: Res PureMT a
emptyRes' = emptyRes (pureMT 0)

reservoirSample :: (RandomGen g) => Int -> a -> Res g a -> Res g a
reservoirSample !maxSize !x (Res samp seen seed)
    | seen < maxSize = Res (M.insert seen x samp) (seen + 1) seed
    | otherwise = Res samp' (seen + 1) seed'
    where
        (r, seed') = next seed
        r' = r `mod` seen 
        samp' = if r' < maxSize
            then M.insert r' x samp
            else samp 
