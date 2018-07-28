{-# LANGUAGE BangPatterns #-}

{-|
 - Module      : System.Random.Reservoir
 - Description : Reservoir Samplers
 - Copyright   : (c) Mark Hay, 2018
 - License     : BSD3
 - Maintainer  : mah6@williams.edu
 - Stability   : experimental
 -
 - Reservoir algorithms suitable for sampling from data streams of large or unknown size.
 -}
module System.Random.Reservoir 
    ( 
    -- * Unweighted Sampling
      Res(..)
    , getSample
    , emptyRes
    , emptyRes'
    , reservoirSample
    ) where

import Control.Monad.ST
import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Data.Word
import System.Random
import System.Random.Mersenne.Pure64

-- | Wrapper for the state kept by algorithm R. 
-- | Keeps the sample as an @IntMap@, the number of elements seen, and the random seed.
data Res g a = Res
    {  
    -- | The current sample. Should usually not be modified by hand.
        resSample :: !(IntMap a)
    -- | The number of elements seen. Should almost never be modified by hand.
    ,   numSeen :: !Int
    -- | The random seed for the sample
    ,   resSeed :: !g
    } deriving (Show)

instance Functor (Res g) where
    fmap f r@(Res s _ _) = r{ resSample = fmap f s }

-- | Extracts the sample as a list.
getSample :: Res g a -> [a]
getSample = fmap snd . M.toList . resSample
{-# INLINE getSample #-}

-- | Creates a @Res@ with nothing in the sample and with the counter at zero.
emptyRes :: RandomGen g => g -> Res g a
emptyRes = Res M.empty 0
{-# INLINE emptyRes #-}

-- | A version of @emptyRes@ specialized to @System.Random.Mersenne.Pure64@.
emptyRes' :: Word64 -> Res PureMT a
emptyRes' n = emptyRes (pureMT n)
{-# INLINE emptyRes' #-}

-- | Jeffrey Vitter's "Algorithm R". Given the elements in the existing sample and a new element,
-- | every element has a equal probability of being selected.
-- | Intended to be partially applied on the sample size and used with @fold@-like functions to sample 
-- | large streams of data.
reservoirSample :: (RandomGen g) 
    => Int -- ^ The maximum number of elements to be in the sample.
    -> a  -- ^ The next element to be considered
    -> Res g a -- ^ The current wrapped sample 
    -> Res g a
reservoirSample !maxSize !x (Res samp seen seed)
    | seen < maxSize = Res (M.insert seen x samp) (seen + 1) seed
    | otherwise = Res samp' (seen + 1) seed'
    where
        (r, seed') = next seed
        r' = r `mod` seen 
        samp' = if r' < maxSize
            then M.insert r' x samp
            else samp 
{-# INLINEABLE reservoirSample #-}
