{-# LANGUAGE RankNTypes #-}
{- |
Module      :  Common.Math
Description :  General-purpose math and stats functions
Copyright   :  (c) 2015 Hawk Weisman
License     :  MIT

Maintainer  :  hi@hawkweisman.me
Stability   :  unstable
Portability :  portable

General-purpose math and stats functions. I'm implementing these mostly for fun
and practice, but they may occasionally be useful.
-}

module Common.Math ( kNearest
                   , hammingDist
                   , kHammingNearest ) where

-------------------------------------------------------------------------------

import Data.List
import Data.Bits
import Data.Char

-------------------------------------------------------------------------------

-- |Type for a distance function over `a`, returning the distance as a value
-- of type _b_ : Ord
type Dist a b = Ord b => a -> a -> b

-- |Tail-recursive _k_-nearest-neighbor search over a list of `a`s,
-- where `a` is an instance of Eq.
--
-- The resultant list should be sorted by nearness to the target, with
-- the first element being the nearest neighbor and the last element being
-- the least near neighbor.
--
-- This should be the fastest possible implementation possible without
-- pre-processing the imputs.
--
-- Based on my Scala implementation at <https://github.com/hawkw/scala-common>.
kNearest :: Eq a => Ord b =>
            Int      -- ^Value of _k_ (the number of neighbors to find)
         -> Dist a b -- ^Distance function (instance of 'Dist')
         -> a        -- ^The value to search for the nearest neighbors to.
         -> [a]      -- ^The list of values to search for neighbors
         -> [a]
kNearest 0 _    _ _   = []
kNearest k dist x xs  = findKNearest (k - 1) (delete nearest xs) [nearest]
    where nearest     = minimumBy minDist xs
          minDist y z = dist x y `compare` dist x z
          -- Performs the actual recursive search
          findKNearest k' xs' neighbors
              | k' == 0   = neighbors'
              | otherwise = findKNearest k'' xs'' neighbors'
              where nearest'   = minimumBy minDist xs'
                    neighbors' = neighbors ++ [nearest']
                    xs''       = delete nearest' xs'
                    k''        = k' - 1

-- |Compare the Hamming distance of two lists of the same type.
--
-- The strings are required to be of equal length.
--
-- This is e a valid instance of 'Dist' and so can be used for finding the
-- nearest neighbors of a list using 'kNearest'.
hammingDist :: Eq a => [a] -> [a] -> Int
hammingDist a b
    | length a == length b = sum $ zipWith (curry same) a b
    | otherwise            = error "Length of both strings must be equal"
    where
        same (x, y)
            | x == y    = 0
            | otherwise = 1

lexicalDist :: String -> String -> Int
lexicalDist a b
    | length a == length b = sum $ zipWith (curry lexDist) a b
    | otherwise            = error "Length of both strings must be equal"
    where lexDist (x, y) = ord x - ord y

-- |Find the _k_ 'Strings' with the nearest Hamming distance
-- ('hammingDist') to the target.
--
-- This is just a batteries-included version of 'kNearest' for 'List's.
--
-- In order to compute Hamming distance, all the strings must be of equal
-- length.
kHammingNearest :: Eq a
                => Int   -- ^Value of _k_ (the number of neighbors to find)
                -> [a]   -- ^Target to find the nearest neighbors of
                -> [[a]] -- ^List of strings to search for neighbors
                -> [[a]]
kHammingNearest k = kNearest k hammingDist
