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
                   , hamming
                   , kHammingNearest ) where

-------------------------------------------------------------------------------

import Data.List
import Data.Bits
import Data.Char

-------------------------------------------------------------------------------

-- |Type for a distance function over `a`, returning the distance as an integer
type Dist a = a -> a -> Int

-- |Tail-recursive _k_-nearest-neighbor search over a list of `a`s,
-- where `a` is an instance of Eq.
--
-- This should be the fastest possible implementation possible without
-- pre-processing the imputs.
--
-- Based on my Scala implementation <https://github.com/hawkw/scala-common here>.
kNearest :: Eq a
         => Int    -- ^Value of _k_ (the number of neighbors to find)
         -> Dist a -- ^Distance function (instance of 'Dist')
         -> a      -- ^The value to search for the nearest neighbors to.
         -> [a]    -- ^The list of values to search for neighbors
         -> [a]
kNearest 0 _    _ _   = []
kNearest k dist x xs  = findKNearest k' minDist (delete nearest xs) [nearest]
    where nearest     = minimumBy minDist xs
          k'          = k - 1
          minDist a b = compare (distTo a) (distTo b)
          distTo      = dist x

-- |Helper function for 'kNearest' (performs the actual recursive search)
findKNearest :: Eq a =>  Int -> (a -> a -> Ordering) -> [a] -> [a] -> [a]
findKNearest k dist xs neighbors
    | k == 0      = nearest : neighbors
    | otherwise   = findKNearest k' dist xs' (nearest : neighbors)
    where nearest = minimumBy dist xs
          xs'     = delete nearest xs
          k'      = k - 1

-- |Compare the Hamming distance of two strings.
--
-- The strings are required to be of equal length.
--
-- This should be a valid instance of 'Dist' and so can be used for finding the
-- nearest neighbors of a 'String' using 'kNearest'.
hamming :: String -> String -> Int
hamming a b
    | length a == length b = sum $
                             map (\ (x, y) -> setBits $ xor (ord x) (ord y) ) $
                             zip a b
    | otherwise            = error "Length of both strings must be equal"
    where setBits 0 = 0
          setBits x = 1 + setBits ( x .&. (x - 1) )

-- |Find the _k_ 'Strings' with the nearest 'Hamming' distance to the target.
--
-- This is just a batteries-included version of 'kNearest' for 'String's.
--
-- In order to compute Hamming distance, all the strings must be of equal
-- length.
kHammingNearest :: Int      -- ^Value of _k_ (the number of neighbors to find)
                -> String   -- ^Target to find the nearest neighbors of
                -> [String] -- ^List of strings to search for neighbors
                -> [String]
kHammingNearest k = kNearest k hamming
