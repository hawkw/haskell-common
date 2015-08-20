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

module Common.Math ( kNearest ) where

-------------------------------------------------------------------------------

import Data.List

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
kNearest k dist x xs  = findKNearest k' minDist (delete nearest xs) [nearest]
    where nearest     = minimumBy minDist xs
          k'          = k - 1
          minDist a b = compare (distTo a) (distTo b)
          distTo      = dist x

-- |Helper function for 'kNearest' (performs the actual recursive search)
findKNearest :: Eq a =>  Int -> (a -> a -> Ordering) -> [a] -> [a] -> [a]
findKNearest k dist xs neighbors = case k of
    0 -> nearest : neighbors
    _ -> findKNearest k' dist xs' (nearest : neighbors)
    where nearest    = minimumBy dist xs
          xs'        = delete nearest xs
          k'         = k - 1
