{-# LANGUAGE RankNTypes #-}

module Common.Math (
    kNearest
    ,
) where

import Data.List

type Dist a = a -> a -> Int

kNearest :: Eq a => Int -> Dist a -> [a] -> [a]
kNearest k dist xs = findKNearest k' dist (delete nearest xs) [nearest]
    where nearest = minimumBy dist xs
          k'         = k - 1

findKNearest :: Eq a =>  Int -> Dist a -> [a] -> [a] -> [a]
findKNearest k dist xs neighbors = case k of
    0 -> nearest : neighbors
    _ -> findKNearest k' dist xs' (nearest : neighbors)
    where nearest    = minimumBy dist xs
          xs'        = delete nearest xs
          k'         = k - 1
