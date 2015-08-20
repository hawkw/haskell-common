module Common.Math where

type Dist a = (Num b) => a -> a -> b

kNearest :: (Num a) => a -> Dist b -> [b] -> [b]
