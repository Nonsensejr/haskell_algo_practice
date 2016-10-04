import Data.List (maximumBy, elemIndex)
import Data.Maybe (Maybe, fromJust)

split :: (Ord a, Num a) => [a] -> ([a], [a])
split l = splitAt (((length l) + 1) `div` 2) l

maxBySum :: (Ord a, Num a) => [[a]] -> [a]
maxBySum xs =
    maximumBy aSum xs
    where aSum x y = sum x `compare` sum y

maxIdx :: (Ord a, Num a) => [a] -> Int
maxIdx arr =
    fromJust (elemIndex (maximumBy aSum arr) arr)
    where aSum x y = x `compare` y

maxFromMidl :: (Ord a, Num a) => [a] -> [a]
maxFromMidl [] = [] 
maxSubarray [a] = [a]
maxFromMidl arr =
    fst (splitAt (maxIdx (scanl (+) 0 arr)) arr)

maxFromMidr :: (Ord a, Num a) => [a] -> [a]
maxFromMidr [] = [] 
maxSubarray [a] = [a]
maxFromMidr arr =
    snd (splitAt (maxIdx (scanr (+) 0 arr)) arr)

maxWithMid :: (Ord a, Num a) => [a] -> [a] -> [a]
maxWithMid l r =
    (maxFromMidr l) ++ (maxFromMidl r)

maxSubarray :: (Ord a, Num a) => [a] -> [a]
maxSubarray [] = []
maxSubarray [a] = [a]
maxSubarray arr =
    maxBySum [ maxSubarray l, maxSubarray r, maxWithMid l r ]
    where
        l = fst (split arr)
        r = snd (split arr)
