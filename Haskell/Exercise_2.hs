module Exercise_2 where

import Data.Ratio
import Test.QuickCheck hiding (choose)
import Data.List

{-H2.1.1-}
removeInvalidGuesses :: [(String,Int)] -> [(String,Int)]
removeInvalidGuesses xs = [(name,guess)| (name,guess)<-xs, name /= "",guess <= 100,guess >= 0 ]


{-H2.1.2-}
average :: [(String,Int)] -> Int
average xs = let c = [guess|(_,guess)<-xs] in if length c /= 0 then sum c `div` length c else 0

{-H2.1.3-}
winners :: [(String,Int)] -> [String]
winners xs = let c = removeInvalidGuesses xs
                 a = average c
                 ziel = [abs (guess-a)|(_,guess)<-c]  
             in if length ziel /= 0 && length c /= 0 then let mz = minimum ziel in [name|(name,guess)<- c, abs (guess-a) == mz] else []

{-H2.2-}
-- Computes the binomial coefficient "n choose k"
choose :: Integer -> Integer -> Integer
n `choose` k = product [n-k+1..n] `div` product [1..k]

bernoulli :: Integer -> Rational
{-WETT-}
bernoulli = undefined
{-TTEW-}

{-H2.3-}
-- toSet l removes the duplicates of a list l
toSet :: Eq a => [a] -> [a]
toSet = nub

-- union s t builds the union of s and t
union :: Eq a => [a] -> [a] -> [a]
union xs ys = toSet $ xs ++ ys

{-H2.3.1-}
power :: [Integer] -> [[Integer]]
power [] = [[]]
power (x:xs) = [x:powerset | powerset <- power xs] ++ power xs

subsetEq :: [Integer] -> [Integer] -> Bool
subsetEq subset s = let isMember n [] = False
                        isMember n (x:xs)| n == x = True
                                       | otherwise = isMember n xs
                    in let m = [e|e<-subset,isMember e s] 
                       in length m == length subset

{-H2.3.2-}
comparable :: [Integer] -> [Integer] -> Bool
comparable c1 c2 = subsetEq c1 c2 || subsetEq c2 c1

{-H2.3.3-}
isAntichain :: [[Integer]] -> Bool
isAntichain [] = True
isAntichain (x:xs) = let t (x:xs) | (x:xs)== [] = [] | (x:xs)== x:[] = [] | otherwise = [(x,s1)|s1<-xs]++ t xs 
                         c = t (x:xs)
                         b = [comparable x1 x2|(x1,x2)<-c]
                     in sum [1|d<-b,d] == 0

{-H2.3.4-}
antichains :: Integer -> [[[Integer]]]
antichains = undefined

{-H2.3.5-}
maxAntichainSize :: [[[Integer]]] -> Int
maxAntichainSize = undefined

prop_spernersTheorem :: Integer -> Property
prop_spernersTheorem = undefined
