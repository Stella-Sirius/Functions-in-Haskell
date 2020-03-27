module Exercise_3 where

import Test.QuickCheck
import Data.List
import Control.Arrow

{-H3.1.1-}
decomposition :: Int -> [Int]
decomposition n |n ==1 = [] |otherwise =
                             iter n (primes n) where
                                                    iter n (p:_) | n < p^2 = [n | n > 1]
                                                    iter n ps@(p:ps') = let (d, r) = n `divMod` p
                                                                        in if r == 0 then p : iter d ps else iter n ps'

decomposition2 :: Int -> [(Int, Int)]
decomposition2 n = (map (head &&& length) . group) (decomposition n)

{-H3.1.2-}
isPrime :: Int -> Bool
isPrime n = let hp k n |(n - ((div n k)*k)) == 0 = k |k^2 > n = n | otherwise = hp (k+1) n in if n == 1 then False else if n>1 then hp 2 n == n else False


primes :: Int -> [Int]
primes 1 = []
primes n | n == 1 = []
         | n == 0 = []
         | otherwise = if isPrime n then (primes (n-1))++[n] else primes (n-1)

{-H3.1.3-}
takes :: [Int] -> [a] -> [[a]]
takes [] xs = []
takes ns []  = []
takes (n:ns) xs  | (length xs > n ||length xs == n) = take n xs : takes ns (drop n xs) 
                 | length xs < n =  [[x]|x<-xs]

{-H3.1.4-}
takePrimes :: [a] -> [[a]]
takePrimes xs = let ns = primes (length xs) in if length xs == 1 then takes [1] xs else takes ns xs

{-H3.2.1-}
add :: [(Integer,Integer)] -> [(Integer,Integer)] -> [(Integer,Integer)] 
add [] ys = ys
add xs [] = xs
add ((c1,g1):xs) ((c2,g2):ys)
    | g1 == g2  = if c1+c2 /= 0 then ([(c1+c2,g1)]++(add xs ys)) else add xs ys
    | g1 < g2 = ([(c1,g1)]++(add xs ((c2,g2):ys)))
    | g1 > g2 = ([(c2,g2)]++(add ((c1,g1):xs) ys))

{-H3.2.2-}
derivative :: [(Integer,Integer)] -> [(Integer,Integer)]
derivative xs = [(y*x,y-1)|(x,y)<-xs,y*x/=0]

{-H3.2.3-}
flipNegExp :: [(Integer,Integer)] -> [(Integer,Integer)]
flipNegExp [] = []
flipNegExp ((x,y):xs) | y<0 = flipNegExp (add [(x,abs y)] xs)
                      |otherwise = ((x,y):xs)

{-H3.3.1-}
unspell :: String -> [Int]
unspell [] = []
unspell (x:xs) = let mapHtD s | s == '0' = [0,0,0,0]
                            | s == '1' = [0,0,0,1]
                            | s == '2' = [0,0,1,0]
                            | s == '3' = [0,0,1,1]
                            | s == '4' = [0,1,0,0]
                            | s == '5' = [0,1,0,1]
                            | s == '6' = [0,1,1,0]
                            | s == '7' = [0,1,1,1]
                            | s == '8' = [1,0,0,0]
                            | s == '9' = [1,0,0,1]
                            | s == 'a'|| s == 'A' = [1,0,1,0]
                            | s == 'b'|| s == 'B' = [1,0,1,1]
                            | s == 'c'|| s == 'C' = [1,1,0,0]
                            | s == 'd'|| s == 'D' = [1,1,0,1]
                            | s == 'e'|| s == 'E' = [1,1,1,0]
                            | s == 'f'|| s == 'F' = [1,1,1,1]
              in mapHtD x ++ unspell xs

{-H3.3.2-}
index :: Int -> Int -> Int -> Int
index l m r = r + 2*m + 4*l

{-H3.3.3-}
ritual :: [Int] -> [Int] -> [Int]
ritual = undefined

{-H3.3.4-}
simulate :: [Int] -> [Int] -> Int -> [[Int]]
simulate = undefined

-- Tux's open source visualisation software. GNU3 LICENSE
showPenguins [] _ = return ()
showPenguins state pc = do
  putStrLn $ [c | d <- head state, let c = if d == 0 then ' ' else pc]
  showPenguins (tail state) pc

{-Some example penguin colonies-}
-- showPenguins (simulate (unspell "5a") (take 60 (repeat 0) ++ [1] ++ take 60 (repeat 0)) 31) '*'

-- showPenguins (simulate (unspell "16") (concat $ take 60 (repeat [0,1])) 60) '*'

-- showPenguins (simulate (unspell "2f") ([1,1] ++ take 30 (repeat 0)) 15) '*'

{-H3.4-}
{-WETT-}
bernoulli :: Integer -> Integer
bernoulli = undefined
{-TTEW-}
