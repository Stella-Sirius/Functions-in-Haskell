module Exercise_1 where

import Test.QuickCheck

{-H1.1-}
myPair :: Integer -> Integer -> Integer
myPair x y | x > 0 = 2 * myPair (x - 1) y | otherwise = 2*y + 1


{-H1.2-}
myFst :: Integer -> Integer
myFst x | (x - ((div x 2)*2)) == 0 = 1 + (myFst (x `div` 2)) | otherwise = 0

{-H1.3-}
mySnd :: Integer -> Integer
mySnd x | (x - ((div x 2)*2)) == 0 = mySnd (x `div` 2) | otherwise = div (x - 1) 2

{-H1.4-}
prop_myPair :: Integer -> Integer -> Property
prop_myPair x y = x > 0 ==> y > 0 ==> myPair x y == (2 ^ myFst (myPair x y)) * (2*mySnd (myPair x y) + 1)
{-prop_myPair x y = myFst (myPair x mySnd (myPair x y)) y == myPair x (mySnd (myPair x y))-}

{-H2.1-}
equivMod :: Integer -> Integer -> Integer -> Bool
equivMod n a b = let m = a - b in (m - ((div m n)*n)) == 0
{-equivMod n a b = let modulo x y = let result = mod x y in if result >= 0 then result else result + y in if n>0 then modulo b n == a else False-}
{-equivMod n a b = if n /= 0 then a == mod b n else True-}
{-equivMod n a b  = if n /= 0 then let m = b - ((b `div` n)*n) in a == m else if b == 0 then a == 0 else if a == 0 && n>0 then True else False-}


{-H2.2-}
quadRes :: Integer -> Integer -> Bool
{-WETT-}
quadRes = undefined
{-TTEW-}

{-H2.3-}
legendre :: Integer -> Integer -> Integer
legendre p a = let check a b p| b < a = b^2 == mod a p || check a (b+1) p | otherwise = False in if a == p || a == mod 0 p then 0 else if a /= (mod 0 p) && check a 0 p then 1 else if check a 0 p == False && a == mod 0 p then -1 else 0



{-H2.4-}
prime :: Integer -> Bool
prime  n = let hp k n |(n - ((div n k)*k)) == 0 = k |k^2 > n = n | otherwise = hp (k+1) n in if n == 1 then False else if n>1 then hp 2 n == n else False

{-H2.5-}
prop_eulersCrit :: Integer -> Integer -> Property
prop_eulersCrit p a = a>=0 ==> prime p ==> legendre p a == (^) a ((p-1) `div` 2) `mod` p