module Exercise_6 where

import Data.Ratio

{-WETT-}
traceFractran :: [Rational] -> Integer -> [Integer]
traceFractran [] y = [y]
traceFractran xt y = let hilf [] y _ = [y]; hilf (x:xs) y (z:zs) = let a =  denominator (x*fromIntegral(y)); b = numerator (x*fromIntegral(y)) in if a == 1 then [y] ++ hilf xt b xt else hilf xs y xt
                        in hilf xt y xt
{-TTEW-}
