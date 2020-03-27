module Exercise_7 where

{-H7.2-}
matchesPath :: String -> String -> Bool
matchesPath [] [] = True
matchesPath [] xs = False
matchesPath ['*'] [] = True
matchesPath ['?'] [] = False
matchesPath _ [] = False
matchesPath (y:ys) (x:xs)   | y == '*' && ['*'] == take 2 ys = True --matchesPath ('/':tail ys) xs
                            | y == '*' && ['*'] == take 1 ys && length (take 2 ys) >= 2 = if matchesPath (tail ys) (x:xs) then True else matchesPath (y:ys) xs--if head (tail (tail ys)) == (head xs) then matchesPath (tail (tail ys)) xs else matchesPath (y:ys) xs
                            | y == '*' && (x == '/' || x == '.') = matchesPath ys (x:xs)
                            | y == '*' && x /= '/' = matchesPath (y:ys) xs
                            | y == '{' = or [matchesPath (ysss++zs) (x:xs)|ysss<-yssss]
                            | y == '?' = matchesPath ys xs
                            | otherwise = y == x && matchesPath ys xs
                            where
                                takecontent [] a = ([a], [])
                                takecontent (y:ys) a
                                    | y == '}' = ([a], ys)
                                    | y == ',' = ([a] ++ fst res, snd res)
                                    | otherwise = takecontent ys (a ++ [y])
                                        where 
                                            res = takecontent ys []
                                (yssss, zs) = takecontent ys [] 
{-H7.3-}
comp :: Eq b => [(Integer,(a,b))] -> [(Integer,(b,c))] -> [(Integer,(a,c))]
comp = undefined 

symcl :: Eq a => [(Integer,(a,a))] -> [(Integer,(a,a))]
symcl = undefined 

{-WETT-}
trancl :: Eq a => [(Integer,(a,a))] -> [(Integer,(a,a))]
trancl = undefined 
{-TTEW-}
