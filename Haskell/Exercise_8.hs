module Exercise_8 where 

import Data.List

{-H.8.1-}

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Show, Eq)

showGraphviz :: Show a => Tree a -> String
showGraphviz t =
  let
    show1 x Leaf = [] 
    show1 x (Node _ y _) = [show x ++ " -- " ++ show y ++ ";"]

    show' Leaf = [] 
    show' (Node l x r) = 
      show1 x l ++ show1 x r ++ show' l ++ show' r
  in
    unlines $ ["graph T {"] ++ show' t ++ ["}"]


t = (Node (Node Leaf 'N' Leaf) 'L'(Node Leaf 'T' Leaf))     
{-H.8.1.1-}
symmetric :: Eq a => Tree a -> Bool
symmetric Leaf = True
symmetric (Node l v r) = l == r

{-H.8.1.2-}
isBST :: Ord a => Tree a -> Bool
isBST Leaf = True
isBST (Node Leaf a Leaf) = True
isBST (Node Leaf v1 (Node l3 v3 r3)) = (v1 < v3) && isBST (Node l3 v3 r3)
isBST (Node (Node l2 v2 r2) v1 Leaf) = (v2 < v1) && isBST (Node l2 v2 r2)
isBST (Node (Node l2 v2 r2) v1 (Node l3 v3 r3)) = (v2 < v1) && (v1 < v3) && isBST (Node l2 v2 r2) && isBST (Node l3 v3 r3)

isAlmostComplete :: Tree a -> Bool
isAlmostComplete Leaf = True
isAlmostComplete (Node Leaf v Leaf) = True
isAlmostComplete n = ishelp n 0 (height n)
                      where   
                            ishelp (Node l v r) a h = if a < (h-2) then helps2 (Node l v r) && ishelp l (a+1) h && ishelp r (a+1) h else True
                            helps2 (Node Leaf v Leaf) = False
                            helps2 (Node Leaf v r) = False
                            helps2 (Node l v Leaf) = False
                            helps2 (Node l v r) = True
                            height Leaf = 0
                            height (Node l v r) = 1 + (max(height l) (height r))

buildBST :: Ord a => [a] -> Tree a
buildBST [] = Leaf
buildBST xs = Node (buildBST left) mid (buildBST right)
              where insert Leaf x = Node Leaf x Leaf
                    insert (Node t1 v t2) x 
                                   | v == x = Node t1 v t2
                                   | v  < x = Node t1 v (insert t2 x)
                                   | v  > x = Node (insert t1 x) v t2
                    (mid, right) = middleEl xs
                    left = xs \\ (mid:right)
                    middleEl s = mEl s s    
                    mEl    []    (h:s2) = (h, s2)
                    mEl (_:[])   (h:s2) = (h, s2)
                    mEl (_:_:s1) (_:s2) = mEl s1 s2
                                    
                 

rangeSubtree :: Ord a => Tree a -> (a, a) -> Tree a
rangeSubtree Leaf (_,_) = Leaf
rangeSubtree t@(Node l v r) q@(a,b) 
                        | v < a = rangeSubtree r q
                        | v > b = rangeSubtree l q
                        | otherwise = Node (rangeSubtree l q) v (rangeSubtree r q)

{-H8.2-}

{-WETT-}
shoefa :: (Num a, Ord a) => [a] -> Int
shoefa xs = undefined 
{-TTEW-}

{-H8.3-}
data Poly a = Poly [(a, Integer)]
  deriving (Eq)

-- takes the derivative of a polynomial
polyDeriv :: Num a => Poly a -> Poly a
polyDeriv (Poly ps) = Poly [(fromInteger e * c, e - 1) | (c, e) <- ps, e /= 0]

-- addition of two polynomials
polyAdd :: (Eq a, Num a) => Poly a -> Poly a -> Poly a
polyAdd (Poly []) q = q
polyAdd p (Poly []) = p
polyAdd p@(Poly ((c1, e1) : ps)) q@(Poly ((c2, e2) : qs))
  | e1 < e2      = let Poly rs = polyAdd (Poly ps) q in
                   Poly ((c1, e1) : rs)
  | e1 > e2      = let Poly rs = polyAdd p (Poly qs) in
                   Poly ((c2, e2) : rs)
  | c1 + c2 == 0 = polyAdd (Poly ps) (Poly qs)
  | otherwise    = let Poly rs = polyAdd (Poly ps) (Poly qs) in
                   Poly ((c1 + c2, e1) : rs)

{-H8.3.1-}
instance Show a => Show (Poly a) where
  show (Poly []) = "0"
  show (Poly [(a, b)]) = show a ++ "x^{" ++ show b ++ "}"
  show (Poly (x:xs)) = show (Poly [x]) ++ " + " ++ show (Poly xs)  

{-H8.3.2-}
polyShift :: (Eq a, Num a) => (a, Integer) -> Poly a -> Poly a
polyShift (a,b) (Poly []) = Poly []
polyShift (a,b) t@(Poly ((c,d):xs)) = Poly (phelp (a,b) t)
                                    where 
                                      phelp (a,b) (Poly []) = []
                                      phelp (a,b) (Poly ((c,d):xs)) = [(a*c,b+d)] ++ phelp (a,b) (Poly xs)


{-H8.3.3-}
instance (Eq a, Num a) => Num (Poly a) where
    x + y = polyAdd x y
    x * y = polymul x y where
                        polymul t (Poly (x:xs)) = polyAdd (polyShift x t) (polymul t (Poly xs))
                        polymul _ (Poly []) = Poly []
                        polymul (Poly []) _ = Poly []
    x - y = polyAdd x (negate y)
    negate (Poly []) = Poly []
    negate (Poly t@((a,b):xs)) = Poly (help t)
                                    where help ((a,b):xs) = [((-1) * a, b)] ++ help xs
                                          help [] = []
    abs = undefined
    signum = undefined
    fromInteger i = if i /= 0 then Poly [(fromInteger i, 0)] else Poly []

{-H8.3.4-}
leadCoeff :: Num a => Poly a -> a
leadCoeff (Poly []) = 0
leadCoeff t@(Poly (x:xs)) = let help (Poly ((a,b):xs)) = [a] ++ help (Poly xs)
                                help (Poly []) = []
                                help2 (Poly ((a,b):xs)) = [b] ++ help2 (Poly xs)
                                help2 (Poly []) = []
                                in (help t) !! head (elemIndices (degree t) (help2 t))

degree :: Poly a -> Integer
degree (Poly []) = -1
degree t@(Poly (x:xs)) = let help (Poly ((a,b):xs)) = [b] ++ help (Poly xs)
                             help (Poly []) = []
                           in foldr1 max (help t)

{-H8.3.5-}
polyDivMod :: (Fractional a, Eq a) => Poly a -> Poly a -> (Poly a, Poly a)
polyDivMod (Poly []) _ = (0,0)
polyDivMod p (Poly []) = (0,p)
polyDivMod p@(Poly (x:xs)) q@(Poly (y:ys)) = help p q 0
                                       where help r q s = let t = Poly ([((leadCoeff r/leadCoeff q),(degree r - degree q))]) in if r /= 0 && degree r >= degree q then help (r - t*q) q (s + t) else (s,r)



{-H8.3.6-}
polyDiv :: (Fractional a, Eq a) => Poly a -> Poly a -> Poly a
polyDiv p q = fst (polyDivMod p q)

polyMod :: (Fractional a, Eq a) => Poly a -> Poly a -> Poly a
polyMod p q = snd (polyDivMod p q)

{-H8.3.7-}
polyEval :: Num a => Poly a -> a -> a
polyEval (Poly []) a = 0
polyEval (Poly ((b,c):xs)) a = b * a^c + polyEval (Poly xs) a

{-H8.3.8-}
genSturm :: (Fractional a, Eq a) => Poly a -> Poly a -> [Poly a]
genSturm = undefined 

sturm :: (Fractional a, Eq a) => Poly a -> [Poly a]
sturm = undefined 

{-H8.3.9-}
countRootsBetween :: Poly Rational -> Rational -> Rational -> Int
countRootsBetween = undefined 


