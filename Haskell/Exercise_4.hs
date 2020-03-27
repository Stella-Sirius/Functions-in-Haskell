module Exercise_4 where

import Data.List

{-H4.1.1-}
isMultiSet :: Eq a => [(a,Int)] -> Bool
isMultiSet s = length s == sum [1|(a,m)<-s,m>0]


{-H4.1.2-}
toList :: [(a,Int)] -> [a]
toList [] = []
toList ((a,s):xs) = replicate s a ++ toList xs 

{-H4.1.3-}
toSet :: Eq a => [(a, Int)] -> [a]
toSet [] = []
toSet ((a,s):xs) = [a] ++ toSet xs

{-H4.1.4-}
toMultiSet :: Eq a => [a] -> [(a, Int)]
toMultiSet = let classifyBy (==) []     = [];
                 classifyBy (==) (x:xs) = (x:filter (== x) xs)
                                        : classifyBy (==) (filter (/= x) xs) where x /= y = not (x == y)
             in let classify = classifyBy (==) in 
                                               let headLength xs = (head xs, length xs) in map headLength . classify 

{-H4.1.5-}
multiplicity :: Eq a => a -> [(a, Int)] -> Int
multiplicity a [] = 0
multiplicity a ((b,c):xs) = if b == a then c else multiplicity a xs

{-H4.1.6-}
dotProduct :: Eq a => [(a, Int)] -> [(a, Int)] -> Int
dotProduct xs ys = let c = [((a,x),(b,y))|(a,x)<-xs,(b,y)<-ys,a == b] in sum[x*y|((a,x),(b,y))<-c]

{-H4.1.7-}
euclidean :: Eq a => [(a, Int)] -> Float
euclidean a = let b = [y*y|(x,y)<-a] in let f [] = 0; f (x:xs) = x + f xs; d = fromIntegral (f b) :: Float in sqrt d 

{-H4.1.8-}
cosine :: Eq a => [(a, Int)] -> [(a, Int)] -> Float
cosine a b = fromIntegral (dotProduct a b)/((euclidean a)*(euclidean b))

{-H4.1.9-}
vocabSimilarity :: String -> String -> Float
vocabSimilarity a b = let split d [] = [];split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s 
                      in let c = toMultiSet (split ' ' a); d = toMultiSet (split ' ' b) 
                         in cosine c d

{-H4.1.10-}
editDistance :: Eq a => [a] -> [a] -> Int
editDistance [] [] = 0
editDistance [] s2 = length s2
editDistance s1 [] = length s1
editDistance s1 s2 | last s1 == last s2 = editDistance (init s1) (init s2)
                   | otherwise = minimum [1 + editDistance (init s1) s2,
                                          1 + editDistance s1 (init s2),
                                          1 + editDistance (init s1) (init s2),
                                          if length s1 > 1 && length s2 >1 && s1 !! (length s1 - 2) == last s2 && last s1 == s2 !! (length s2 - 2) then 1 + editDistance (take (length s1 - 2) s1) (take (length s2 - 2) s2) else 99 
                                          ]


frequentWords = 
    [
        "the", "at", "there", "some", "my",
        "of", "be", "use", "her", "than",
        "and", "this", "an", "would", "first",
        "a", "have", "each", "make", "water",
        "to", "from", "which", "like", "been",
        "in", "or", "she", "him", "call",
        "is", "one", "do", "into", "who",
        "you", "had", "how", "time", "oil",
        "that", "by", "their", "has", "its",
        "it", "word", "if", "look", "now",
        "he", "but", "will", "two", "find",
        "was", "not", "up", "more", "long",
        "for", "what", "other", "write", "down",
        "on", "all", "about", "go", "day",
        "are", "were", "out", "see", "did",
        "as", "we", "many", "number", "get",
        "with", "when", "then", "no", "come",
        "his", "your", "them", "way", "made",
        "they", "can", "these", "could", "may",
        "I", "said", "so", "people", "part",
        "Alice", "Bob"
    ] 
{-H4.1.11-}
{-WETT-}
spellCorrect :: [String] -> [String] -> [[String]]
spellCorrect xs ys = let c = [[editDistance x y|y<-ys]|x<-xs]
                         in let b = [minimum c1|c1<- transpose c] 
                            in let h = [elemIndices b1 c1|(b1,c1)<-zip b $ transpose c]
                                in [[xs !! h2|h2<-h1]|h1<-h]

{-TTEW-}
