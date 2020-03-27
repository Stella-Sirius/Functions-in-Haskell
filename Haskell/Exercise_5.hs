module Exercise_5 where
import Test.QuickCheck
import Data.List
import qualified Data.Map as Map

{- H5.3 -}
{-WETT-}
type Vertex = Int
type Edge = (Vertex, Vertex)
type Graph = ([Vertex], [Edge])

longestPath :: Graph -> Vertex -> Int
longestPath (vs,[]) z = 0
longestPath (vs,es) z = longest' [1] 0 0 
  where
  nodes = Map.fromListWith (++) $ map (\(k,v) -> (k,[v])) es
  longest' ys n p
    | ys == [z] = n
    | z `elem` ys = longest' res (n + 1) n
    | null $ ys \\ [z] = p
    | otherwise = longest' res (n + 1) p
      where
        res = nub $ concat [y|Just y<- [Map.lookup x nodes| x<-ys]]



{-TTEW-}

-- generates a DAG with u vertices and only one node without incoming edges
-- you can use this function to test your implementation using QuickCheck

genDag :: Int -> Gen Graph
genDag n = let v = [1..n] in
  do b <- mapM (\i -> choose (1,n-i)) [1..n-1]
     t <- mapM (\(c,i) -> vectorOf c (choose (i+1, n))) (zip b [1..n])
     let e = nub $ ([(1, i) | i<-[2..n]] ++ edges t 1 [])
     return $ (v,e)
  where
    edges [] _ acc = acc
    edges (ts:xs) i acc = edges xs (i+1) (acc ++ [(i,t) | t<-ts])
