module Exercise_10 where
import Data.List
import Test.QuickCheck

{-H10.1-}
data Player = V | H -- vertical or horizontal player
  deriving (Eq,Show)
data Field = P Player | E -- a field is occupied by a player or empty
  deriving (Eq,Show)
type Row = [Field]
type Column = [Field]
type Board = [Row] -- we assume that boards are squares and encode a board row by row
data Game = Game Board Player -- stores the currenty board and player
  deriving (Eq,Show)

-- get a given row of a board
row :: Board -> Int -> Row
row = (!!)

-- get a given column of a board
column :: Board -> Int -> Column
column = row . transpose

-- width of a board
width :: Board -> Int
width [] = 0
width (x:xs) = length x

-- height of a board
height :: Board -> Int
height = length

{-H10.1.1-}
prettyShowBoard :: Board -> String
prettyShowBoard [] = ""
prettyShowBoard (x:xs) = (concat [syn x1|x1<-x]) ++ "\n" ++ prettyShowBoard xs 
 where 
  syn c = case c of
    E -> "+"
    P p-> show p

{-H10.1.2-}
-- position on a board (row, column)
-- (0,0) corresponds to the top left corner
type Pos = (Int, Int)

isValidMove :: Game -> Pos -> Bool
isValidMove (Game [] player) p = False
isValidMove (Game [[E]] player) p = False
isValidMove (Game board player) p = if player == H then helph board p else helpv board p where
  helph board (r,c) =  let a = (board !! r); b = (a !! c); d = a !! (c+1); l = length board in
                          if (r < l) && (c+1)< l && (0 <= r) && (0 <= c)then
                            if b == E && d == E then True else False
                            else False
  helpv board (r,c) =  let a1 = (board !! r); b1 = (a1 !! c); a2 = (board !! (r+1)); b2 = (a2 !! c); l = length board in
                        if (r+1 < l) && c< l && (0 <= r) && (0 <= c) then
                          if b1 == E && b2 == E then True else False
                        else False

{-H10.1.3-}
canMove :: Game -> Bool
canMove g@(Game board player) = or [isValidMove g (x,y)|x<-[0..(height board-1)],y<-[0..(width board-1)]]

{-H10.1.4-}
updateBoard :: Board -> Pos -> Field -> Board
-- updateBoard board (r,c) f = let n = insertAt f c (drop (c+1) board2) in insertAt n r (drop (r+1) board)
updateBoard board (r,c) f = let n = insertAt f c board2 in insertAt n r board
    where
    board2 = (board !! r)
    insertAt newElement 0 (a:as) = newElement:as
    insertAt newElement i (a:as) = a : insertAt newElement (i - 1) as


{-H10.1.5-}
playMove :: Game -> Pos -> Game
playMove  (Game board player) (r,c) = if player == V then Game bh2 H
                                      else Game bv2 V
  where
  bv1 = updateBoard board (r,c) (P H)
  bv2 = updateBoard bv1 (r,c+1) (P H)
  bh1 = updateBoard board (r,c) (P V)
  bh2 = updateBoard bh1 (r+1,c) (P V)



{-H10.1.6-}
-- the first paramter of a strategy is an infite list of
-- random values between (0,1) (in case you wanna go wild with
-- probabilistic methods)
type Strategy = [Double] -> Game -> Pos


christmasAI :: Strategy
christmasAI rs g@(Game b p) = childNodes !! maxi
  where
    childNodes = [(x,y)|x<-[0..bHeight],y<-[0..bWidth],isValidMove g (x,y)]
    bHeight = height b -1
    bWidth = width b -1
    res = monteCarlo rs g p 0
    modRes = [fromIntegral(x) / fromIntegral(y)|(x,y)<-init res]
    Just maxi = (maximum modRes) `elemIndex` modRes
    
    
monteCarlo :: [Double] -> Game -> Player -> Int -> [(Int,Int)]
-- monteCarlo = undefined
monteCarlo rs game@(Game board player) p iters
  | not $ canMove game = [(0,0),(0,iters)] -- if player == p then [(-40,100)] else [(40,100)]
  | 1000 < (snd $ last listres) = listres
  | otherwise = monteCarlo' (indices++zeroIndices) (init listres) (snd $ last listres)
    where
      children = [(x,y)|x<-[0..bHeight],y<-[0..bWidth],isValidMove game (x,y)]
      bHeight = height board -1
      bWidth = width board -1
      indices = elemIndices 1 [x `div` y|(x,y)<-init listres]
      zeroIndices = [0..(length children) - 1] \\ indices
      selectedChild = if null indices then children else [children !! x|x<-indices]
      listres :: [(Int,Int)] -- result of simulation
      listres = listSims rs game children iters
      -- list of simulation of children
      listSims :: [Double] -> Game -> [Pos] -> Int-> [(Int,Int)]
      listSims rs2 g2 [] iter = [(0,iter)]
      listSims rs2 g2 (c:cs) iter = head res : listSims (drop 10 rs2) g2 cs (snd $ last res)
        where
          res = simulate rs2 (playMove g2 c) iter
      -- simulate one step with following arbritary moves
      simulate :: [Double] -> Game -> Int -> [(Int,Int)]
      simulate rs1 g1@(Game b1 p1) iter
        | not $ canMove g1 = if p1 == p then [(0,1), (0,iter)] else [(1,1), (0,iter)]
        | iter > 1000 = [(0,1), (0,iter)]
        | otherwise = simulate (drop len rs1) newGame (iter+1)
        where
          childs = [(x,y)|x<-[0..bHeight],y<-[0..bWidth],isValidMove g1 (x,y)]
          len = length childs
          Just maxIdx = (maximum $ take len rs1) `elemIndex` (take len rs1)
          selected = childs !! maxIdx
          newGame = playMove g1 selected
      monteCarlo' :: [Int] -> [(Int,Int)] -> Int -> [(Int,Int)]
      -- TODO: zero list
      monteCarlo' [] results iters1 = results ++ [(0,iters1)]
      monteCarlo' (idx:idxs) results iters1
        | 1000 < iters1 = monteCarlo' idxs (appendToList results idx (0,1)) iters1
        | otherwise = monteCarlo' idxs (appendToList results idx simRes) $ snd $ last nestRes
        where
          newGame = playMove game $ children !! idx
          nestRes = expand rs newGame iters1
          simRes = foldl1 (\(a,b) (x,y) -> (a+x,b+y)) $ init nestRes
      expand :: [Double] -> Game -> Int -> [(Int,Int)]
      expand rs3 g3@(Game b3 p3) iters3
        | not $ canMove g3 = [(0,0),(0,iters3)] -- if player == p then [(-40,100)] else [(40,100)]
        | otherwise = listres3
        where
          children3 = [(x,y)|x<-[0..bHeight],y<-[0..bWidth],isValidMove g3 (x,y)]
          listres3 = listSims rs3 g3 children3 iters3


appendToList :: [(Int, Int)] -> Int -> (Int,Int) -> [(Int,Int)]
appendToList ((a,b):ls) 0 (a1,b1) = (a + a1,b + b1):ls
appendToList (l:ls) n x = l:appendToList ls (n - 1) x


{-H10.1.7-}
play :: [[Double]] -> Int -> Strategy -> Strategy -> ([Board],Player)
play rss dim sv sh = let b = replicate dim $ replicate dim E in playGame rss sh sv (Game b V) []

playGame :: [[Double]] -> Strategy -> Strategy -> Game  -> [Board] -> ([Board],Player)
playGame (r:rs) h v g@(Game b p) bs
  | not (canMove g) = if p == H then (bs, V) else (bs,H)
  | p == H = if isValidMove g ps then playGame rs h v ng (bs++[b1]) else (bs, V)
  | p == V = if isValidMove g ps then playGame rs h v ng (bs++[b1]) else (bs, H)
  where
    ps = if p == H then h r g else v r g
    ng@(Game b1 p1) = (playMove g ps)

-- generates infinite list of values between (0,1)
genRandomZeroOne :: Gen [Double]
genRandomZeroOne = mapM (const $ choose (0::Double,1)) [1..]

-- plays a game and prints it to the console
playAndPrint :: Int -> Strategy -> Strategy -> IO ()
playAndPrint dim sh sv = do 
  rss <- generate $ mapM (const $ genRandomZeroOne) [1..]
  let (bs, w) = play rss dim sh sv
  putStr $ (unlines $ map prettyShowBoard bs) ++ "\nWinner: " ++ show w ++ "\n"
