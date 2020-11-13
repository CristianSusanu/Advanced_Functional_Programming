module Assignment3 where

import Data.List
import Data.Tree

import Types
import DomViz  -- comment out as a last resort if you are unable to install diagrams

-- given a cell c and a player p, compute the adjacent cell c'
-- that is also occupied if p plays a domino at c
adjCell :: Cell -> Player -> Cell
adjCell (x,y) H = (x+1,y)
adjCell (x,y) V = (x,y+1)

-- compute the opponent of a player
opp :: Player -> Player
opp H = V
opp V = H

-- determine whether a move is valid in a given board
valid :: Board -> Cell -> Bool
valid b c = c `elem` free b && adjCell c (turn b) `elem` free b

-- create an empty board from an arbitrary list of cells
empty :: [Cell] -> Board
empty cs = Board { turn = H, free = cs, hist = [] }

-- create a rectangular board of arbitrary dimensions
board :: Int -> Int -> Board
board maxx maxy = empty [(x,y) | x <- [1..maxx], y <- [1..maxy]]

-- create a crosshatch-shaped square board of arbitrary dimension
hatch :: Int -> Board
hatch n = empty [(x,y) | x <- [1..2*n+1], y <- [1..2*n+1], odd y || x == 1 || x == (2*n+1) || odd x]

-- some example Domineering games
board4x4_3 = Board { turn = H,
                     free = [(1,1),(1,2),(2,2),(2,3),(2,4),(3,2),(3,3),(3,4),(4,1),(4,2),(4,3),(4,4)],
                     hist = [(1,3),(2,1)] }

alphaDom_vs_LeeSedom =
  Board { turn = V,
          free = [(-4,1),(-4,3),(-2,0),(-2,4),(2,1),(2,4),(3,-4),(3,4),(4,-2),(4,0)],
          hist = [(0,4),(4,1),(0,-4),(-4,-3),(-1,-2),(2,-1),(-2,-4),(-4,-1),(-1,2),(4,3),(1,2),(-2,2),(-4,-4),(-2,-2),(2,-2),(4,-4),(-3,1),(2,-4),(-4,4),(-1,3),(-4,2),(-3,-2),(3,-1),(1,-3),(-2,-3),(3,1),(1,3)] }

alphaDom_vs_RanDom =
  Board { turn = V,
          free = [(-4,-3),(-4,0),(-2,-4),(-2,-2),(-1,-4),(-1,-2),(-1,2),(-1,4),(0,-4),(0,-2),(0,2),(0,4),(1,-4),(1,-2),(1,2),(1,4),(2,-4),(2,-2),(2,4),(3,-4),(4,0),(4,3)],
          hist = [(-3,4),(2,-1),(-3,2),(4,-2),(-4,-4),(-4,3),(3,4),(2,1),(-3,1),(3,1),(-4,-1),(-2,-1),(-2,3),(-4,1),(1,3),(4,-4),(-4,-2),(4,1),(1,-3),(3,-2),(-2,-3)] }

-- Question 1

legalMoves :: Player -> Board -> [Cell]
legalMoves p (Board t [] []) = []
legalMoves p (Board t [] (h:hs)) =  []
legalMoves p (Board t (f:fs) []) = if((adjCell f p)`elem`fs) then [f]++(legalMoves p (Board t fs [])) else (legalMoves p (Board t fs []))
legalMoves p (Board t (f:fs) (h:hs)) = if((adjCell f p)`elem`fs) then [f]++(legalMoves p (Board t fs (h:hs))) else (legalMoves p (Board t fs (h:hs)))

moveLegal :: Board -> Cell -> Board
moveLegal (Board t fs hs) c = if((valid (Board t fs hs) c)) then (Board (opp t) (delete (adjCell c t) (delete c fs)) (c:hs)) else (Board t fs hs)

replay :: Board -> [Board]
replay (Board t fs []) = [(Board t fs [])]
replay (Board t fs hs) = replay(Board (opp t) (fs ++ [(head hs)] ++ [(adjCell (head hs) (opp t))]) (tail hs)) ++ [(Board t fs hs)]

-- Question 2

gametree :: Board -> Tree Board
gametree b = Node b [gametree (moveLegal b c) | c <- legalMoves (turn b) b]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _)  = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

score :: Board -> Score
score (Board t fs hs) = if(length(legalMoves t (Board t fs hs)) == 0) then (Win (opp t)) else (Heu (formula))
                        where
                           formula = if(t==V) then(length (legalMoves V (Board t fs hs)) - length (legalMoves H (Board t fs hs)) - 1) else (length (legalMoves V (Board t fs hs)) - length (legalMoves H (Board t fs hs)) +1)

minimax :: (Board -> Score) -> Tree Board -> Tree (Board, Score)
minimax _ (Node b []) = Node (b, score b) []
minimax s (Node b fs) = if((turn b) == H) then (Node (b, minimum zs) ts) else if((turn b) == V) then (Node (b, maximum zs) ts) else (fail "not in scope")
                        where
                           ts = map (minimax s) fs
                           zs = [z | (Node (_ ,z) _) <- ts]

bestmoves :: Int -> (Board -> Score) -> Board -> [Cell]
bestmoves d s b = [head (hist(x)) | (Node (x, y) _) <- ts, y == best]
                    where
                       tree = (prune d (gametree b))
                       Node(_, best) ts = (minimax s tree)

chooseSafe :: PickingMonad m => [a] -> m (Maybe a)
chooseSafe [] = return Nothing
chooseSafe xs = do
  i <- pick 0 (length xs - 1)
  return (Just (xs !! i))

randomBestPlay :: PickingMonad m => Int -> (Board -> Score) -> Board -> m (Maybe Cell)
randomBestPlay d sfn = chooseSafe . bestmoves d sfn
randomPlay :: PickingMonad m => Board -> m (Maybe Cell)
randomPlay b = chooseSafe (legalMoves (turn b) b)
