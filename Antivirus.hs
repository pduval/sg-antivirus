import Astar

data Piece = Piece [(Int, Int)] String | Virus [(Int, Int)] deriving (Show,Eq,Ord)
data FixedPiece = FixedPiece (Int, Int) deriving (Show,Eq,Ord)
data GameState = GameState [FixedPiece] [Piece] deriving (Show, Eq, Ord)
data Direction = Up | LLeft | RRight | Down

dir_show :: Direction -> String
dir_show Up = "Up"
dir_show LLeft = "Left"
dir_show RRight = "Right"
dir_show Down = "Down"

coords_show :: [(Int,Int)] -> String
coords_show [] = ""
coords_show ((x,y):xs) = ("("++Prelude.show x ++ "," ++ Prelude.show y++")") ++ (coords_show xs)

piece_show :: Piece -> String
piece_show (Virus xs) = "virus:"++(coords_show xs)
piece_show (Piece xs s) = "piece:"++(s ++" "++ coords_show xs)

fp_show :: FixedPiece -> String
fp_show (FixedPiece (x,y)) = "fixed:("++ (show x) ++ "," ++ (show y)++")"

pieces_show :: [Piece] -> String
pieces_show [] = ""
pieces_show (x:xs) = (piece_show x)++"\n"++(pieces_show xs)

fpieces_show :: [FixedPiece] -> String
fpieces_show [] = ""
fpieces_show (x:xs) = (fp_show x)++"\n"++(fpieces_show xs)

game_show :: GameState -> String
game_show (GameState fps ps) = "Game:\n" ++ (fpieces_show fps) ++ "---\n" ++ pieces_show ps

move :: (Int,Int) -> Direction -> (Int, Int)
move (x,y) Up = (x, y-1)
move (x,y) Down = (x, y+1)
move (x,y) LLeft = (x-1, y)
move (x,y) RRight = (x+1, y)
  
movePiece :: Piece -> Direction -> Piece
movePiece (Piece xs name) dir = Piece [move (x,y) dir | (x,y)<-xs] name
movePiece (Virus xs) dir = Virus [move (x,y) dir | (x,y) <- xs]

posInGame :: (Int,Int) -> Bool
posInGame (x,y) 
  | y == -4 && x == 0 = True
  | y > 3 || y < -4 || x < -3 || x > 3 = False
  | y <= 0 && (x > (3+y) || x < (-3-y)) = False
  | y > 0 && (x > 3-y || x < (y-3)) = False
  | otherwise = True

positions :: Piece -> [(Int, Int)]
positions (Piece xs name) = xs
positions (Virus xs) = xs

viruspos :: Piece -> [(Int, Int)]
viruspos (Piece xs name) = []
viruspos (Virus xs) = xs

  
allpositions :: GameState -> [(Int, Int)]
allpositions (GameState fps ps) = [pos | FixedPiece pos <- fps] ++ [pos | p <- ps, pos <- positions(p)]

validPos :: [(Int, Int)] -> (Int,Int) -> Bool
validPos xs (x,y) = posInGame (x,y) && 1 == length (filter (\(x1,y1) -> x1==x && y1==y) xs)
  
validState :: GameState -> Bool
validState (GameState fps ps) = length (filter (==False) (map (validPos allpos) allpos)) == 0 where
  allpos = allpositions (GameState fps ps)

goalState :: GameState -> Bool
goalState (GameState _ ps) = any (\(x,y) -> x == 0 && y == -4) (concat (map viruspos ps))

moves :: Piece -> [(Piece, String)]
moves (Piece xs name) = [(movePiece (Piece xs name) dir, name ++ " -> " ++ (dir_show dir)) | dir <- [LLeft, RRight, Up, Down]]
moves (Virus xs) = [(movePiece (Virus xs) dir, "Virus -> " ++ (dir_show dir)) | dir <- [LLeft, RRight, Up, Down]]

pieceMoves :: [Piece] -> Int -> [([Piece], String)]
pieceMoves xs 0 = [((p1 : (tail xs)), desc) | (p1, desc) <- moves (head xs)] 
pieceMoves xs n = [((take n xs) ++ nx, desc) | (nx, desc) <- pieceMoves (drop n xs) 0]

gameMoves :: [Piece] -> [([Piece], String)]
gameMoves xs = [(ps, s) |  n <- [0..(length xs)-1], (ps,s) <- pieceMoves xs n]

antivirusNextStates :: (GameState -> Cost) -> GameState -> [(Node GameState, Cost)]
antivirusNextStates f (GameState fp ps) =   
       [(Node gs 1 (f gs) desc [], 1) | (gs, desc) <- (filter (\(g,d) -> validState g) [(GameState fp np, desc) | (np, desc) <- gameMoves ps])]

stateCost :: GameState -> Cost
stateCost x 
  | goalState x = 0
  | otherwise = 1    

show_states:: [(Node GameState, Cost)] -> String
show_states [] = ""
show_states ((Node x _ _ _ _, c):xs) = (game_show x) ++"\n"++(show_states xs)

show_node :: Node GameState -> String
show_node (Node x _ _ _ []) = game_show x
show_node (Node _ _ _ desc (x:xs)) = (show_node x) ++ "\n" ++ desc

episode17 = GameState [FixedPiece (0,0), FixedPiece (-2, -1)] [
  Piece [(0,-3), (-1, -2)] "Pink",
  Piece [(1,-2), (2, -1)] "Black",
  Piece [(0,-1), (1,-1)] "Blue",
  Virus [(-1,0), (-1,-1)]
  ]

episode1 = GameState [] [Virus [(0,-3), (0,-2)]]
episode1bis = GameState [] [Virus [(-1,0), (-1,-1)]]
episode2 = GameState [FixedPiece (0,0), FixedPiece (-2, -1)] [
  Piece [(0,-1), (1,-1)] "Blue",
  Virus [(-1,0), (-1,-1)]
  ]
episode2e = GameState [] [
  Piece [(0,-1), (1,-1)] "Blue",
  Virus [(-1,0), (-1,-1)]
  ]

solve_game :: GameState -> [Node GameState]
solve_game x = gsearchGoal x stateCost (antivirusNextStates stateCost) game_show
