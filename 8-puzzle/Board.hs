module Board where

import Data.List(sort)

--Board Tiles
type Tile = Int
type X = Int
type Y = Int
type Position = (X, Y)

type Board = [[Tile]]

--Confirm the board structure is a sizeXsize list.
validBoardStructure :: Int -> Board -> Bool
validBoardStructure size = all ((== size) . length)
--Confirm the values of the board are valid
validBoardValues :: Int -> Board -> Bool
validBoardValues size board = ([0..size^2-1] == (sort $ concat board))
--Return weather or not a given board is valid
validBoard :: Int -> Board -> Bool
validBoard size board = validBoardStructure size board && validBoardValues size board

-- Return weather or not a given position is out of bounds
validPosition :: Int -> Position -> Bool
validPosition size (x, y) = x >= 0 && x < size && y >= 0 && y < size

-- Get list of positions from given [left, right, up, down]
possiblePositions :: Position -> [Position]
possiblePositions (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

-- Get list of all Valid moves of a given tile
validPositions :: Int -> Position -> [Position]
validPositions size p = filter (validPosition size) (possiblePositions p)

-- Given a board, return a list of all possible boards
possibleBoards :: Board -> [Board]
possibleBoards board = map (\x -> flipTiles x blankPos board) (validMov board)
    where blankPos = findBlank board
	      validMov = validPositions (length board) (findBlank board)

-- Flip tiles at two given positions
flipTiles :: Position -> Position -> Board -> Board
flipTiles p1 p2 board = changeBoard val2 p1 (changeBoard val1 p2 board)
    where val1 = getTile p1 board
          val2 = getTile p2 board
-- Return a Board with value at position changed
changeBoard :: Tile -> Position -> Board -> Board
changeBoard _ _ [] = []
changeBoard val (px, 0) (x:xs) = makeRow val px x : changeBoard val (px, -1) xs
changeBoard val (px, py) (x:xs) = x : changeBoard val (px, py-1) xs
-- Return Row with value at position changed
makeRow :: Tile -> X -> [Int] -> [Int]
makeRow _ _ [] = []
makeRow val 0 (_:xs) = val : xs
makeRow val px (x:xs) = x : makeRow val (px-1) xs

-- Given the position of a Tile, Return in
getTile :: Position -> Board -> Tile
getTile (px, py) board = board!!py!!px

-- Find the "blank" tile (board must be valid)
findBlank :: Board -> Position
findBlank board = findTile board 0
findTile :: Board -> Tile -> Position
findTile board tile = head [(x,y) | x <- [0..size-1], y <- [0..size-1], getTile (x, y) board == tile]
    where size = length board

--isGoal :: Board -> Board -> Bool
--isGoal goal board = goal == board

--Print the Board to the terminal
printBoard board = putStrLn $ boardString board

boardString :: Board -> String
boardString [] = ""
boardString (x:xs) = '[' : boardRowString x ++ "]\n" ++ boardString xs

boardRowString :: [Tile] -> String
boardRowString [] = " "
boardRowString (0:xs) = " " ++ "X" ++ boardRowString xs
boardRowString (x:xs) = " " ++ show x ++ boardRowString xs

