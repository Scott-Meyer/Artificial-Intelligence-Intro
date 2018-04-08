# Solving the 8-Puzzle

## Introduction
The goal of this assignment is to solve a relatively simple puzzle with a few different kinds of graph search, providing both experience in solving problems this way as well as information on the efficiency and usability of the different methods.

## Problem Definition

```json
{"n" : 3,
 "start" : [[7,2,4],
            [5,0,6],
		[8,3,1]],
 "goal" : [[0,1,2],
           [3,4,5],
	     [6,7,8]]}
```

Given a JSON file containing the initial board state and the goal state, find the path to the goal state while following the rules of the puzzle.
It is also important to note, that in an 8puzzle, you are only allowed to move tiles into the blank slot, and you are only allowed do that vertically or horizontally.

## Discussion On JSON and DATA
Solving 8-puzzles from JSON with graphs introduces a few distinct problems.
The first, was deciding what programming language to use. After some internal debate, I landed on the Haskell programming language. I thought the functional style would match, and I just really wanted some experience using it. After I landed on Haskell, I still had to extract the puzzle from the provided JSON files, and turn them into usable Haskell structures. To start, I made a few new Haskell types.

```Haskell
type Tile = Int
type X = Int
type Y = Int
type Position = (X, Y)
type Board = [[Tile]]
data Game = Game { n :: Int
                 , state :: Board
                 , goal :: Board
                 }
```

The second, is converting from JSON to Haskell data. For this I used the Haskell package Aeson available from either cabal or stack. Using this package, I just need to create an instance of FromJSON for my game type and I will get a return that is either a error string or my type.

```Haskell
import Data.Aeson
instance FromJSON Game where
    parseJSON = withObject "game" $ \o -> do
        n <- o .: "n"
        state <- o .: "start"
        goal <- o .: "goal"
        return (Game n state goal)

--load json file
getJSON :: (String -> IO B.ByteString)
getJSON = B.readFile
--Attempt to parse a JSON file into a Game object
jsonToGame :: String -> IO (Either String Game)
jsonToGame file = (eitherDecode <$> getJSON file) :: IO(Either String Game)
```

Third, Now that I the ability to read a JSON file into a Haskell game with error handling included, there one thing missing before the return from jsonToGame can be used. Only errors for reading JSON and making a Game are caught, the Game made from JSON still must be verified. 

```Haskell
--Validate a game (is nXn && contins only [1,2..])
validGame :: Game -> Bool
validGame (Game size start goal) = validBoardStructure size start
                                && validBoardValues size start
                                && validBoardStructure size goal
                                && validBoardValues size goal
		  
--Confirm the board structure is a sizeXsize list.
validBoardStructure :: Int -> Board -> Bool
validBoardStructure size = all ((== size) . length)
--Confirm the values of the board are valid
validBoardValues :: Int -> Board -> Bool
validBoardValues size board = ([0..size^2-1] == (sort $ concat board))
```
Now that the data from the JSON is in Haskell and validated, we can begin to solve the puzzle. But, before implementation…

## Results
One of the first things I noticed once I graphed the results, was how utterly unreliable depth first search is. Not where there occasions when it was vastly out performed by all three of the other search types, but it never did significantly better than them! Even worse is something this graph doesn’t show. While all 3 other types of search always found a solution of the proper depth, backtrack1 found and average solution depth of 49, which is bad when you consider that it had an artificial bound of 50.

![DATA](https://i.imgur.com/PQRJWUP.png)

One thing that really surprised me, was how good iterative deepening (run on backtrack1) worked out. At first glace it seems no b etter than basic breadth first graph search, but there is something else going on. Aside from the fact that iterative deepening uses far less RAM, there is processing speed. Although graph search and iterative deepening had to make a very similar amount of boards, iterative deepening ran about twice as fast. Iterative deepening was also about twice as easy to code. All in all I think iterative deepening is a surprisingly good option when you don’t know much about the problem, it can even be sped up with memorizing.
Lastly, we have the single run of a 15-puzzle. Notice that the depth first search is missing, this is because to account for how many nodes where made, all my functions had to be changed from returning “Maybe Node” to “(Maybe Node, Int)”, this along with both the Lazy and Recursive nature of Haskell, this caused my stack to include millions of chains of function runs instead of actual data. With this, the backtrack1 on 15-puzzle went from taking 4mb of ram to 60+gb and errored out. I am sure I could correct this with a Writer Monad and breaking purity, but it didn’t really seem worth the effort.

![TABLE](https://i.imgur.com/fxUcrE2.png)

## Implementation
Once the JSON files where read, converted, and validated, all that’s left is to solve the puzzle. One of the neatest tricks to realize when you go about creating a plan to solve an 8-puzzle (any n^3-1 puzzle) is that instead of thinking in terms of moving a tile into an empty slot, you can think of swapping the empty spot with any of its 2-4 adjacent tiles. Utilizing that, and the fact that any tile must have a position of 0 to size-1 gives a quick and dirty way to find all possible direct successors to any given game.
```Haskell
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
```

Creating the graph can then be done by just using possibleBoards Board to get a list of all child nodes. 
Skipping the implementation of the, rather trivial, Backtrack1 search, one of the more interesting aspects of Iterative Deepening is how easy it is to make if you already have a depth first search.All that had to be done was to modify the Backtrack1 to take a depth, so that you can recursively iterate over it.
```Haskell
data Node = NullNode 
          | Node { game :: Game
                 , parent :: Node
                 , cost :: Int
                 }
--Iterate depth (recursive), return final
iterdeep' :: Int -> Game -> Maybe Node
iterdeep' b g
    | b > bound         = Nothing
    | node==Nothing     = iterdeep' (b+1) g
    | otherwise         = node
    where
        firstNode = (Node g NullNode 0)
        node = backtrack1' b [firstNode]
```

The more interesting Implementation was the A* and Graph Search, because they can be implemented as a single unit. All I had to do was create a function that gave me a distance  in the A* way, create a separate function that gave me a distance in terms of depth in tree, make my graph search take a function to calculate depth/distance and BAM I can just call it both ways to do both searches
```Haskell
--For A* search
astar :: Node -> Game -> Int
astar (Node pg _ x) game = trav + 1 + hur
    where trav = if x==0 then 0 else x - pcost
          hur  = huristic game
          pcost = huristic pg

huristic :: Game -> Int
huristic game = sum $ map (tileDist game) [1..size]
    where size = n game * (n game) - 1

tileDist :: Game -> Tile -> Int
tileDist (Game _ state goal) x=posDiff (findTile state x) (findTile goal x)

posDiff :: Position -> Position -> Int
posDiff (x1, y1) (x2, y2) = (abs $ x1 - x2) + (abs $ y1 - y2)

--For breadth First Search
distance :: Node -> Game -> Int
distance (Node _ _ x) _ = x + 1
```

After that the actual implementation of the graph search was rather easy, because I used a built in minimum first heap. All I had to do was always try the first thing on the heap and see if it was the final node. If it wasn’t I just created its children, added them to the heap, and called again. If it was, I could just return that and have a simple code to follow its parents up to the head of the tree. This works with A* where the minimum “cost” of the node should be the smallest huristic+depth, and for graph search where the smallest “cost” is just the lowest depth, so each depth layer is completed before another starts.


## Conclusion
It was interesting to use these different searches, but its seems obvious that only iterative deepening and A* are of any use. If you can make any kind of heuristic at all, A* is so ridiculously amazing that it seems to be the only option. If you can’t, then iterative deepening is the only viable choice. Breadth first graph search takes an insane amount of storage, and depth first just gives you a random probably horrible answer, and it may even be SLOWER!