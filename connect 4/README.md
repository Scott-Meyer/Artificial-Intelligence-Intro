# Game Playing, Connect 4

## Introduction
The goal of this assignment is to learn how to play a game using a type of min-max algorithm.

## Problem Definition
Create a program, that will accept a JSON payload of the state of a connect 4 game and return a move as a JSON payload. The input/output of JSON will be handled in the standard in and standard out (IE: Terminal write/read). 

The format of the JSON payload is shown on the right, the grid is given in Colum major. The “player” is which player you are.

The program will be started as a sub-process by a racket player manager which will also start another program as a sub-process to play against you.

```json
//Given
{"grid":[[0,0,0,0,0,0]
        ,[0,0,0,2,2,2]
        ,[1,2,2,1,2,1]
        ,[0,0,0,0,2,1]
        ,[0,0,0,0,1,1]
        ,[0,0,0,1,2,2]
        ,[0,0,0,0,1,1]]
,"height":6
,"player":1
,"width":7}

//Return
{"move":4}
```
## Program Design

My design for this program consists of a few basic parts

In/out, create a function called play that gets JSON from stdin, converts the JSON to a Connect4 object, passes the Connect4 object to a function to get a Move object back, convert the Move object to a JSON payload, output the JSON to stdout, recursively call self for next move.

getMove, the goal of this function is to take a Connect4 object, and return a Move object. This is done simply by calling “minMax depth Connect4”

After that the psudo code is almost straight from Wikipedia (https://en.wikipedia.org/wiki/Negamax) other than a driver function that allows me to get a move back instead of a value. The driver function also allows from some easy multithreading because the few negamax calls could all be done in their own threads.

```
01 miniMax depth Connect4
02     succMoves = validMoves Connect4
03     succRatings = map negamax(x, depth-1, -9999, 9999, -1) (toNode succMoves)
04     return head filter (first x==max succRatings) zip succMoves succRatings
01 function negamax(node, depth, α, β, color)
02     if depth = 0 or node is a terminal node
03         return color * (boardRate (curBoard node))
04     childNodes := validMoves(node)
05     bestValue := −9999
06     foreach child in childNodes
07         v := −negamax(child, depth − 1, −β, −α, −color)
08         bestValue := max( bestValue, v )
09         α := max( α, v )
10         if α ≥ β
11             break
12     return bestValue
```
## Implementation

Because I chose to create my connect 4 player with the Haskell programming language, the implementation can be separated into two main parts. The first part is the unsafe input/output and the pure find next move.

## Input & Output
The IO part of this program consists of 3 functions.

**main**, the main function simply outputs a string to the stderr file to let you know the program was started, then runs play.

**play**, the play program is the main driver. It starts by getting a line from stdin (terminal) and storing it as “js”, and outputting that line to stderr. Lastly, it attempts to convert the line from JSON to a Connect4 object, if it fails it outputs the error, if it succeeds it outputs the move (from the getMove function)  and calls itself again. End of the program is handled by the Connect 4 driver.

**putOut**, simplifies the operation of printing to both stdout and stderr. This simplifies debugging.

```Haskell
main :: IO ()
main = hPutStrLn stderr "Connect Four" >> play

play :: IO ()
play = do
    js <- getLine
    hPutStrLn stderr js
    case (parseJ $ B.pack js) of
        Left err -> hPutStrLn stderr err
        Right mygame -> do 
            putOut (B.unpack (encode (getMove mygame)))
            play

putOut :: String -> IO ()
putOut str = do
    hPutStrLn stderr str
    putStrLn str
    hFlush stdout
```

## Finding the next move (getMove)

This is the meat of the program. The goal is to write a function (getMove :: Connect4 -> Move)

```Haskell
getMove :: Connect4 -> Move
getMove c4 = if (player c4 `notElem` (concat $ grid c4))
                then (Move (width c4 `div` 2) (player c4))
                else minMax 6 c4
```

This is a quality of life, to play in the middle for the first move to make the first move instant. After that simply call the minMax function with a pre-set ply of 6.

```Haskell
minMax :: Int -> Connect4 -> Move
minMax depth c4 = (snd (ratemove !! (length ratemove `div` 2)))
    where
        maxRate = minimum sucRatings
        ratemove = filter (\x -> fst x == maxRate) (zip sucRatings sucMoves)
        sucRatings = map (nega.(toNode NullNode)) sucMoves
        sucMoves = validMoves c4
        nega = negamax c4 (depth - 1) (-9999) 9999 (-1)

negamax :: Connect4 -> Int -> Int -> Int -> Int -> Node -> Int
negamax c4 depth alpha beta color node
    | depth == 0 = color * boardRating
    | sucMoves == [] = (color * boardRating)
    | otherwise  = bestValue
    where
      boardRating = (boardRate (curBoard c4 node))
      bestValue = bestVal (-9999 : v) (-9999) alpha beta
      v = map (negate.nega.(toNode node)) sucMoves
      nega = negamax c4 (depth - 1) (-beta) (-alpha) (-color)
      sucMoves = if (color == 1)
                    then validMoves c4
                    else validMoves (otherPlayer c4)
```

And a “boardRate” which is a huristic, which simply returns how many pieces someone has in a row.

```Haskell
-- Rate a board
boardRate :: Connect4 -> Int
boardRate (Connect4 g _ _ p)
    | friendly == 4 = 9999
    | enemy == 4    = -9999
    | otherwise     = friendly
    where
        friendly = maximum (map (inrow p) rg)
        enemy = maximum (map (inrow ep) rg)
        rg = rotated g
        ep = if (p == 1) then 2 else 1
rotated g = [g
            , rowM g
            , filter size (diagonals (rowM g))
            , filter size (diagonals (reverse (rowM g)))]
    where size = (\ x -> length x > 3)
inrow :: Player -> Grid -> Int
inrow p m = maximum (map maximum (filter (/= []) (map ((map length).(filter (elem p)).group) m)))

```

## Results

My results are disipointing. It seems my simple huristic doesn’t find win conditions properly.

**VS Random:**

![GAME](https://i.imgur.com/j7RRdfU.png)
![GAME](https://i.imgur.com/WQthIJO.png)

**VS Self:**

![GAME](https://i.imgur.com/rxRVDUF.png)
## Conclusion

One of the hardest things about minimax is wrapping your head around what the plys actually means/does to the program. I did a lot of thinking and work on more complex heuristics. While some of them did let me play a little better, because of how often the heuristic function is ran, more complex heuristics forced me to turn down the number of plys which sometimes resulted in the program doing worse.

I had a really had time figuring out why my program would make specific moves based on my really stupid heuristic, and I would every time eventually realize it was because it saw something 4-7 moves away. It was very hard for my brain to internalize thinking of the state of millions of boards and picking the move that leads to the best board.

Also, minimax is confusing, and so is Haskell.
