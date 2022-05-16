--Description

{-
This file includess main functions to make guesses and receive feedback for the battleship game,
(Explain the problem), which is implemented by guessing battleship position on a grid
(Overall approach to the problem)
(important functions to note within the code)
-}

module Proj2 (Location,
            toLocation,
            fromLocation,
            feedback,
            GameState,
            initialGuess,
            -- nextGuess
            ) where
import Data.List
import Data.Char


{-
Type Declaritation
-}
type Location = (Char, Char)
-- GameState
type GameState = ()

{-
Helper Functions
-}


countCorrect:: [Location] -> [Location] -> Int
countCorrect guesses targets = countCorrect' (sort guesses) (sort targets)
countCorrect' [] _ = 0
countCorrect' _ [] = 0
countCorrect' guesses@(x: xs) targets@(y: ys)
    | x < y = countCorrect' xs targets
    | x == y = countCorrect' xs ys + 1
    | otherwise = countCorrect' guesses ys

count_1and2::[Location] -> [Location] -> (Int,Int)
count_1and2 [] _ = (0,0)
count_1and2 (x:xs) targets =
    addTuple (toClosestDis x targets) (count_1and2 xs targets)

addTuple:: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuple (x1,x2) (y1,y2) = (x1+y1, x2+y2)

calculateDistant:: Location -> Location -> Int
calculateDistant (a,b) (c,d) = max (abs (x1 - x2)) (abs (y1 -y2))
    where   x1 = ord a
            x2 = ord c
            y1 = ord b
            y2 = ord d

toClosestDis:: Location -> [Location] -> (Int, Int)
toClosestDis oneGuess targets = case minimum ([calculateDistant oneGuess y| y <- targets]) of
        1 -> (1,0)
        2 -> (0,1)
        _ -> (0,0)


{-
Main Functions
-}

-- toLocation
-- gives Just the Location named by the string,
-- or Nothing if the string is not a valid location name.
toLocation :: String -> Maybe Location
toLocation str
    | length str /= 2             = Nothing
    | fst `notElem` "ABCDEFGH"     = Nothing
    | scd `notElem` "1234"        = Nothing
    | otherwise                   = Just (fst,scd)
    where
        fst = head str
        scd = str !! 1


-- fromLocation
-- gives back the two-character string version of the specified location;
-- for any location loc, toLocation (fromLocation loc) should return Just loc.
-- For Testing:
    -- feedback [('A','2'),('B','2'),('C','2')] [('C','2'),('D','2'),('E','2')]
fromLocation :: Location -> String
fromLocation (x1,x2) = x1 : [x2]

-- feedback
feedback :: [Location] -> [Location] -> (Int,Int,Int)
feedback guesses targets = (correct, dist_1, dist_2)
    where
        correct = countCorrect guesses targets
        (dist_1, dist_2) = count_1and2 guesses targets

--test case
-- feedback [('B','3'),('C','3'), ('H','3')] [('H','1'),('B','2'),('D','3')]
-- feedback [('H','2'),('H','1'), ('B','2')] [('H','1'),('B','2'),('D','3')]
-- feedback [('A','3'),('D','2'), ('H','1')] [('A','1'),('D','2'),('B','3')]
-- feedback [('H','4'),('G','3'), ('H','2')] [('A','1'),('D','2'),('B','3')]
-- feedback [('D','2'),('B','3'), ('A','1')] [('A','1'),('D','2'),('B','3')]
-- H1, B2, D3	B1, A2, H3	0, 2, 1
-- H1, B2, D3	B2, H2, H1	2, 1, 0
-- A1, D2, B3	A3, D2, H1	1, 1, 0s
-- A1, D2, B3	H4, G3, H2	0, 0, 0
-- A1, D2, B3	D2, B3, A1	3, 0, 0

generateAllLocations::[Maybe Location]
generateAllLocations = [toLocation (x : [y]) | x <- "ABCDEFGH", y <- "1234"]

-- initialGuess
-- initialGuess :: [Location]
initialGuess = take 3 generateAllLocations

-- nextGuess
nextGuess :: ([Location],GameState) -> (Int,Int,Int) -> ([Location],GameState)
nextGuess