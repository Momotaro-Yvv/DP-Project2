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
            nextGuess
            ) where

import Data.List
import Data.Char
import Data.Array


{-
Type Declaritation
-}
type Guesses = [Location]
type Location = (Char, Char)

-- GameState,containing all the possible choices from which nextGuess can choose
type GameState = Int

-- the list of all possible Locations
type Choices = [Location]

{-
Helper Functions
-}


countCorrect:: [Location] -> [Location] -> Int
countCorrect targets guesses = countCorrect' (sort targets) (sort guesses)
countCorrect' [] _ = 0
countCorrect' _ [] = 0
countCorrect' targets@(y: ys) guesses@(x: xs)
    | x < y = countCorrect' targets xs
    | x == y = countCorrect' ys xs + 1
    | otherwise = countCorrect' ys guesses

count_1and2::[Location] -> [Location] -> (Int,Int)
count_1and2 _ [] = (0,0)
count_1and2 targets (x:xs)=
    addTuple (toClosestDis targets x) (count_1and2 targets xs)

addTuple:: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuple (x1,x2) (y1,y2) = (x1+y1, x2+y2)

calculateDistant:: Location -> Location -> Int
calculateDistant (a,b) (c,d) = max (abs (x1 - x2)) (abs (y1 -y2))
    where   x1 = ord a
            x2 = ord c
            y1 = ord b
            y2 = ord d

toClosestDis:: [Location] -> Location -> (Int, Int)
toClosestDis targets oneGuess = case minimum ([calculateDistant oneGuess y| y <- targets]) of
        1 -> (1,0)
        2 -> (0,1)
        _ -> (0,0)


generateAllChoices :: [Maybe Location]
generateAllChoices = [toLocation (x : [y]) | x <- "ABCDEFGH", y <- "1234"]

-- Every set contains a unique empty subset.
subsets :: (Eq t, Num t) => t -> [a] -> [[a]]
subsets 0 _ = [[]]
subsets _ [] = []
subsets n (x : xs) = map (x :) (subsets (n - 1) xs) ++ subsets n xs

--generate all possible choices of the guess
choices:: [[Maybe Location]]
choices = subsets 3 generateAllChoices

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
feedback targets guesses = (correct, dist_1, dist_2)
    where
        correct = countCorrect targets guesses
        (dist_1, dist_2) = count_1and2 targets guesses

--test case
-- feedback [('H','1'),('B','2'),('D','3')] [('B','3'),('C','3'), ('H','3')]
-- feedback [('H','1'),('B','2'),('D','3')] [('H','2'),('H','1'), ('B','2')]
-- feedback [('A','1'),('D','2'),('B','3')] [('A','3'),('D','2'), ('H','1')]
-- feedback [('A','1'),('D','2'),('B','3')] [('H','4'),('G','3'), ('H','2')]
-- feedback [('A','1'),('D','2'),('B','3')] [('D','2'),('B','3'), ('A','1')]
-- feedback [('A','2'),('A','1'), ('G','2')] [('H','1'),('G','2'),('H','4')]
-- H1, B2, D3	B1, A2, H3	0, 2, 1
-- H1, B2, D3	B2, H2, H1	2, 1, 0
-- A1, D2, B3	A3, D2, H1	1, 1, 0
-- A1, D2, B3	H4, G3, H2	0, 0, 0
-- A1, D2, B3	D2, B3, A1	3, 0, 0

-- initialGuess
-- TODO: how to make smart initial guess?
-- initialGuess :: [Maybe Location, GameState]
initialGuess :: ([Maybe Location], GameState)
initialGuess = (choices!! 3, 1)

-- nextGuess
-- nextGuess :: [Location,GameState] -> (Int,Int,Int) -> [Location,GameState]
-- nextGuess (previouGuess, choices) (feedback previouGuess targets) = (previouGuess, choices) (feedback previouGuess targets) 1
--     where newGuess =

-- nextGuess :: [Maybe Location,GameState] -> (Int,Int,Int) -> [Maybe Location,GameState]
-- nextGuess :: [Int] -> (a, b, c) -> [[Maybe Location]]
-- nextGuess :: [a1] -> (a2, a3, a4) -> [[Maybe Location]]
nextGuess (previouGuess,index) [a, b, c]  = (choices !! index, index+1)
