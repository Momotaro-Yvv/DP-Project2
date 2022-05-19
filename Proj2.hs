-- COMP90048 Project2
--Author: Yuwen Tao <yuwtao@student.unimelb.edu.au>
--Student ID: 1183577

{-
This file includess main functions of implementing a two-player guessing game.
The game is played on a 4×8 grid, where the searcher need to guess the locations
of 3 battleships hidden by the hider.
After each guess, the hider responds with three numbers,
which the searcher can use in next rounds.
The game will end till all 3 battleships been found.


There are two main functions used to implement this game:
1. the "feedback" takes searcher's guess and hider's true location,
    and return feedback of three number in a tuple

    The main approach of this function is simply
    calculating the distance of each guess and the target

2. the "nextGuess" takes the Gamestate from the initialGuess,
    and feedback from hider, and return the next guess, and new Gamestate

    The main approach of this function is
    1) selecting guesses with lowest expected number of remaining candidates
    and 2) by pruning impossible candidate search space.
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
import Data.Maybe

{-
Type Declaritation
-}
type Guesses = [Location]
type Location = (Char, Char)
-- GameState,containing all the possible choices from which nextGuess can choose
type GameState = ([[Location]],[Location])
-- the list of all possible Locations
type Choices = [Location]

{-
Helper Functions
-}

-- This helper function takes two lists of Locations,
--  and count how many (out of three) does the seacher guesses right
countCorrect:: [Location] -> [Location] -> Int
countCorrect targets guesses = countCorrect' (sort targets) (sort guesses)
countCorrect' [] _ = 0
countCorrect' _ [] = 0
countCorrect' targets@(y: ys) guesses@(x: xs)
    | x < y = countCorrect' targets xs
    | x == y = countCorrect' ys xs + 1
    | otherwise = countCorrect' ys guesses

--This function calcuate and return a tuple:
--(the total number of guesses 1 space away, guesses 2 space away)
count_1and2::[Location] -> [Location] -> (Int,Int)
count_1and2 _ [] = (0,0)
count_1and2 targets (x:xs)=
    addTuple (toClosestDis targets x) (count_1and2 targets xs)

-- This is a helper funtcion that add numbers of two tuple
--  and output a new tuple
addTuple:: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuple (x1,x2) (y1,y2) = (x1+y1, x2+y2)

-- This function calculate and return the distance of two Locations
calculateDist:: Location -> Location -> Int
calculateDist (a,b) (c,d) = max (abs (x1 - x2)) (abs (y1 -y2))
    where
        x1 = ord a
        x2 = ord c
        y1 = ord b
        y2 = ord d

-- This helper function takes the distance of two Locations,
--    and tranfer into tuple format
toClosestDis:: [Location] -> Location -> (Int, Int)
toClosestDis targets oneGuess =
    case minimum ([calculateDist oneGuess y| y <- targets]) of
        1 -> (1,0)
        2 -> (0,1)
        _ -> (0,0)

-- This helper function simple iterate all possible Location on the 8*4 grid
generateAllChoices :: String -> String -> [Location]
generateAllChoices col row =
    [fromJust (toLocation (x : [y])) | x <-col, y <- row]

-- This helper function choose every possible set for a given Locations
--     and return them in a list
chooseThree :: (Eq t, Num t) => t -> [a] -> [[a]]
chooseThree 0 _ = [[]]
chooseThree _ [] = []
chooseThree n (x : xs) = map (x :) (chooseThree (n - 1) xs) ++ chooseThree n xs

--This function will take lists of Locations and the feedback
--  and the number of correct guess,
--  if correct_num = 0, delete all sets that contain the previous 3 Locations,
--  return a new list of pruned search space
pruneCorrectNum :: (Eq a, Num a) => [Location] -> [[Location]] -> a -> [[Location]]
pruneCorrectNum previouGuess originalSearchSpace correct_num =
    if correct_num == 0
        then
            filter (noDistanceOf 0 previouGuess) originalSearchSpace
    else originalSearchSpace

-- Helper function for pruningCorrectNum
-- Given two list of Locations,
-- return true if none of them are overlap, and false otherwise
noDistanceOf:: Int -> [Location] -> [Location] -> Bool
noDistanceOf dist previouGuess thisSearchSpace=
    dist `notElem` [calculateDist x y| x <-previouGuess, y <- thisSearchSpace]

-- It will take lists of Locations and feedback, numbers of the feedback
--  if dist_1&dist_2 = 0, delete all sets within 2 spaces
--  from the previous 3 Locations,
--  if dist_1 =0, delete all the sets 1 space from the previous 3 Locations,
--  and return a new list of pruned search space
pruneDistOneTwo:: [Location] -> [[Location]] -> Int-> Int -> Int -> [[Location]]
pruneDistOneTwo previouGuess searchSpace correct_num dist_1 dist_2 =
    if dist_1 == 0 then
        if dist_2 == 0
            then filter (countDistanceOf [(0,0)] previouGuess) searchSpace
        else
            filter (countDistanceOf [(0,1),(0,2),(0,3)] previouGuess) searchSpace
    else searchSpace

-- Helper function for pruningDistOneTwo
--  Given two lists of Locations,
--  return true if the number of guesses 1 and 2 space away
--  from a true target is the subset of dist, otherwise false
countDistanceOf::[(Int,Int)] -> [Location] -> [Location] -> Bool
countDistanceOf dist previouGuess thisSearchSpace=
    count_1and2 previouGuess thisSearchSpace `elem` dist

-- Helper function that divide two interger and keep the digits
fractionalDivide :: (Fractional a1, Integral a2, Integral a3) => a2 -> a3 -> a1
fractionalDivide a b = fromIntegral a / fromIntegral b

{-
Main Functions
-}

--Input: a string of 2 length
--Output:
    -- if valid, gives Just the Location named by the string,
    -- is not a valid location name, Nothing
toLocation :: String -> Maybe Location
toLocation str
    | length str /= 2             = Nothing
    | fst `notElem` "ABCDEFGH"    = Nothing
    | scd `notElem` "1234"        = Nothing
    | otherwise                   = Just (fst, scd)
    where
        fst = head str
        scd = str !! 1

-- Input: Location
-- Output: two-character string of the specified location
fromLocation :: Location -> String
fromLocation (x1, x2) = x1 : [x2]

-- Input: two Location respectively the searcher's guess and the ture targets
-- Output the tuple:
    -- (correct locations num, num of ship 1space away, num of 2spaces away)
feedback :: [Location] -> [Location] -> (Int, Int, Int)
feedback targets guesses = (correct, dist_1, dist_2)
    where
        correct = countCorrect targets guesses
        (dist_1, dist_2) = count_1and2 targets guesses

-- No input arguments
-- Output:
    -- initial guess (Location)
    -- GameState: (the initial guess candicates, a potential list of targets)
initialGuess :: ([Location], GameState)
initialGuess = ([('A','1'),('A','2'),('B','1')], (initialSearchSpace, []))
    where
        initialSearchSpace = chooseThree 3 $generateAllChoices "ABCDEFGH" "1234"

-- Input: (guess of Location , GameState) from previous/initial round, feedback
-- Output: new guess and updated GameState
nextGuess :: ([Location], GameState) -> (Int, Int, Int) -> ([Location], GameState)
nextGuess (previouGuess, (candidates,promisingLoc)) (correct_num, dist_1, dist_2) =
    if dist_2 == 3 && previouGuess == [('A','1'),('A','2'),('B','1')]
        --when from initial guess
        then ([('H','4'),('H','3'),('G','4')],(initialPrune,[('C','3')]))
    else
        --when from previous guessing
        (nextGuess, (prunedCandicts,newPromisingLoc))
    where
        prunedCandicts =
            --if dis_1&dist_2 = 0,
            -- prune all Locations around the previous location
            pruneDistOneTwo previouGuess (
                -- if correct_num =0 meaning none of the previous location is correct
                pruneCorrectNum previouGuess (
                    --delete the previous Location from candicate list
                    filter (/=previouGuess) candidates)
                correct_num
                )
            correct_num dist_1 dist_2

        initialPrune = filter (/=previouGuess)
            $chooseThree 3 $('C','3') : ('D', '4') : generateAllChoices "EFGH" "1234"

        --update the potential list of targets
        -- if previous round guess 2 or more locations correctly
        newPromisingLoc =
            if correct_num ==2 then
                promisingLoc ++ previouGuess
            else
                promisingLoc

        --for each guess candidate, calculate its expected number
        possibilityList = map (calcuPossibility prunedCandicts newPromisingLoc) prunedCandicts
        --select the one with minimum expected number as next round guess
        nextGuess = snd(minimum possibilityList)

-- This function calculate expected number of each set of guess for "nextGuess",
-- Input:
    -- 1)a list of guess candicate of a guess,
    -- 2)list of high possibility Locations,
    -- 3)the guess whose value be calculated
-- Output:
    -- a tuple (the expected number, one guess)
calcuPossibility :: Fractional a1 => [[Location]] -> [Location]-> [Location] -> (a1, [Location])
calcuPossibility candidates promisingLocation guess=
    if noDistanceOf 0 guess promisingLocation then
        (3+ calculatePossibilityAll,guess)
    else
        (calculatePossibilityAll,guess)
        where
            calculatePossibilityAll =
                possibilityOfOneGuess[feedback target guess| target <- candidates]

-- This function is a helper function for "calculatePossibility",
-- Input: a list of all feedbacks of one guess and each of its possible targets
-- Output:  ∑ count(f)/2T
possibilityOfOneGuess :: (Ord a2, Fractional a1) => [a2] -> a1
possibilityOfOneGuess feedBacks =
    sum([fractionalDivide (length x * length x) (length feedBacks)| x <- group(sort feedBacks)])

