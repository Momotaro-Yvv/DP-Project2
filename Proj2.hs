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
import Data.Maybe

{-
Type Declaritation
-}
type Guesses = [Location]
type Location = (Char, Char)

-- GameState,containing all the possible choices from which nextGuess can choose
type GameState = ([[Location]],Int,[Location])

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

generateAllChoices :: String -> String -> [Location]
generateAllChoices col row= [fromJust (toLocation (x : [y])) | x <-col, y <- row]

-- Every set contains a unique empty subset.
chooseThree :: (Eq t, Num t) => t -> [a] -> [[a]]
chooseThree 0 _ = [[]]
chooseThree _ [] = []
chooseThree n (x : xs) = map (x :) (chooseThree (n - 1) xs) ++ chooseThree n xs

--This function will take lists of Locations and the feedback, return a new list of pruned search space
-- pruningCorrectNum:: [Location] -> [Location] -> Int -> [Location]
pruningCorrectNum :: (Eq a, Num a) => [Location] -> [[Location]] -> a -> [[Location]]
pruningCorrectNum previouGuess originalSearchSpace correct_num =
    if correct_num == 0
        then
            filter (noDistanceOf 0 previouGuess) originalSearchSpace
    else originalSearchSpace

pruningDistOneTwo:: [Location] -> [[Location]] -> Int-> Int -> Int -> [[Location]]
pruningDistOneTwo previouGuess originalSearchSpace correct_num dist_1 dist_2 =
    if dist_1 == 0 && dist_2 == 0
            then filter (countDistanceOf [(1,0,0),(0,0,0),(2,0,0),(3,0,0)] previouGuess) originalSearchSpace
        -- else
        --     filter (countDistanceOf [(1,0,1),(1,0,2),(1,0,3),(2,0,1),(2,0,2),(2,0,3),(3,0,1),(3,0,2),(3,0,3)] previouGuess) originalSearchSpace
    else originalSearchSpace

-- pruning_dist_2:: [Location] -> [[Location]] -> Int -> Int -> [[Location]]
-- pruning_dist_2 previouGuess originalSearchSpace dist_1 dist_2 =
--     if dist_1 == 0 &&  == 0
--         then filter (noDistanceOf 2 previouGuess) originalSearchSpace
--     else originalSearchSpace
-- ifDistanceHasOne [('D','4'),('H','1'),('E','4')] ('D','3')


countDistanceOf::[(Int,Int,Int)] -> [Location] -> [Location] -> Bool
countDistanceOf dist previouGuess thisSearchSpace=
    feedback previouGuess thisSearchSpace `elem` dist

noDistanceOf:: Int -> [Location] -> [Location] -> Bool
noDistanceOf dist previouGuess thisSearchSpace=
    dist `notElem` [calculateDistant x y| x <-previouGuess, y <- thisSearchSpace]

{-
Main Functions
-}

-- toLocation
-- gives Just the Location named by the string, or Nothing if the string is not a valid location name.
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
    -- feedback [('A','2'),('B','2'),('C','2')] [('C','2'),('D','2'),('E','2')]
fromLocation :: Location -> String
fromLocation (x1,x2) = x1 : [x2]

-- feedback
feedback :: [Location] -> [Location] -> (Int,Int,Int)
feedback targets guesses = (correct, dist_1, dist_2)
    where
        correct = countCorrect targets guesses
        (dist_1, dist_2) = count_1and2 targets guesses

-- initialGuess
-- TODO: how to make smart initial guess?
initialGuess :: ([Location], GameState)
initialGuess = ([('A','1'),('A','2'),('B','1')], (initialSearchSpace, 0,[]))
    where initialSearchSpace = chooseThree 3 $generateAllChoices "ABCDEFGH" "1234"

nextGuess :: ([Location],GameState) -> (Int,Int,Int) -> ([Location],GameState)
nextGuess (previouGuess, (originalGuessCandidates,index,confirmedLocation)) (correct_num, dist_1, dist_2) =
    if previouGuess == [('A','1'),('A','2'),('B','1')]
        if(correct_num, dist_1, dist_2) == (0,0,3) then
            ([('H','4'),('H','3'),('G','4')],(initialPrunedSearchSpace,index+1,[('C','3')]))
        else (correct_num, dist_1, dist_2) == (1,1,1)
            ([('H','4'),('H','3'),('G','4')],(prunedSearchSpace,index+1,[('B','2')]))

        (nextGuess, (prunedSearchSpace,index+1,newconfirmedLocation))
    where
        prunedSearchSpace =
            pruningDistOneTwo previouGuess (
                pruningCorrectNum previouGuess (
                    filter (/=previouGuess) originalGuessCandidates)
                correct_num
                )
            correct_num dist_1 dist_2

        initialPrunedSearchSpace = filter (/=previouGuess) $chooseThree 3 $('C','3') : ('D', '4') : generateAllChoices "EFGH" "1234"
        newconfirmedLocation =
            if correct_num ==2 then
                confirmedLocation ++ previouGuess
            else
                confirmedLocation
        possibilityListSorted = map (calculatePossibility prunedSearchSpace confirmedLocation) prunedSearchSpace
        nextGuess = snd(minimum possibilityListSorted)


calculatePossibility :: Fractional a1 => [[Location]] -> [Location]-> [Location] -> (a1, [Location])
calculatePossibility guessCandidates confirmedLocation guess=
    if noDistanceOf 0 guess confirmedLocation then
        (3+ calculatePossibilityAll,guess)
    else
        (calculatePossibilityAll,guess)
        where calculatePossibilityAll = calculatePossibilityOfOneGuess[feedback target guess| target <- guessCandidates]

calculatePossibilityOfOneGuess :: (Ord a2, Fractional a1) => [a2] -> a1
calculatePossibilityOfOneGuess feedBacks = sum([fractionalDivide (length x * length x) (length feedBacks)| x <- group(sort feedBacks)])

fractionalDivide :: (Fractional a1, Integral a2, Integral a3) => a2 -> a3 -> a1
fractionalDivide a b = fromIntegral a / fromIntegral b

