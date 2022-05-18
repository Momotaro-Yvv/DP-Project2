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
type GameState = ([Location],Int)

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
pruningCorrectNum previouGuess originalSearchSpace correct_num =
    if correct_num == 0
        then
            filter (notElemOf previouGuess) originalSearchSpace
    else originalSearchSpace

pruning_dist_1:: [Location] -> [Location] -> Int -> [Location]
pruning_dist_1 previouGuess originalSearchSpace dist_1 =
    if dist_1 == 0
        then filter (noDistanceOf 1 previouGuess) originalSearchSpace
    else originalSearchSpace

pruning_dist_2:: [Location] -> [Location] -> Int -> Int -> [Location]
pruning_dist_2 previouGuess originalSearchSpace dist_1 dist_2 =
    if dist_1 == 0 && dist_2 == 0
        then filter (noDistanceOf 2 previouGuess) originalSearchSpace
    else originalSearchSpace

-- ifDistanceHasOne [('D','4'),('H','1'),('E','4')] ('D','3')
-- pruning [('D','4'),('H','1'),('E','4')] [('D','3'),('D','2')] (1, 0, 1)
-- pruning [('D','4'),('H','1'),('E','4')] [('D','3'),('D','2'),('E','4')] (0, 0, 1)
noDistanceOf:: Int -> [Location] -> Location -> Bool
noDistanceOf dist previouGuess thisSearchSpace=
    dist `notElem` [calculateDistant thisSearchSpace x| x <-previouGuess]

notElemOf:: [Location] -> Location -> Bool
notElemOf previouGuess thisSearchSpace=
    thisSearchSpace `notElem` previouGuess
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
initialGuess = ([('A','1'),('A','2'),('B','1')], (generateAllChoices "ABCDEFGH" "1234", 0))
    -- where initialSearchSpace = generateAllChoices

nextGuess :: ([Location],GameState) -> (Int,Int,Int) -> ([Location],GameState)
nextGuess (previouGuess, (originalSearchSpace,index)) (correct_num, dist_1, dist_2) =
    if dist_2 == 3 && previouGuess == [('A','1'),('A','2'),('B','1')]
        then (nextGuess,(initialPrunedSearchSpace,index+1))
    else
        (nextGuess, (prunedSearchSpace,index+1))

    where
        prunedSearchSpace =
            pruning_dist_2 previouGuess (
                pruning_dist_1 previouGuess (
                    pruningCorrectNum previouGuess originalSearchSpace correct_num)
                dist_1)
            dist_1 dist_2
        initialPrunedSearchSpace = ('C','3') : ('D', '4') : generateAllChoices "EFGH" "1234"
        guessCandidates = chooseThree 3 prunedSearchSpace
        possibilityListSorted = sort(map (calculatePossibility guessCandidates) guessCandidates)
        nextGuess = snd(possibilityListSorted !!index)
-- calculatePossibility (chooseThree 3 generateAllChoices) [('C','2'),('D','2'),('E','2')]

calculatePossibility :: Fractional a1 => [[Location]] -> [Location] -> (a1, [Location])
calculatePossibility guessCandidates guess= (calculatePossibilityOfOneGuess [feedback target guess| target <- guessCandidates],guess)

calculatePossibilityOfOneGuess :: (Ord a2, Fractional a1) => [a2] -> a1
calculatePossibilityOfOneGuess feedBacks = sum([fractionalDivide (length x * length x) (length feedBacks)| x <- group(sort feedBacks)])

fractionalDivide :: (Fractional a1, Integral a2, Integral a3) => a2 -> a3 -> a1
fractionalDivide a b = fromIntegral a / fromIntegral b


-- nextGuess ([('A','1'),('A','2'),('B','1')], (generateAllChoices "ABCDEFGH" "1234",0)) (0, 0, 3)