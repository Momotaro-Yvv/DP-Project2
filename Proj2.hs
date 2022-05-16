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
            -- feedback,
            GameState,
            -- initialGuess,
            -- nextGuess
            ) where
import Data.List

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
countCorrect guess target = countCorrect' (sort guess) (sort target)
countCorrect' [] _ = 0
countCorrect' _ [] = 0
countCorrect' guess@(x: xs) target@(y: ys)
    | x < y = countCorrect' xs target
    | x == y = countCorrect' xs ys + 1
    | otherwise = countCorrect' target ys


count_1and2::[Location] -> [Location] -> (Int,Int)
count_1and2 =
{-
Main Functions
-}

-- toLocation
-- gives Just the Location named by the string,
-- or Nothing if the string is not a valid location name.
toLocation :: String -> Maybe Location
toLocation str
    | length str /= 2             = Nothing
    | fst `notElem` "ABCDEFG"     = Nothing
    | scd `notElem` "1234"        = Nothing
    | otherwise                         = Just (fst,scd)
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
feedback [x1,x2,x3] [y1,y2,y3] = (correct, dist_1, dist_2)
    where
        correct = countCorrect [x1,x2,x3] [y1,y2,y3]
        (dist_1, dist_2)  = count_1and2 [x1,x2,x3] [y1,y2,y3]

-- initialGuess
-- initialGuess :: ([Location],GameState)

-- nextGuess
-- nextGuess :: ([Location],GameState) -> (Int,Int,Int) -> ([Location],GameState)