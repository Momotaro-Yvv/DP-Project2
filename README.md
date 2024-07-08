# Project Overview
This is a two-player guessing game written in Haskell, to showcasing the idea of declarative programming paradigm style.

## How to play
The game is played on a 4×8 grid, where the searcher need to guess the locations of 3 battleships hidden by the hider.
After each guess, the hider responds with three numbers, which the searcher can use in next rounds.
The game will end till all 3 battleships been found.

## Implementation Details
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
