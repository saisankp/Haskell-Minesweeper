module Autoplayer (
    autoPlay
) where

import System.Random
import Grid
import Data.Maybe


-- Use a random number generator to randomly mine a square
moveRandomly :: Grid -> StdGen -> Grid 
moveRandomly grid numberGenerator = do
    let (yVal, newGenerator) = randomR (0, height grid-1) numberGenerator
    let (xVal, updatedNumberGenerator) = randomR (0, width grid-1) newGenerator
    let stateOfSquare = getStateOfSquare grid (xVal, yVal)
    if stateOfSquare == unmined then 
        mineSquareToReveal (xVal, yVal) grid
    -- If this square is unmined, then we try again
    else moveRandomly grid updatedNumberGenerator

-- Find where there are squares beside a given point (x,y) (i.e. top, bottom, left, right, and 4 diagonals) and return a list of those points
getSquaresBeside :: (Int, Int) -> Grid  -> [(Int, Int)]
getSquaresBeside (x,y) grid = do
    let listOfSquareCoordinates = [(x-1, y-1), (x, y-1), (x+1, y-1), (x-1, y), (x+1, y), (x-1, y+1), (x, y+1), (x+1, y+1)]
    filter (insideGrid grid) listOfSquareCoordinates

-- Strategy 1: If value of square = actual number of nearby flags, then remaining nearby squares have no mines.
patternMatchWithSquareUncover :: (Int, Int) -> Grid -> Maybe Grid
patternMatchWithSquareUncover (x,y) grid = do
    let numberOfMinesNearby = getSquare grid (x,y)
    let squaresNearby = getSquaresBeside (x,y) grid 
    let statesOfSquaresNearby = (map (getStateOfSquare grid) squaresNearby)
    let numberOfUnminedSquaresNearby = length (filter (\x -> x == unmined) statesOfSquaresNearby)
    let numberOfFlagsNearby = length (filter (\x -> x == flagged) statesOfSquaresNearby)
    case numberOfFlagsNearby of
        _ | (numberOfUnminedSquaresNearby > 0 && numberOfMinesNearby == numberOfFlagsNearby) -> (if (getStateOfSquare grid (x,y) == mined) then 
                                                                                                    Just (openUpUnmined squaresNearby grid)
                                                                                                 else Nothing)
        _ -> Nothing

-- Strategy 2: If the value of a square = number of unmined squares nearby, they are all mines.
patternMatchWithFlags :: (Int, Int) -> Grid -> Maybe Grid
patternMatchWithFlags (x,y) grid = do
    let numberOfMinesNearby = getSquare grid (x,y)
    let squaresNearby = getSquaresBeside (x,y) grid 
    let statesOfSquaresNearby = (map (getStateOfSquare grid) squaresNearby)
    let numberOfFlagsNearby = length (filter (\x -> x == flagged ) statesOfSquaresNearby)
    let numberOfUnminedSquaresNearby = length (filter (\x -> x == unmined) statesOfSquaresNearby)
    case numberOfUnminedSquaresNearby of
        _ | (numberOfUnminedSquaresNearby > 0 && numberOfUnminedSquaresNearby == numberOfMinesNearby-numberOfFlagsNearby) -> Just (makeNearbyUnminedFlagged squaresNearby grid)
        _ -> Nothing

-- Make a unmined square that is first seen as flagged
makeNearbyUnminedFlagged ::  [(Int, Int)] -> Grid -> Grid
makeNearbyUnminedFlagged [] grid = grid
makeNearbyUnminedFlagged (firstSquare : remainingSquares) grid = do
    if getStateOfSquare grid firstSquare /= unmined then 
        makeNearbyUnminedFlagged remainingSquares grid
    else setStateOfSquare grid firstSquare flagged

-- Open a unmined square that is first seen
openUpUnmined :: [(Int, Int)] -> Grid -> Grid
openUpUnmined [] grid = grid
openUpUnmined (firstSquare : remainingSquares) grid = do
    if getStateOfSquare grid firstSquare /= unmined then 
        openUpUnmined remainingSquares grid 
    else mineSquareToReveal firstSquare grid

-- Make a move as the auto-player by using 2 strategies first and if those fail then we choose randomly
autoPlay :: Grid -> IO Grid
autoPlay grid = do
    let sequenceOfCoordinates = [(x,y) | y <- [0 .. Grid.height grid - 1], x <- [0 .. Grid.width grid - 1]]
    -- Strategy 1: If value of square = actual number of nearby flags, then remaining nearby squares have no mines
    let patternWithSquareUncover =  findPattern grid patternMatchWithSquareUncover sequenceOfCoordinates
    -- Strategy 2: If value of a square = number of unmined squares nearby, then each of those are mines
    let patternWithFlag =  findPattern grid patternMatchWithFlags sequenceOfCoordinates 
    -- If these 2 strategies fail, then we simply choose randomly as a last resort
    if isJust patternWithFlag then 
        return (fromMaybe grid patternWithFlag)
    else if isJust patternWithSquareUncover then 
        return (fromMaybe grid patternWithSquareUncover)
    else do
        fmap (moveRandomly grid) newStdGen
    where
        -- Keep applying the strategy function
        findPattern _ _ [] = Nothing
        findPattern grid functionWithAutoplayerStrategy (xy : xys) = do
            if isJust (functionWithAutoplayerStrategy xy grid) then (functionWithAutoplayerStrategy xy grid)
            else findPattern grid functionWithAutoplayerStrategy xys