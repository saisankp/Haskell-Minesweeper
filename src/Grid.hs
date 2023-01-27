module Grid (
    Grid (..), StateOfSquare (..),
    performActionsForClick,
    mined, flagged, unmined, 
    questioned, 
    setupGrid, getSquare, 
    getStateOfSquare, 
    setStateOfSquare,
    insideGrid,
    mineSquareToReveal, mine,
    running, userWon, userLost,
    refreshMinesweeperStatus,
) where
import System.Random
import Data.Vector

-- The different status types of the game at any moment
data MinesweeperStatus = UserLost | Running | UserWon deriving (Eq, Show)
-- The different states of a square in the grid at any moment
data StateOfSquare = Flagged | Mined | Questioned | Unmined  deriving (Eq, Show)
-- In the Minesweeper game, a square has a number (Int) representing how many mines it is near to
type Square = Int
data Grid = Grid {
    width :: Int,
    height :: Int,
    amountOfMines :: Int,
    squares :: Vector (Vector Square),
    squareStatus :: Vector (Vector StateOfSquare),
    minesweeperStatus :: MinesweeperStatus
}

-- A normal square with no mine
empty :: Square
empty = 0

-- A square with a mine
mine :: Square
mine = -1

-- The state of squares can be changed with the 4 functions below
mined :: StateOfSquare
mined = Mined

unmined :: StateOfSquare
unmined = Unmined 

flagged :: StateOfSquare
flagged = Flagged

questioned :: StateOfSquare
questioned = Questioned

-- The status of the game can be changed with 3 functions below
running :: MinesweeperStatus
running = Running

userWon :: MinesweeperStatus
userWon = UserWon 

userLost :: MinesweeperStatus
userLost = UserLost 

-- If a click happens, then it can either be a user trying to question, mine or flag a square
performActionsForClick :: String -> Grid -> (Int, Int) -> Grid
performActionsForClick typeOfClick grid (x,y) = do
    let stateOfSquare = getStateOfSquare grid (x,y) 
    if typeOfClick == "question" then
        if (stateOfSquare == flagged || stateOfSquare == unmined) then
            -- Set the state of the square as questioned
            setStateOfSquare grid (x,y) questioned
        else if stateOfSquare == questioned then
            setStateOfSquare grid (x,y) unmined
        else grid
    else if typeOfClick == "mine" then
        if stateOfSquare == mined then grid
        -- Mine the square to reveal an upcoming refresh of the game status
        else mineSquareToReveal (x,y) (setStateOfSquare grid (x,y) unmined)
    else if typeOfClick == "flag" then
        if (stateOfSquare == questioned || stateOfSquare == unmined) then
            -- Set the state of the square as flagged
            setStateOfSquare grid (x,y) flagged
        else if stateOfSquare == flagged then
            setStateOfSquare grid (x,y) unmined
        else grid
    else error "The type of click is invalid"

-- Setup the grid by making an empty grid and filling it with elements
setupGrid :: Int -> Int -> Int -> (Int, Int) -> StdGen -> Grid
setupGrid width height amountOfMines coordinatesToNotFill stdGen = do
    let gridWithNoElements = getEmptyGrid width height amountOfMines
    fillGridWithElements stdGen gridWithNoElements amountOfMines coordinatesToNotFill 

-- Return an empty grid with blank unmined squares and a particular height, width and number of mines
getEmptyGrid :: Int -> Int -> Int -> Grid
getEmptyGrid width height amountOfMines = Grid {
    width = width,
    height = height,
    amountOfMines = amountOfMines,
    squares = generate width (\_ -> Data.Vector.replicate height Grid.empty),
    squareStatus = generate width (\_ -> Data.Vector.replicate height Unmined),
    minesweeperStatus = running
}


-- Fill the grid with elements by using a random number generator to find a coordinate (xVal, yVal) for each mine
fillGridWithElements :: StdGen -> Grid -> Int -> (Int, Int) -> Grid
fillGridWithElements _ grid 0 _ = grid
fillGridWithElements numberGenerator grid amountOfMines coordinatesToNotFill = do
    let (yVal, newGenerator) = randomR (0, height grid-1) numberGenerator
    let (xVal, updatedNumberGenerator) = randomR (0, width grid-1) newGenerator
    -- If there is already a mine there, then we simply try again and pick a new random number
    if (getSquare grid (xVal, yVal) == mine) then
        fillGridWithElements updatedNumberGenerator grid amountOfMines coordinatesToNotFill
    else
        -- If the coordinate where we try to place a mine is not meant to hold one, then we get a new random number
        if (xVal, yVal) == coordinatesToNotFill then
            fillGridWithElements updatedNumberGenerator grid amountOfMines coordinatesToNotFill
        else
            fillGridWithElements updatedNumberGenerator (addMineToSquare (xVal, yVal) grid) (amountOfMines-1) coordinatesToNotFill 

-- Reveal squares in any direction (top, bottom, left, right, and 4 diagonals) from a coordinate 
revealMine :: (Int, Int) -> Grid -> Grid
revealMine (x,y) grid = do
    if getSquare grid (x,y) == Grid.empty then
        let updatedBoard = setStateOfSquare grid (x,y) mined
        in mineSquareToReveal (x-1, y) $ mineSquareToReveal (x, y-1) $ mineSquareToReveal (x+1, y) $ mineSquareToReveal (x, y+1) updatedBoard
    else setStateOfSquare grid (x,y) mined

-- Mine a square, revealing if you are safe or not
mineSquareToReveal :: (Int, Int) -> Grid -> Grid
mineSquareToReveal (x,y) grid = do
    if insideGrid grid (x,y) then
        if (getStateOfSquare grid (x,y) == questioned || getStateOfSquare grid (x,y) == unmined) then
            revealMine (x,y) grid
        else setStateOfSquare grid (x,y) mined
    else grid

-- To update the game state, we can iterate through each square and see if there is any square that indicates a loss (otherwise it's a win or simply in progress)
refreshMinesweeperStatus :: Grid -> Grid
refreshMinesweeperStatus grid = do
    let sequenceOfCoordinates = [(x,y) | y <- [0 .. Grid.height grid - 1], x <- [0 .. Grid.width grid - 1]]
    refreshMinesweeperOneSquareAtATime grid userWon sequenceOfCoordinates
    where
        refreshMinesweeperOneSquareAtATime grid state [] = grid {minesweeperStatus = state}
        -- Hitting a unmined mine means we lose, if we hit a unmined normal square then the game is still running, otherwise we won
        refreshMinesweeperOneSquareAtATime grid UserWon (coords:coordsList) | (getStateOfSquare grid coords == mined && getSquare grid coords == mine) = grid {minesweeperStatus = userLost}
                                                                            | (getStateOfSquare grid coords /= mined && getSquare grid coords /= mine) = refreshMinesweeperOneSquareAtATime grid Running coordsList
                                                                            | otherwise = refreshMinesweeperOneSquareAtATime grid UserWon coordsList 
        refreshMinesweeperOneSquareAtATime grid Running (coords:coordsList) | (getStateOfSquare grid coords == mined && getSquare grid coords == mine) = grid {minesweeperStatus = userLost}
                                                                            | otherwise = refreshMinesweeperOneSquareAtATime grid Running coordsList


-- Change the state of a square given its coordinates
setStateOfSquare :: Grid -> (Int, Int) -> StateOfSquare -> Grid
setStateOfSquare grid (x,y) status = do
    -- Add the new state to the square in question, and add this to the grid after updating the states
    let updatedStates = update (squareStatus grid) (singleton (x, (update (squareStatus grid ! x) (singleton (y, status)))))
    grid {squareStatus = updatedStates}

-- If the square coordinate is inside the grid, then return the square's state
getStateOfSquare :: Grid -> (Int, Int) -> StateOfSquare
getStateOfSquare grid (x,y) = if insideGrid grid (x,y) then do
                                    squareStatus grid !x !y
                                else error "Invalid square coordinates"

-- Get the value inside the square                                
getSquare ::  Grid -> (Int, Int) -> Square
getSquare grid (x,y) | insideGrid grid (x,y) = squares grid ! x ! y
                     | not (insideGrid grid (x,y)) = -1

-- Change value inside a square
setSquare :: (Int, Int) -> Grid -> Square -> Grid
setSquare (x,y) grid value = do
    let updatedSquares = update (squares grid) (singleton (x, (update (squares grid ! x) (singleton (y, value)))))
    grid {squares = updatedSquares}

-- Checking if a point in inside the Grid or not
insideGrid :: Grid -> (Int, Int) -> Bool
insideGrid grid (xVal, yVal) = if (yVal >= 0 && xVal >= 0) then
                                    if (yVal < height grid && xVal < width grid) then True
                                    else False
                                else False

-- Add mine to square given its coordinates
addMineToSquare :: (Int, Int) -> Grid -> Grid
addMineToSquare (x,y) grid = do
    let boardWithMinePlanted = setSquare (x,y) grid mine 
    let listOfSquareCoordinates = [(x-1, y-1), (x,y-1), (x+1, y-1), (x-1, y), (x+1, y), (x-1, y+1), (x,y+1), (x+1, y+1)]
    -- Update the value in squares that are nearby (top, bottom, left, right, and 4 diagonals)
    addToNearbySquares listOfSquareCoordinates boardWithMinePlanted

-- Add to nearby squares using recursion adding 1 to each square beside it (using addToSquare)
addToNearbySquares :: [(Int, Int)] -> Grid -> Grid
addToNearbySquares listOfSquareCoordinates grid = do
    case listOfSquareCoordinates of
        [] -> grid
        (firstCoordinate: remainingCoordinates) -> (if getSquare grid firstCoordinate == mine then
                                                        addToNearbySquares remainingCoordinates grid
                                                    else
                                                        addToSquare firstCoordinate (addToNearbySquares remainingCoordinates grid))

-- Adding 1 to the value of a square given its coordinates
addToSquare :: (Int, Int) -> Grid -> Grid
addToSquare coordinateOfSquare grid | insideGrid grid coordinateOfSquare = setSquare coordinateOfSquare grid (getSquare grid coordinateOfSquare + 1)
                                    | not (insideGrid grid coordinateOfSquare) = grid