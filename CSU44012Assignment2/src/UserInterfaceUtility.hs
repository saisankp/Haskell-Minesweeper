module UserInterfaceUtility (
    heightOfCanvas, widthOfCanvas, updateCanvas, 
    drawLineAtIntervals, showChosenInputType,
    canvasToGridCoordinate, revealAllMines, 
    getListOfSquaresAndDraw, findIfWinOrLose,
    drawSquare
) where
import Graphics.UI.Threepenny
import Control.Monad
import Data.IORef
import System.Random
import Grid
import Autoplayer


-- Height of canvas (600px was a reasonable size that fit well into my screen)
heightOfCanvas :: Double
heightOfCanvas = 600 :: Double

-- Width of canvas (600px was a reasonable size that fit well into my screen)
widthOfCanvas :: Double
widthOfCanvas = 600 :: Double

{- Remove the old canvas and display the new state of the game given data of the game.
   if there is a win/loss, we end the game and display text to show this -}
updateCanvas :: (Int, Int) -> Element -> Grid -> Double -> Double -> Int -> UI ()
updateCanvas (x,y) canvas stateOfGame numberOfRowsInGrid numberOfColumnsInGrid amountOfMines = do
        clearCanvas canvas
        if minesweeperStatus stateOfGame == Grid.running then do
            getListOfSquaresAndDraw (x,y) False (heightOfCanvas/numberOfRowsInGrid) (widthOfCanvas/numberOfColumnsInGrid) canvas stateOfGame
            drawLineAtIntervals "width" canvas (0,0) (heightOfCanvas/numberOfColumnsInGrid) numberOfColumnsInGrid
            drawLineAtIntervals "height" canvas (0,0) (widthOfCanvas/numberOfRowsInGrid) numberOfRowsInGrid
        else do
            getListOfSquaresAndDraw (x,y) True (heightOfCanvas/numberOfRowsInGrid) (widthOfCanvas/numberOfColumnsInGrid) canvas stateOfGame
            drawLineAtIntervals "width" canvas (0,0) (heightOfCanvas/numberOfColumnsInGrid) numberOfColumnsInGrid
            drawLineAtIntervals "height" canvas (0,0) (widthOfCanvas/numberOfRowsInGrid) numberOfRowsInGrid
            set' textFont "50px sans-serif" canvas 
            set' strokeStyle "black" canvas
            set' lineWidth 2 canvas
            if minesweeperStatus stateOfGame == userWon then do
                set' fillStyle (htmlColor "green") canvas
                fillText "You win!" (300,290) canvas
                strokeText "You win!" (300,290) canvas
            else if minesweeperStatus stateOfGame == userLost then do
                set' fillStyle (htmlColor "red") canvas
                fillText "You lose!" (300,290) canvas
                strokeText "You lose!" (300,290) canvas
            else (pure())
            set' textFont "20px sans-serif" canvas 


{- We need to draw lines both from bottom to top, and left to right. To do this,
   we can recursively draw lines with the appropriate width and height of the grid squares -}
drawLineAtIntervals :: String -> Element -> Point -> Double -> Double -> UI ()
drawLineAtIntervals heightOrWidth canvas (x,y) heightOrWidthOfGrid numberOfLines = 
    -- Draw horizontally from bottom (x=0, y=0 passed in) to top (x=0, y++ each iteration)
    if heightOrWidth == "height" then
        do {
            drawLineFromPointOneToPointTwo canvas ((0,y), (heightOfCanvas,y));
            when (numberOfLines > 0) (drawLineAtIntervals "height" canvas (x, y + heightOrWidthOfGrid) heightOrWidthOfGrid (numberOfLines - 1))}
    -- Draw vertically from left (y=0, x=0 passed in) to right (y=0, x++ each iteration)
    else if heightOrWidth == "width" then
        do {
            drawLineFromPointOneToPointTwo canvas ((x,0), (x,widthOfCanvas));
            when (numberOfLines > 0) (drawLineAtIntervals "width" canvas (x+heightOrWidthOfGrid, y) heightOrWidthOfGrid (numberOfLines - 1))}
    else error "Incorrect type of drawing direction passed into function"


{- Show a bold border around one of the option buttons for mine, flag, and question 
   on the right of the grid, while ensuring the other two are not bold anymore -}
showChosenInputType :: Element -> Element -> Element -> UI ()
showChosenInputType setFirstNormalBorder setSecondNormalBorder setThickBorder = do
    set' style [("border", "solid black 1px"), ("font-weight", "normal")] setFirstNormalBorder
    set' style [("border", "solid black 1px"), ("font-weight", "normal")] setSecondNormalBorder
    set' style [("border", "solid red 4px"), ("font-weight", "bold")] setThickBorder

{- Making our own hashmap of sorts, where we have different colors for each number from the original Minesweeper game,
   The colors are based from here if you're interested (https://preview.redd.it/85h4esk1hew41.jpg?auto=webp&s=9291748abbb92f54efc5d3ee7cf72cb3321a2d17) -}
setColorForNumber :: Int -> Element -> UI()
setColorForNumber number canvas | number == 1 = set' fillStyle (htmlColor "Blue") canvas
                                | number == 2 = set' fillStyle (htmlColor "ForestGreen") canvas
                                | number == 3 = set' fillStyle (htmlColor "Red") canvas
                                | number == 4 = set' fillStyle (htmlColor "DarkBlue") canvas
                                | number == 5 = set' fillStyle (htmlColor "Brown") canvas
                                | number == 6 = set' fillStyle (htmlColor "DarkCyan") canvas
                                | number == 7 = set' fillStyle (htmlColor "Black") canvas
                                | number == 8 = set' fillStyle (htmlColor "Grey") canvas
                                | otherwise = set' fillStyle (htmlColor "Black") canvas

-- Return a coordinate of a square on the grid by taking in coordinates of a point on the canvas
canvasToGridCoordinate :: Point -> Double -> IO (Int, Int)
canvasToGridCoordinate (x,y) sizeOfSquare = return (gridX, gridY)
    where
        gridY = floor (y/sizeOfSquare)
        gridX = floor (x/sizeOfSquare)

-- Reveal all mines using recusion by going along the x-axis (horizontal axis using x++) and revealing all the values along the y-axis (vertical axis using revealMinesInColumnForX)
revealAllMines :: (Int, Int) -> IORef Grid -> Grid -> Element -> Double -> Double -> Int -> Int -> UI ()
revealAllMines coordinates grid stateOfGame canvas numberOfRowsInGrid numberOfColumnsInGrid amountOfMines x = do
    if ((fromIntegral x <= numberOfColumnsInGrid)) then do
        -- Use y=0 to start from the bottom of each x-axis value and go to the top
        revealMinesInColumnForX coordinates grid stateOfGame canvas numberOfRowsInGrid numberOfColumnsInGrid amountOfMines (x, 0)
        updateCanvas (coordinates) canvas stateOfGame numberOfRowsInGrid numberOfColumnsInGrid amountOfMines
        stateOfGame <- liftIO $ readIORef grid
        -- Now increment x, and call this function again to reveal the y-axis values in the next column (i.e. for x++)
        increase <- return(x+1)
        revealAllMines coordinates grid stateOfGame canvas numberOfRowsInGrid numberOfColumnsInGrid amountOfMines increase
    else pure()

-- For a given x-axis value (horizontal axis), we reveal all values/mines in squares from bottom (y=0) to top (y++ each iteration)
revealMinesInColumnForX :: (Int, Int) -> IORef Grid -> Grid -> Element -> Double -> Double -> Int -> (Int, Int) -> UI ()
revealMinesInColumnForX coordinates grid stateOfGame canvas numberOfRowsInGrid numberOfColumnsInGrid amountOfMines (x,y) = do
    if ((x <= Grid.width stateOfGame - 1) && (y <= Grid.height stateOfGame - 1)) then do
        if (getSquare stateOfGame (x,y)) == mine then do
            stateOfGame <- liftIO $ readIORef grid
            liftIO $ writeIORef grid (refreshMinesweeperStatus (performActionsForClick "mine" stateOfGame (x,y)))
        else (pure())
        revealMinesInColumnForX coordinates grid stateOfGame canvas numberOfRowsInGrid numberOfColumnsInGrid amountOfMines (x, y+1)
    else pure()

-- For parts of the grid, we need to draw a line from point initial point (x,y) to final point (a,b)
drawLineFromPointOneToPointTwo :: Element -> (Point, Point) -> UI ()
drawLineFromPointOneToPointTwo canvas (initialPoint,finalPoint) = do
    beginPath canvas
    moveTo initialPoint canvas
    lineTo finalPoint canvas
    closePath canvas
    stroke canvas

-- Draw squares after calculating a list of points (x,y) on the grid
getListOfSquaresAndDraw :: (Int, Int) -> Bool -> Double -> Double -> Element -> Grid -> UI ()
getListOfSquaresAndDraw (a,b) isGameEndingMine widthOfGrid heightOfGrid canvas grid = do
    let sequenceOfDrawings = [drawSquare (a,b) isGameEndingMine widthOfGrid heightOfGrid (x,y) canvas grid | y <- [0 .. Grid.height grid - 1], x <- [0 .. Grid.width grid - 1]]
    sequence_ (sequenceOfDrawings)

getCoordinates :: Double -> Double -> (Int, Int) -> [Double]
getCoordinates widthOfGrid heightOfGrid (x,y) = [((fromIntegral x * widthOfGrid) + widthOfGrid/2), ((fromIntegral y * heightOfGrid) + heightOfGrid * 0.75), (fromIntegral x * widthOfGrid), (fromIntegral y * heightOfGrid)]


-- Draw each square based on the coordinates and the current condition of the square
drawSquare :: (Int, Int) -> Bool -> Double -> Double -> (Int, Int) -> Element -> Grid -> UI ()
drawSquare (a,b) isGameEndingMine widthOfGrid heightOfGrid (x,y) canvas grid = do
    if squareCondition == questioned then
        drawQuestionInSquare canvas (xValForCanvas, yValForCanvas) (xVal, yVal) widthOfGrid heightOfGrid
    else if squareCondition == mined then
        if squareNumber == mine then
            if isGameEndingMine && (x,y) == (a,b) then
                drawMineInSquare "Red" canvas (xValForCanvas, yValForCanvas) (xVal, yVal) widthOfGrid heightOfGrid
            else drawMineInSquare "DarkGray" canvas (xValForCanvas, yValForCanvas) (xVal, yVal) widthOfGrid heightOfGrid
        else drawNumberInSquare squareNumber canvas (xVal, yVal) (xValForCanvas, yValForCanvas) widthOfGrid heightOfGrid
    else if squareCondition == flagged then 
        drawFlagInSquare canvas (xValForCanvas, yValForCanvas) (xVal, yVal) widthOfGrid heightOfGrid
    else pure ()
    where 
        squareCondition = getStateOfSquare grid (x,y)
        squareNumber = getSquare grid (x,y)
        -- !! represents the action of indexing into a list, for example [1,2,3] !! 0 gets the first element of the list i.e. 1
        xVal = (getCoordinates widthOfGrid heightOfGrid (x,y)) !! 0
        yVal = (getCoordinates widthOfGrid heightOfGrid (x,y)) !! 1
        xValForCanvas = (getCoordinates widthOfGrid heightOfGrid (x,y)) !! 2
        yValForCanvas = (getCoordinates widthOfGrid heightOfGrid (x,y)) !! 3

-- Find out if the game has reached a status of winning or losing
findIfWinOrLose :: Grid -> Bool
findIfWinOrLose gridStatus = do
    if minesweeperStatus gridStatus == userLost then False
    else True

-- Draw a number in the square, using a darker grey color to fill the square to mimic the original Minesweeper
drawNumberInSquare :: Int -> Element -> (Double , Double) -> (Double, Double) -> Double -> Double -> UI()
drawNumberInSquare number canvas coordinates topLeftCoords widthOfGrid heightOfGrid = do
    set' strokeStyle "black" canvas
    set' lineWidth 1 canvas
    set' fillStyle (htmlColor "DarkGray") canvas
    fillRect topLeftCoords widthOfGrid heightOfGrid canvas
    setColorForNumber number canvas;
    set' textFont "16px sans-serif" canvas
    -- Squares with 0 muust be shown as empty
    if number /= 0 then do
        fillText (show number) coordinates canvas
        strokeText (show number) coordinates canvas
    else do pure()

-- A question mark can be shown inside a particular square (using the question mark emoji) given its coordinates
drawQuestionInSquare :: Element -> (Double, Double) -> (Double, Double) -> Double -> Double -> UI()
drawQuestionInSquare canvas topLeftCoords centerCoords widthOfGrid heightOfGrid = do
    set' textFont "15px sans-serif" canvas
    set' fillStyle (htmlColor "white") canvas
    fillText "❓" centerCoords canvas

-- A flag can be shown inside a particular square (using the flag emoji) given its coordinates
drawFlagInSquare :: Element -> (Double, Double) -> (Double, Double) -> Double -> Double -> UI()
drawFlagInSquare canvas topLeftCoords centerCoords widthOfGrid heightOfGrid = do
    set' textFont "15px sans-serif" canvas
    set' fillStyle (htmlColor "white") canvas
    fillText "🚩" centerCoords canvas

-- A mine can be shown inside a particular square (using the bomb emoji) given its coordinates
drawMineInSquare :: String -> Element -> (Double, Double) -> (Double, Double) -> Double -> Double -> UI()
drawMineInSquare colorOfSquare canvas topLeftCoords centerCoords widthOfGrid heightOfGrid = do
    set' fillStyle (htmlColor colorOfSquare) canvas
    fillRect topLeftCoords widthOfGrid heightOfGrid canvas
    set' textFont "15px sans-serif" canvas
    set' fillStyle (htmlColor "white") canvas
    fillText "💣" centerCoords canvas