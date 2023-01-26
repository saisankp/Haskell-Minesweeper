module Minesweeper (
    startMinesweeper
) where
import Graphics.UI.Threepenny
import Control.Monad
import Data.IORef
import System.Random
import Grid
import Autoplayer
import UserInterfaceUtility

data Modes = CurrentlyMining | CurrentlyFlagging | CurrentlyQuestioning

{- After a difficulty level is chosen, this function starts the game by showing the mine, flag, and question button on the right
   and also the back, auto-play, and refresh game button on the left -}
startMinesweeper :: Double -> Double -> Window -> Int -> UI ()
startMinesweeper numberOfRowsInGrid numberOfColumnsInGrid window amountOfMines = do
    -- Buttons that are shown when the game is running (using emoji's as the contents of the buttons), with the mine button chosen as default
    mineSquare <- button #+ [string "⛏️"] # set style [("position", "absolute"), ("top", "30%"), ("left", "71.5%"),("transform", "translate(-50%, -50%)%"), ("border", "solid black 1px"), ("font-size", "40px")]
    flagSquare <- button #+ [string "🚩"] # set style [("position", "absolute"), ("top", "46%"), ("left", "71.5%"),("transform", "translate(-50%, -50%)%"), ("border", "solid black 1px"), ("font-size", "40px")]
    unsureSquare <- button #+ [string "❓"] # set style [("position", "absolute"), ("top", "62%"), ("left", "71.5%"),("transform", "translate(-50%, -50%)%"), ("border", "solid black 1px"), ("font-size", "40px")]
    backToHome <- button #+ [string "⬅️"] # set style [("position", "absolute"), ("top", "30%"), ("left", "24%"),("transform", "translate(-50%, -50%)%"), ("border", "solid DimGray 4px"), ("font-size", "40px")]
    playMove <- button #+ [string "▶️"] # set style [("position", "absolute"), ("top", "46%"), ("left", "24%"),("transform", "translate(-50%, -50%)%"), ("border", "solid DimGray 4px"), ("font-size", "40px")]
    refreshGame <- button #+ [string "🔁"] # set style [("position", "absolute"), ("top", "62%"), ("left", "24%"),("transform", "translate(-50%, -50%)%"), ("border", "solid DimGray 4px"), ("font-size", "40px")]
    showChosenInputType flagSquare unsureSquare mineSquare

    -- Add styling and height/width to the canvas for aesthetic purposes
    canvas <- canvas # set style [("position", "absolute"), ("top", "0"), ("bottom", "0"), ("left", "0"), ("right", "0"), ("margin", "auto"), ("border", "solid black 1px"), ("background", "#eee")]
                     # set Graphics.UI.Threepenny.height (floor heightOfCanvas)
                     # set Graphics.UI.Threepenny.width (floor widthOfCanvas)
                     # set textAlign Center 

    -- Declare IORefs to store details about the game
    modeInGame <- liftIO $ newIORef CurrentlyMining
    randomNumberGenerator <- liftIO newStdGen
    mousePosition <- liftIO $ newIORef (0,0)
    startMineSweeper <- liftIO $ newIORef False
    grid <- liftIO $ newIORef $ setupGrid (floor numberOfRowsInGrid) (floor numberOfColumnsInGrid) amountOfMines (0, 0) randomNumberGenerator
    
    -- Draw the grid, passing in (0,0) to kick off recursion
    drawLineAtIntervals "width" canvas (0,0) (widthOfCanvas/numberOfColumnsInGrid) numberOfColumnsInGrid
    drawLineAtIntervals "height" canvas (0,0) (heightOfCanvas/numberOfRowsInGrid) numberOfRowsInGrid
    getBody window #+ [element mineSquare, element flagSquare, element unsureSquare, element backToHome, element playMove, element refreshGame, column [element canvas]]

    -- Every time the mouse moves, we keep track of it's position
    on mousemove canvas $ \xy -> do
        liftIO $ writeIORef mousePosition xy

     -- Every time the mine button is pressed, we change its border to bold and the rest to normal
    on click mineSquare $ \_ -> do
        showChosenInputType flagSquare unsureSquare mineSquare
        liftIO $ writeIORef modeInGame CurrentlyMining

    -- Every time the flag button is pressed, we change its border to bold and the rest to normal
    on click flagSquare $ \_ -> do
        showChosenInputType mineSquare unsureSquare flagSquare
        liftIO $ writeIORef modeInGame CurrentlyFlagging

    -- Every time the unsure button is pressed, we change its border to bold and the rest to normal    
    on click unsureSquare $ \_ -> do
        showChosenInputType mineSquare flagSquare unsureSquare
        liftIO $ writeIORef modeInGame CurrentlyQuestioning

    -- Every time the back button is pressed, we simply refresh the webpage
    on click backToHome $ \_ -> do
        runFunction $ ffi "window.location.reload()"

    -- Every time the auto-play button is pressed, we use our autoplayer make a decision and update the board with this new chosen square
    on click playMove $ \_ -> do
        stateOfGame <- liftIO $ readIORef grid
        let gameStatus = findIfWinOrLose(stateOfGame)
        if gameStatus then do
            updatedGrid <- liftIO $ autoPlay stateOfGame
            liftIO $ writeIORef grid updatedGrid
            stateOfGame <- liftIO $ readIORef grid
            updatedGrid <- liftIO $ return $ refreshMinesweeperStatus stateOfGame
            liftIO $ writeIORef grid updatedGrid
            stateOfGame <- liftIO $ readIORef grid
            (x,y) <- liftIO $ readIORef mousePosition
            indexOnGrid <- liftIO $ canvasToGridCoordinate (x,y) (heightOfCanvas/numberOfRowsInGrid) 
            let gameStatus = findIfWinOrLose(stateOfGame)
            -- If we lose the game after the auto-player chooses a square, then reveal all mines to show to the user
            if (gameStatus == False) then do
                revealAllMines indexOnGrid grid stateOfGame canvas numberOfRowsInGrid numberOfColumnsInGrid amountOfMines 0
            else do
                stateOfGame <- liftIO $ readIORef grid
                updateCanvas (indexOnGrid) canvas stateOfGame numberOfRowsInGrid numberOfColumnsInGrid amountOfMines
        else do
            pure()

    -- Every time the refresh button is pressed, we simply delete all elements and call this function again
    on click refreshGame $ \_ -> do
        delete mineSquare
        delete flagSquare
        delete unsureSquare
        delete playMove
        delete refreshGame
        delete canvas
        startMinesweeper numberOfRowsInGrid numberOfColumnsInGrid window amountOfMines

    -- Every time a square is clicked, we reveal the square and evaluate the game status depending on if a mine was chosen
    on click canvas $ \_ -> do
        -- Get up-to-date data from the IORefs at the moment the user clicked the canvas
        (x,y) <- liftIO $ readIORef mousePosition
        indexOnGrid <- liftIO $ canvasToGridCoordinate (x,y) (heightOfCanvas/numberOfRowsInGrid) 
        gameRunning <- liftIO $ readIORef startMineSweeper
        stateOfGame <- liftIO $ readIORef grid
        
        -- If the user chose a mine on the first go, this is a bit unfair so we can simply refill the board with new values until this doesn't happen
        when (getSquare stateOfGame indexOnGrid == mine && not gameRunning)
            (liftIO $ writeIORef grid (setupGrid (floor numberOfRowsInGrid) (floor numberOfColumnsInGrid) amountOfMines indexOnGrid randomNumberGenerator))

        currentMode <- liftIO $ readIORef modeInGame
        stateOfGame <- liftIO $ readIORef grid
        liftIO $ writeIORef startMineSweeper True
        let gameStatus = findIfWinOrLose(stateOfGame)
        -- Given that the game is not lost yet, then we need to apply the appropriate action depending on the user input (mine, flag, question) 
        if gameStatus then do
            (case currentMode of
                CurrentlyQuestioning -> do liftIO $ writeIORef grid (performActionsForClick "question" stateOfGame indexOnGrid)
                CurrentlyMining -> do liftIO $ writeIORef grid (refreshMinesweeperStatus (performActionsForClick "mine" stateOfGame indexOnGrid))
                CurrentlyFlagging -> do liftIO $ writeIORef grid (performActionsForClick "flag" stateOfGame indexOnGrid))
            stateOfGame <- liftIO $ readIORef grid
            let gameStatus = findIfWinOrLose(stateOfGame)
            -- If this action has lost the game, then we reveal all the mines showing the last mine chosen in a red color (imitating the original Minesweeper)
            if (gameStatus == False) then do
                revealAllMines indexOnGrid grid stateOfGame canvas numberOfRowsInGrid numberOfColumnsInGrid amountOfMines 0
            else do
                stateOfGame <- liftIO $ readIORef grid
                updateCanvas (indexOnGrid) canvas stateOfGame numberOfRowsInGrid numberOfColumnsInGrid amountOfMines
        else do
            pure()