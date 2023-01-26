import Graphics.UI.Threepenny
import Control.Monad
import Data.IORef
import System.Random
import Grid
import Autoplayer
import Minesweeper

main :: IO () 
main = do
    startGUI defaultConfig { jsStatic = Just "src/." } setup

{- Setting up the application by applying CSS, showing the headerText text, and buttons for different levels of the game 
using increasing numbers of squares in the grid as shown in https://freeminesweeper.org/ -}
setup :: Window -> UI()
setup window = do
    addStyleSheet window "styling.css"
    body <- getBody window
    headerText <- Graphics.UI.Threepenny.span
    lineBreak <- Graphics.UI.Threepenny.br
    set' text "Minesweeper - CSU44012 Edition" headerText
    set' style [("color", "white"), ("text-shadow", "black 0px 0px 4px"), ("font-family", "Copperplate"), ("font-size", "60px"), ("position", "absolute"), ("top", "30%"), ("left", "15%"),("transform", "translate(-50%, -50%)%")] headerText
    beginner <- button #+ [string "Beginner mode"] # set style [("position", "absolute"), ("top", "45%"), ("left", "43%"),("transform", "translate(-50%, -50%)%"), ("border", "solid black 2px"), ("font-size", "30px"), ("background", "green")]
    intermediate <- button #+ [string "Intermediate mode"] # set style [("position", "absolute"), ("top", "55%"), ("left", "41.5%"),("transform", "translate(-50%, -50%)%"), ("border", "solid black 2px"), ("font-size", "30px"), ("background", "yellow")]
    expert <- button #+ [string "Expert mode"] # set style [("position", "absolute"), ("top", "65%"), ("left", "44.3%"),("transform", "translate(-50%, -50%)%"), ("border", "solid black 2px"), ("font-size", "30px"), ("background", "red")]
    getBody window #+ [element headerText, element lineBreak, element beginner, element intermediate, element expert]
    return window # set title "CSU44012 Minesweeper"

    -- After clicking on beginner mode, delete the current elements and start the game with 8x8 squares and 10 mines
    on click beginner $ \_ -> do
        delete headerText
        delete beginner
        delete intermediate
        delete expert
        startMinesweeper 8 8 window 10

    -- After clicking on intermediate mode, delete the current elements and start the game with 16x16 squares and 40 mines
    on click intermediate $ \_ -> do
        delete headerText
        delete beginner
        delete intermediate
        delete expert
        startMinesweeper 16 16 window 40

    {- After clicking on expert mode, delete the elements and start the game with 31x31 squares and 99 mines,
       the online game has a size of 16x31 but I kept 31x31 to maintain a square shape and also make it a bit harder -}
    on click expert $ \_ -> do
        delete headerText
        delete beginner
        delete intermediate
        delete expert
        startMinesweeper 31 31 window 99