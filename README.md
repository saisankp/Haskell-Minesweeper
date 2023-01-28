# Haskell-Minesweeper

This stack project in Haskell contains an auto-playing implementation of the game minesweeper which uses Threepenny for the user interface. Minesweeper is a single-player logic puzzle; the [basic rules](http://www.freeminesweeper.org/help/minehelpinstructions.html) are simple (wikipedia has a [good description](http://en.wikipedia.org/wiki/Minesweeper_%28video_game%29) as well). The program distributes mines randomly around the grid, allowing the user to uncover and flag mines interactively, while also detecting endgame conditions. It has an a 'play move' button which *attempts* to play a good, safe, move. [Minesweeper is known to be NP-complete](http://web.mat.bham.ac.uk/R.W.Kaye/minesw/ordmsw.htm), which means that a well written and efficient solver is difficult but my autoplayer uses 2 advanced tactics which plays a move automatically if there is an unambiguously safe move available and if a safe move is not available the program will play the least dangerous move available. 

## Demo


https://user-images.githubusercontent.com/34750736/214979327-d03715e2-03f9-4d6f-9039-86144d27e991.mp4



## How to run
To run the project, simply go to the top level directory (not src) of this project folder and run these 2 commands:


```
stack build
stack exec CSU44012Assignment2-exe
```

The Minesweeper implementation will be hosted at http://127.0.0.1:8023 on your local machine.

## Documentation
Documentation can be found in two ways:
1. In-depth comments dispersed along this project to explain what each function is doing, along with more comments to explain trickier blocks of code.
3. A PDF file called **"Functional-Minesweeper-Assignment.pdf"** which is in this repo and outlines the high level design choices I made, and also a reflection on the process of designing the program (containing how suitable Haskell was as a language for tackling each phase of the project, what my experience was of the software development process including things like testing and debugging, and what features of the language proved useful). 
