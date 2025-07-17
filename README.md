# Description
Our project provides an opportunity to generate a random map with enemies and loot on it for Dungeons and Dragons (or another game with an identical map)
# Background
Dungeons and dragons is a tabletop role-playing game for several players. One of
the main parts of the game is a map of the area where the players are moving.
Since creating the map (especially vast territories that are not important for
the plot of the game) can be problematic and consume lots of time, we decided
to implement a map generator for the DnD, including enemies appearance and
availability of useful loot.
# Features
* Generation of a map with setable parameters;
* Random spawn of loot and enemies in the room;
* Opportunity to modify the chance of enemies and loot spawn.
# The working version
Here you can see the code of the last version of project: [Link](app\Main.hs)
# Demo
Here you can see demo video: [Video](https://drive.google.com/file/d/1AjPuOCoTzIh-SZtUfmxo-wzgiyuWKrUH/view?usp=drivesdk)
## Annotation:
* **Enemies** are marked as `*`;
* **Loot** is marked as `$`;
* **Walls** are marked as `#`.

Generation method:
* Creating a two-dimensional array (Grid) where each cell is randomly defined as a wall or void according to probability;
* "Blurring" of the relief by a cellular automaton;
* Ensuring connectivity;
* Placing enemies and loot according to probability.

# Installation
1) create a folder for the project and execute the following program in terminal: "git clone https://github.com/TimurBikmetov/DnD-map-generator/tree/main"
2) Write "cabal build" in terminal
3) Write "cabal run" in terminal
4) Follow the instructions to customize parameters of the map generation 