# PFL Project 1 - Tactigon
The game is called `Tactigon` and it can be found and explored in [tactigongame.com](https://tactigongame.com/)

## 1. Group Tactigon_5
<!--TODO: add contribution percentages -->
| Name | Number | Contribution (%) |
|:-:|:-:|:-:|
| Félix Marcial Alves Martins | 202108837 | Prolog expert |
| Marco Filipe Gonçalves Vilas Boas | 202108774 | Prolog expert |

## 2. Installation and execution
The only requirement to run our game, besides SICStus Prolog, is to run the program in the terminal and not the GUI. This is because some of the characters we use are not monospaced in the GUI, which makes the board not be displayed correctly.

## 3. Game description

### 3.1 Pieces
Each player starts with a pentagon, 3 squares, 3 triangles and 6 circles.
The pieces move according to their number of sides and in any direction (i.e. movement does not need to be in a straight line). Pieces cannot jump other pieces.

- Circle: Moves 1 space
- Triangle: Moves 3 spaces
- Square: Moves 4 spaces
- Pentagon: Moves 5 spaces

Attacks from different pieces have 3 possible outcomes:
- The attacked piece gets captured
- Both the attacker and the attacked get captured
- The attacker cannot actually attack the other piece (this is considered an invalid move)

Here we present in tex the interactions between all the pieces in relation to how they can attack each other, and what outcomes arise from such attacks.
| Attacking piece | Circle | Triangle | Square | Pentagon |
|:-:|:-:|:-:|:-:|:-:|
| Circle | Captures | Captures | Captures | Captures |
| Triangle | Both captured | Captures | Captures | Captures |
| Square | Cannot capture | Both captured | Captures | Captures |
| Pentagon | Cannot capture | Cannot capture | Cannot capture | Captures |

### 3.2 Turns
Each player takes turns in moving one of their pieces. Combat ends the turn of the player.

### 3.3 Win condition
There are 2 win conditions.
If a player captures the enemy's pentagon, he wins the game. Secondly, if a player can put any two pieces on both golden squares at the same time, and the other player cannot remove them on his turn, the first player wins.

### 3.4 Board
The board is presented here, in a starting position:

![Alt text](images/starting_board.png)

### 3.5 Advanced game rules:
In the game, 2 advanced rules are presented, which we implement.
- Moving from a golden tile: a piece that starts on a golden tile is allowed to move one more space than usual.
- Square movement: a square can jump over any pieces, except enemy squares.


## 4. Game logic

### 4.1 Internal Game State Representation
<!--
how game state is represented: Player-Board (board is a list of lists with -1s, 0s for empty, r for player, 1 for circle, ...)
include representation of initial, intermediate and final game states
-->
In our project, the game is represented by a board and the player whose turn it is to play. The player is either 'r' or 'g', which represents red or green. The board is represented by a list of lists, where each list represents a row of the board. Each element of a row is either an invalid position, an empty space or a piece from a certain player. 

Invalid positions are represented as -1, and they exist to facilitate the correlation between the user input (row X and column Y), to a certain value from the board.Empty spaces are represented by 0, and pieces are represented by the player's color and the piece's number of sides.
Examples of pieces:
- r-1 represents a circle (1) from the red player (r)
- g-3 represents a triangle (3) from the green player

<!--
TODO: insert our board representation of the initial, intermediate and final game states
-->



### 4.2 Game State Visualization
We provide a way for users to make the board have a different size. However, the board does not grow in all directions. The user can input a padding size which will only be used to add padding to the board on both sides.
<!--TODO: add something demonstrating the padding-->
The display_game predicate only makes the assumption that the board has 7 lines, and does not assume anything regarding the columns, since their size can be different.
The display_game predicate displays the complete board and also the labels for the rows and columns.
The labels for the columns depend on the user input for the padding.

The menu system in our game allows for a user to go back in multiple occasions (selecting a move, selecting the computer difficulty, padding size, etc.). However, after starting a game, there is no way to go back, except for finishing the game. We did not consider this a problem because the user can quickly finish the game.


<!--
Interaction with the menu, input from the user, initial_state(SIZE, GameState) -> explain our size
display_game -> verifies if there is something in the bottom or upper part of our location to tell if we should have a straight horizontal line or a connection upwards or downwards (e.g. ┴)
-->

### 4.3 Move Validation and Execution
A move is composed of a starting position and an ending position.
As the user inputs the first position (row and column), we check if it is a valid piece for the current player. After, when the user specifies the ending position, we simply call move, which does verifies if the complete move is valid (valid piece and valid ending position), and executes the move if it is.

Executing the move itself is simply changing the value of the starting position to 0 (empty), and the value of the ending position to the resulting piece, which is not necessarily equal to the piece moved.

### 4.4 List of Valid Moves

### 4.5 End of Game

### 4.6 Game State Evaluation

### 4.7 Computer Plays


## 5. Conclusion


## Bibliography
- [Tactigon Game](https://tactigongame.com/)
