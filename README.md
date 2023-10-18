# PFL Project 1 - Tactigon
The game is called `Tactigon` and it can be found and explored in [tactigon](https://tactigongame.com/)

## 1. Group Tactigon_5
<!--TODO: add contribution percentages -->
| Name | Number | Contribution (%) |
|:-:|:-:|:-:|
| Félix Marcial Alves Martins | 202108837 | Prolog expert |
| Marco Filipe Gonçalves Vilas Boas | 202108774 | Prolog expert |


## 2. Game description

### 2.1 Pieces
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

### 2.2 Turns
Each player takes turns in moving one of their pieces. Combat ends the turn of the player.

### 2.3 Win condition
There are 2 win conditions.
If a player captures the enemy's pentagon, he wins the game. Secondly, if a player can put any two pieces on both golden squares at the same time, and the other player cannot remove them on his turn, the first player wins.

### 2.4 Board
The board is presented here, in a starting position:
<!--TODO:
probably a screenshot of our game
-->

### 2.5 Advanced game rules:
<!--TODO:
specify these if we implement them
probably very easy to implement after the basic game is done
-->


