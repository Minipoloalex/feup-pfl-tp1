/*
GameState:
Player-Board

user writes line X, column Y
board represented horizontally
Board N = 0
[
    [-1, -1, -1, -1, 0, 0, -1, -1, -1, -1],
    [-1, r-1, 0, r-1, 0, 0, 0, g-1, 0, g-1, -1],
    [0, r-4, r-3, 0, 0, 0, 0, g-3, g-4, 0]
    [r-1, r-3, r-5, r-4, r-1, 0, g-1, g-4, g-5, g-3, g-1]
    [0, r-4, r-3, 0, 0, 0, 0, g-3, g-4, 0],
    [-1, r-1, 0, r-1, 0, 0, 0, g-1, 0, g-1, -1],
    [-1, -1, -1, -1, 0, 0, -1, -1, -1, -1]
]
*/

:- use_module(library(lists)).

% replicate(+Amount, +Elem, ?List)
replicate(0, _, []):-!.
replicate(Amount, Elem, [Elem | T]):-
    Amount1 is Amount - 1,
    replicate(Amount1, Elem, T).


transform(empty, 0).
transform(circle, 1).
transform(triangle, 3).
transform(square, 4).
transform(pentagon, 5).
transform(red, r).
transform(green, g).

% get_pieces(+LineNr, +Colo, -Pieces)
% switch case with default value empty list
get_pieces(0, Color, [Color-C, E, Color-C]):-
    !,
    transform(empty, E),
    transform(circle, C).
get_pieces(1, Color, [E, Color-S, Color-T, E]):-
    !,
    transform(empty, E),
    transform(triangle, T),
    transform(square, S).
get_pieces(2, Color, [Color-C, Color-T, Color-P, Color-S, Color-C]):-
    !,
    transform(circle, C),
    transform(triangle, T),
    transform(square, S),
    transform(pentagon, P).
get_pieces(3, Color, [E, Color-S, Color-T, E]):-
    !,
    transform(empty, E),
    transform(triangle, T),
    transform(square, S).
get_pieces(4, Color, [Color-C, E, Color-C]):-
    !,
    transform(empty, E),
    transform(circle, C).
get_pieces(_, _, []).

change_player(red, green).
change_player(green, red).

get_line(LineNr, PaddingSize, MiddleSize, Line):-
    transform(red, R),
    transform(green, G),
    transform(empty, E),
    get_pieces(LineNr, R, RedPieces),
    get_pieces(LineNr, G, AuxGreenPieces),
    reverse(AuxGreenPieces, GreenPieces),
    
    minus_ones_each_side(LineNr, PaddingSize, NumberMinusOnes),
    replicate(NumberMinusOnes, E, MinusOnes),

    replicate(PaddingSize, E, Padding),
    replicate(MiddleSize, E, Middle),
    % Order in the board is:
    % Padding, RedPieces, Padding, Middle, Padding, GreenPieces, Padding
    append(Padding, MinusOnes, End1),
    append(GreenPieces, End1, End2),
    append(Padding, End2, End3),
    append(Middle, End3, End4),
    append(Padding, End4, End5),
    append(RedPieces, End5, End6),
    append(Padding, End6, End7),
    append(MinusOnes, End7, Line).

% get_lines(+PaddingSize, -Lines)
get_lines(PaddingSize, [[0,0] | Lines]):-
    get_lines(0, PaddingSize, Lines).
get_lines(5, _, [[0, 0]]):-!. % there are only 5 lines as of now
get_lines(LineNr, PaddingSize, [Line | RecRes]):-
    MiddleSize is 1 + abs(LineNr - 2),
    get_line(LineNr, PaddingSize, MiddleSize, Line),

    NextLineNr is LineNr + 1,
    get_lines(NextLineNr, PaddingSize, RecRes).


% initial_state(+Size, -GameState)
initial_state(PaddingSize, GameState):-
    PaddingSize >= 0,
    get_lines(PaddingSize, GameState),
    write(GameState), nl.

% minus_ones_each_side(+LineNr, +PaddingSize, -NumberMinusOnes)
% Gets the number of minus ones a line has on each side
minus_ones_each_side(1, PS, N):-
    N is PS * 2 + 4.
minus_ones_each_side(2, _, 1).
minus_ones_each_side(3, _, 0).
minus_ones_each_side(4, _, 0).
minus_ones_each_side(5, _, 0).
minus_ones_each_side(6, _, 1).
minus_ones_each_side(7, PS, N):-
    N is PS * 2 + 4.
