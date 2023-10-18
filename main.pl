play:-
    initial_state(2, _).
/*
GameState:
Player-Board

user writes line X, column Y
board represented horizontally
Board N = 5
N = 5
[
                [0, 0],
      [1, 0, 1, 0, 0, 0, 0, 0, 0],
     [0, 4, 3, 0, 0, 0, 0, 0, 0, 0]
    [1, 3, 5, 4, 1, 0, 0, 0, 0, 0, 0]
     [0, 4, 3, 0, 0, 0, 0, 0, 0, 0],
      [1, 0, 1, 0, 0, 0, 0, 0, 0],
                [0, 0]
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

% % change_value_at_index(+OldLine, +Index, +Element, -NewLine)
% change_value_at_index(OldLine, Index, Element, NewLine):-
%     Index >= 0,
%     change_value_at_index_aux(OldLine, Index, Element, NewLine).
% change_value_at_index_aux([_ | T], 0, Element, [Element | T]):-!.
% change_value_at_index_aux([OldValue | T], Index, Element, [OldValue | RestLine]):-
%     NewIndex is Index - 1,
%     change_value_at_index_aux(T, NewIndex, Element, RestLine).


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

get_line(LineNr, PaddingSize, MiddleSize, FinalLine):-
    transform(red, R),
    transform(green, G),
    transform(empty, E),
    get_pieces(LineNr, R, RedPieces),
    get_pieces(LineNr, G, AuxGreenPieces),
    reverse(AuxGreenPieces, GreenPieces),
    
    replicate(PaddingSize, E, Padding),
    replicate(MiddleSize, E, Middle),
    % Order in the board is:
    % Padding, RedPieces, Padding, Middle, Padding, GreenPieces, Padding
    append(GreenPieces, Padding, End1),
    append(Padding, End1, End2),
    append(Middle, End2, End3),
    append(Padding, End3, End4),
    append(RedPieces, End4, End5),
    append(Padding, End5, FinalLine).

% get_lines(+PaddingSize, -Lines)
get_lines(PaddingSize, [[0,0] | Lines]):-
    get_lines(0, PaddingSize, Lines).
get_lines(5, _, [[0, 0]]):-!. % there are only 5 lines as of now
get_lines(LineNr, PaddingSize, [Line | RecRes]):-
    MiddleSize is 1 + abs(LineNr - 2),
    get_line(LineNr, PaddingSize, MiddleSize, Line),

    NextLineNr is LineNr + 1,
    get_lines(NextLineNr, PaddingSize, RecRes).


%% initial_state(+Size, -GameState)
initial_state(PaddingSize, GameState):-
    PaddingSize >= 0,
    get_lines(PaddingSize, GameState),
    write(GameState), nl.
