:- ensure_loaded(library(lists)).

% replicate(+Amount, +Elem, ?List)
replicate(0, _, []):-!.
replicate(Amount, Elem, [Elem | T]):-
    Amount1 is Amount - 1,
    replicate(Amount1, Elem, T).



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

% get_pieces(+LineNr, +Colo, -Pieces)
% switch case with default value empty list (no pieces)
get_pieces(2, Color, [Color-1, 0, Color-1]):-!. % [circle, empty, circle]
get_pieces(3, Color, [0, Color-4, Color-3, 0]):-!. % [empty, square, triangle, empty]
get_pieces(4, Color, [Color-1, Color-3, Color-5, Color-4, Color-1]):-!.
get_pieces(5, Color, [0, Color-4, Color-3, 0]):-!.
get_pieces(6, Color, [Color-1, 0, Color-1]):-!.
get_pieces(_, _, []).


get_line(LineNr, PaddingSize, Line):-
    (LineNr =:= 1; LineNr =:= 7),   % There are 7 lines
    !,
    minus_ones_each_side(7, PaddingSize, NumberMinusOnes),
    replicate(NumberMinusOnes, -1, MinusOnes),
    append([0, 0], MinusOnes, End),
    append(MinusOnes, End, Line).

get_line(LineNr, PaddingSize, Line):-
    MiddleSize is 1 + abs(LineNr - 4),
    Empty = 0,

    get_pieces(LineNr, r, RedPieces),
    get_pieces(LineNr, g, AuxGreenPieces),
    reverse(AuxGreenPieces, GreenPieces),
    
    minus_ones_each_side(LineNr, PaddingSize, NumberMinusOnes),
    replicate(NumberMinusOnes, -1, MinusOnes),

    replicate(PaddingSize, Empty, Padding),
    replicate(MiddleSize, Empty, Middle),
    % Order in the board is:
    % MinusOnes, Padding, RedPieces, Padding, Middle, Padding, GreenPieces, Padding, MinusOnes
    append(Padding, MinusOnes, End1),
    append(GreenPieces, End1, End2),
    append(Padding, End2, End3),
    append(Middle, End3, End4),
    append(Padding, End4, End5),
    append(RedPieces, End5, End6),
    append(Padding, End6, End7),
    append(MinusOnes, End7, Line).

% get_lines(+PaddingSize, -Lines)
get_lines(PaddingSize, Lines):-
    get_lines(1, PaddingSize, Lines).

get_lines(8, _, []):-!.    % 7 lines only
get_lines(LineNr, PaddingSize, [Line | RecRes]):-
    get_line(LineNr, PaddingSize, Line),

    NextLineNr is LineNr + 1,
    get_lines(NextLineNr, PaddingSize, RecRes).

% initial_state(+PaddingSize, -GameState)
initial_state(PaddingSize, GameState):-
    PaddingSize >= 0,
    get_lines(PaddingSize, GameState).
