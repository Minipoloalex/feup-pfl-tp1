:- ensure_loaded(library(lists)).
board([[-1, -1, -1, -1, 0, 0, -1, -1, -1, -1],[-1, r-1, 0, r-1, 0, 0, 0, g-1, 0, g-1, -1],[0, r-4, r-3, 0, 0, 0, 0, g-3, g-4, 0],[r-1, r-3, r-5, r-4, r-1, 0, g-1, g-4, g-5, g-3, g-1],[0, r-4, r-3, 0, 0, 0, 0, g-3, g-4, 0],[-1, r-1, 0, r-1, 0, 0, 0, g-1, 0, g-1, -1],[-1, -1, -1, -1, 0, 0, -1, -1, -1, -1]]).

% print_empty_hexagons(+Number)
print_empty_hexagons(LineNr, Number):-
    1 is LineNr mod 2,  % if odd line, add 2 spaces at the beginning
    !,
    write('  '),
    print_empty_hexagons(Number).
print_empty_hexagons(_, Number):-
    print_empty_hexagons(Number).
print_empty_hexagons(Number):-
    for(_, 1, Number) do write('    ').


existent(Line, Index, Offset):-
    RealIndex is Index + Offset,
    nth1(RealIndex, Line, Element),
    Element \= -1.

get_second_symbol(AuxLine, Index, Offset1, Offset2, Existent, Existent):- % dynamic SecondSymbol
    (existent(AuxLine, Index, Offset1); existent(AuxLine, Index, Offset2)),
    !.
get_second_symbol(_, _, _, _, _, NonExistent):-
    horizontal(NonExistent).

get_offsets(LineNr, 0, 1):-
    1 is LineNr mod 2,
    !.
get_offsets(_, -1, 0).

write_separators_line(LineSize, LineNr, InitialIndex, AuxLine, FirstCorner, LastCorner, FirstSymbol, SecondSymbolExistent):- % dynamic SecondSymbol
    get_offsets(LineNr, Offset1, Offset2),
    horizontal(Horizontal),

    write(FirstCorner),
    write(Horizontal),
    Index is InitialIndex + 1,
    get_second_symbol(AuxLine, Index, Offset1, Offset2, SecondSymbolExistent, SecondSymbol),
    write(SecondSymbol),
    write(Horizontal),

    (for(Offset, 2, LineSize),
        param(FirstSymbol),
        param(SecondSymbolExistent),
        param(Horizontal),
        param(InitialIndex),
        param(Offset1),
        param(Offset2),
        param(AuxLine)
        do
            Index is InitialIndex + Offset,
            get_second_symbol(AuxLine, Index, Offset1, Offset2, SecondSymbolExistent, SecondSymbol),
            write(FirstSymbol),
            write(Horizontal),
            write(SecondSymbol),
            write(Horizontal)
    ),

    write(LastCorner),
    nl.

% write_board(+Board)
display_game(Board) :-
    write_board_upper_part(1, [ [] | Board ]).

write_board_upper_part(LineNr, [AuxLine, Line | OtherLines]) :-
    filter_invalid(Line, CleanLine, MinusOneCount),
    print_empty_hexagons(LineNr, MinusOneCount),

    top_right_corner(RightCorner),
    top_left_corner(LeftCorner),
    up_corner(UpCorner),
    down_corner(DownCorner),
    length(CleanLine, LineSize),

    write_separators_line(LineSize, LineNr, MinusOneCount, AuxLine, LeftCorner, RightCorner, DownCorner, UpCorner),

    print_empty_hexagons(LineNr, MinusOneCount),
    write_line(CleanLine),

    NextLineNr is LineNr + 1,
    % change hardcoded 4
    (LineNr =:= 4 -> write_board_lower_part(LineNr, [Line | OtherLines]); write_board_upper_part(NextLineNr, [Line | OtherLines])).


write_board_lower_part(LineNr, [LastLine]):-
    filter_invalid(LastLine, CleanLine, MinusOneCount),
    print_empty_hexagons(LineNr, MinusOneCount),
    
    bot_right_corner(RightCorner),
    bot_left_corner(LeftCorner),
    up_corner(UpCorner),
    down_corner(DownCorner),
    length(CleanLine, LineSize),

    write_separators_line(LineSize, LineNr, MinusOneCount, [], LeftCorner, RightCorner, UpCorner, DownCorner).

write_board_lower_part(LineNr, [Line, AuxLine | OtherLines]):-
    filter_invalid(Line, CleanLine, MinusOneCount),
    print_empty_hexagons(LineNr, MinusOneCount),

    bot_right_corner(RightCorner),
    bot_left_corner(LeftCorner),
    up_corner(UpCorner),
    down_corner(DownCorner),
    length(CleanLine, LineSize),

    write_separators_line(LineSize, LineNr, MinusOneCount, AuxLine, LeftCorner, RightCorner, UpCorner, DownCorner),

    NextLineNr is LineNr + 1,

    filter_invalid(AuxLine, CleanAuxLine, MinusOneCountAux),
    print_empty_hexagons(NextLineNr, MinusOneCountAux),
    write_line(CleanAuxLine),

    write_board_lower_part(NextLineNr, [AuxLine | OtherLines]).

% filter_invalid(+Line, -NewLine, -InvalidsNumberEachSide)
filter_invalid(Line, ResultLine, MinusOneCount):-
    filter_invalid_first(Line, NewLine, MinusOneCount, 0), % remove the first -1's and count them
    filter_invalid_last(NewLine, ResultLine).       % get the values before -1's appear again

filter_invalid_first([-1 | T], NewLine, Result, Count):-
    !,
    Count1 is Count + 1,
    filter_invalid_first(T, NewLine, Result, Count1).
filter_invalid_first(Line, Line, Result, Result).   % Head is not -1: return the rest of the line and the count

filter_invalid_last([], []).    % if no -1's, copy the list
filter_invalid_last([-1 | _], []):- !.  % remove the last -1's
filter_invalid_last([H | T], [H | Rest]):-
    filter_invalid_last(T, Rest).




% write_line(+Line)
write_line(Line) :-
    vertical(VerticalSeparator),
    write_line(Line, VerticalSeparator).

% write_line(+Line, +VerticalSeparator)
write_line([], VerticalSeparator) :- write(VerticalSeparator), nl.
write_line([Hexagon | Line], VerticalSeparator) :-
    translate(Hexagon, Piece),
    write(VerticalSeparator),
    write(' '),
    write(Piece),
    write(' '),
    write_line(Line, VerticalSeparator).

%%%%% Helpers %%%%%

% vertical    (-VerticalSeparator  )
% horizontal  (-HorizontalSeparator)
% right_corner(-RightCorner)
% left_corner (-LeftCorner )
% down_corner (-DownCorner )
% up_corner   (-UpCorner   )
vertical(Symbol)     :- char_code(Symbol, 9474).
horizontal(Symbol)   :- char_code(Symbol, 9472).
top_right_corner(Symbol) :- char_code(Symbol, 9488).
top_left_corner(Symbol)  :- char_code(Symbol, 9484).
bot_right_corner(Symbol) :- char_code(Symbol, 9496).
bot_left_corner(Symbol)  :- char_code(Symbol, 9492).
down_corner(Symbol)  :- char_code(Symbol, 9516).
up_corner(Symbol)    :- char_code(Symbol, 9524).

% translate(+CellPiece, -WritableSymbol)
translate(0, ' ').
translate(r-1, X) :- char_code(X,  9675).
translate(r-3, X) :- char_code(X,  9651).
translate(r-4, X) :- char_code(X,  9633).
translate(r-5, X) :- char_code(X, 11040).

translate(g-1, X) :- char_code(X,  9679).
translate(g-3, X) :- char_code(X,  9650).
translate(g-4, X) :- char_code(X,  9632).
translate(g-5, X) :- char_code(X, 11039).

%%%%%%%%%%%%


test:-
    board(_B),
    display_game(_B).
