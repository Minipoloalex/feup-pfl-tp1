%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                            %
%   This module contains the predicates used to display the  %
%   game when it is being played. Displays the board and     %
%   the pieces.                                              %
%                                                            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- ensure_loaded(library(lists)).

% number_of_digits(+N, -Size)
% Returns the number of digits of N
number_of_digits(N, Size):-
    N >= 1,
    number_of_digits(N, Size, 0).

number_of_digits(0, Size, Size):- !.
number_of_digits(N, Size, Acc):-
    NewAcc is Acc + 1,
    NewN is N // 10,
    number_of_digits(NewN, Size, NewAcc).

% print_n(+N, +String)
% Prints String N times
print_n(N, String):-
    for(_, 1, N), param(String) do write(String).

% print_empty_squares(+Number)
% Prints Number empty squares. Takes into account difference between even and odd lines.
print_empty_squares(LineNr, Number):-
    1 is LineNr mod 2,  % if odd line, add 2 spaces at the beginning
    !,
    write('  '),
    print_empty_squares(Number).

print_empty_squares(_, Number):-
    print_empty_squares(Number).

print_empty_squares(Number):-
    print_n(Number, '    ').

% existent(+Line, +Index, +Offset)
% Checks if the element at Index + Offset exists (is not -1)
existent(Line, Index, Offset):-
    RealIndex is Index + Offset,
    nth1(RealIndex, Line, Element),
    Element \= -1.

% get_second_symbol(+AuxLine, +Index, +Offset1, +Offset2, +Existent, -SecondSymbol)
% Gets the second symbol of the line, given the existent symbol

% SecondSymbol is Existent if symbols at offset exist
get_second_symbol(AuxLine, Index, Offset1, Offset2, Existent, Existent):- % dynamic SecondSymbol
    (existent(AuxLine, Index, Offset1); existent(AuxLine, Index, Offset2)),
    !.
% SecondSymbol is horizontal if symbols at offset don't exist
get_second_symbol(_, _, _, _, _, NonExistent):-
    horizontal(NonExistent).

% get_offsets(+LineNr, -Offset1, -Offset2)
% Gets the offsets for the separators
get_offsets(LineNr, 0, 1):-
    1 is LineNr mod 2,
    !.

get_offsets(_, -1, 0).

% write_separators_line(+LineSize, +LineNr, +InitialIndex, +AuxLine, +FirstCorner, +LastCorner, +FirstSymbol, +SecondSymbolExistent)
% Writes the separators line
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

% display_game(+Board-+MaxSize)
% Displays the game board
display_game(Board-MaxSize) :-
    write_board_upper_part(1, [ [] | Board ]),  % writes the board (upper and lower parts)
    write_column_numbers(MaxSize).  % writes the column numbers (for odd and even lines)

% write_column_numbers(+Size)
% Writes the column numbers
write_column_numbers(Size):-
    write('    1'),
    (for(Column, 2, Size) do 
        number_of_digits(Column, NumberSize),
        EmptySpaces is 4 - NumberSize,
        print_n(EmptySpaces, ' '),
        write(Column)),
    nl,
    write('      1'),
    OddSize is Size - 1,
    (for(Column, 2, OddSize) do 
        number_of_digits(Column, NumberSize),
        EmptySpaces is 4 - NumberSize,
        print_n(EmptySpaces, ' '),
        write(Column)),
    nl.

% write_board_upper_part(+LineNr, +Board)
% Writes the upper part of the board
% When LineNr gets to 4 (half of the board), it passes execution to write the lower part of the board
write_board_upper_part(LineNr, [AuxLine, Line | OtherLines]) :-
    filter_invalid(Line, CleanLine, MinusOneCount),

    top_right_corner(RightCorner),
    top_left_corner(LeftCorner),
    up_corner(UpCorner),
    down_corner(DownCorner),
    length(CleanLine, LineSize),

    write('  '),
    print_empty_squares(LineNr, MinusOneCount),
    write_separators_line(LineSize, LineNr, MinusOneCount, AuxLine, LeftCorner, RightCorner, DownCorner, UpCorner),

    write(LineNr), write(' '),
    print_empty_squares(LineNr, MinusOneCount),
    write_line(CleanLine),

    NextLineNr is LineNr + 1,
    % change hardcoded 4
    (LineNr =:= 4 -> write_board_lower_part(LineNr, [Line | OtherLines]); write_board_upper_part(NextLineNr, [Line | OtherLines])).

% write_board_lower_part(+LineNr, +Board)
% Writes the lower part of the board
write_board_lower_part(LineNr, [LastLine]):-
    filter_invalid(LastLine, CleanLine, MinusOneCount),

    bot_right_corner(RightCorner),
    bot_left_corner(LeftCorner),
    up_corner(UpCorner),
    down_corner(DownCorner),
    length(CleanLine, LineSize),
    
    write('  '),
    print_empty_squares(LineNr, MinusOneCount),
    write_separators_line(LineSize, LineNr, MinusOneCount, [], LeftCorner, RightCorner, UpCorner, DownCorner).

% write_board_lower_part(+LineNr, +Board)
% Writes the lower part of the board
write_board_lower_part(LineNr, [Line, AuxLine | OtherLines]):-
    filter_invalid(Line, CleanLine, MinusOneCount),

    bot_right_corner(RightCorner),
    bot_left_corner(LeftCorner),
    up_corner(UpCorner),
    down_corner(DownCorner),
    length(CleanLine, LineSize),

    write('  '),
    print_empty_squares(LineNr, MinusOneCount),
    write_separators_line(LineSize, LineNr, MinusOneCount, AuxLine, LeftCorner, RightCorner, UpCorner, DownCorner),

    NextLineNr is LineNr + 1,

    filter_invalid(AuxLine, CleanAuxLine, MinusOneCountAux),
    write(NextLineNr), write(' '),
    print_empty_squares(NextLineNr, MinusOneCountAux),
    write_line(CleanAuxLine),

    write_board_lower_part(NextLineNr, [AuxLine | OtherLines]).

% filter_invalid(+Line, -NewLine, -InvalidsNumberEachSide)
% Filters the invalid values from the line
% NewLine is the line without the invalid values
% InvalidsNumberEachSide is the number of invalid values on each side
filter_invalid(Line, ResultLine, MinusOneCount):-
    filter_invalid_first(Line, NewLine, MinusOneCount, 0), % remove the first -1's and count them
    filter_invalid_last(NewLine, ResultLine).       % get the values before -1's appear again

% filter_invalid_first(+Line, -NewLine, -InvalidsNumberEachSide, +Count)
% Filters the invalid values from the left side of the line
filter_invalid_first([-1 | T], NewLine, Result, Count):-
    !,
    Count1 is Count + 1,
    filter_invalid_first(T, NewLine, Result, Count1).

filter_invalid_first(Line, Line, Result, Result).   % Head is not -1: return the rest of the line and the count

% filter_invalid_last(+Line, -NewLine)
% Filters the invalid values from the right side of the line
filter_invalid_last([], []).    % if no -1's, copy the list
filter_invalid_last([-1 | _], []):- !.  % remove the last -1's
filter_invalid_last([H | T], [H | Rest]):-
    filter_invalid_last(T, Rest).

% write_line(+Line)
% Writes the middle part of the line
% Includes vertical separators
write_line(Line):-
    vertical(VerticalSeparator),
    write_line(Line, VerticalSeparator).

% write_line(+Line, +VerticalSeparator)
% Writes a line with vertical separators
write_line([], VerticalSeparator):- write(VerticalSeparator), nl.
write_line([Hexagon | Line], VerticalSeparator):-
    translate(Hexagon, Piece),
    write(VerticalSeparator),
    write(' '),
    write(Piece),
    write(' '),
    write_line(Line, VerticalSeparator).

%%%%% Display Helpers %%%%%

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
translate(r-1, X):- char_code(X,  9675).
translate(r-3, X):- char_code(X,  9651).
translate(r-4, X):- char_code(X,  9633).
translate(r-5, X):- char_code(X, 11040).

translate(g-1, X):- char_code(X,  9679).
translate(g-3, X):- char_code(X,  9650).
translate(g-4, X):- char_code(X,  9632).
translate(g-5, X):- char_code(X, 11039).

%%%%%%%%%%%%
