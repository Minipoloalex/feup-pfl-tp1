:- ensure_loaded(library(lists)).

% print_board(Board)
pX(Line, Spaces) :-
    spaces(Spaces),
    print_line(Line),
    nl.

print_line([]).
print_line([-1 | Line]) :-
    print_line(Line).
print_line([X | Line]) :-
    translate(X, Y),
    write(Y), write(' '),
    print_line(Line).

spaces(0).
spaces(N) :-
    N > 0,
    NextN is N - 1,
    write(' '),
    spaces(NextN).

print_board([L1, L2, L3, L4, L5, L6, L7]) :-
    p1(L1),
    p2(L2),
    p3(L3),
    p4(L4),
    p3(L5),
    p2(L6),
    p1(L7).

p1(L) :- pX(L, 9).
p2(L) :- pX(L, 2).
p3(L) :- pX(L, 1).
p4(L) :- pX(L, 0).

% translate(+InternalRepresentation, -CharOutput)
translate(0, ' ').
translate(r-1, '1').
translate(r-3, '3').
translate(r-4, '4').
translate(r-5, '5').
translate(g-1, 'A').
translate(g-3, 'C').
translate(g-4, 'D').
translate(g-5, 'E').


% subtract(+List1, +List2, -ResultingList)
% Resulting List = List1 - List2 (i.e. elements of List1 not in List2)
subtract([], _, []) :- !.
subtract([A|T], B, R) :-
    memberchk(A, B), !,
    subtract(T, B, R).
subtract([A|T], B, [A|R]) :-
    subtract(T, B, R).

% other_player(?Player, ?OtherPlayer)
other_player(r, g).
other_player(g, r).

% valid_piece(+Board, ?X, ?Y, ?Piece)
% need to return every possible piece (for the computer)
valid_piece(Board, X, Y, Player-Piece):-
    valid_position(Board, X, Y, Player-Piece),
    nth1(X, Board, Line),
    nth1(Y, Line, Player-Piece).    % if Piece is -1 or 0, then this fails (no player)


% valid_position(+Board, X, Y, -Value)
get_value(Board, X, Y, Value):-
    nth1(X, Board, Line),
    nth1(Y, Line, Value).
valid_position(Board, X, Y, Piece):-
    get_value(Board, X, Y, Piece),
    Piece \= -1.

% hardcoded board to test things
board([[-1, -1, -1, -1, 0, 0, -1, -1, -1, -1],[-1, r-1, 0, r-1, 0, 0, 0, g-1, 0, g-1, -1],[0, r-4, r-3, 0, 0, 0, 0, g-3, g-4, 0],[r-1, r-3, r-5, r-4, r-1, 0, g-1, g-4, g-5, g-3, g-1],[0, r-4, r-3, 0, 0, 0, 0, g-3, g-4, 0],[-1, r-1, 0, r-1, 0, 0, 0, g-1, 0, g-1, -1],[-1, -1, -1, -1, 0, 0, -1, -1, -1, -1]]).

% get_resulting_piece(+Attacker, +Attacked, ?ResultingPiece)
get_resulting_piece(Player-_, Player-_, _):-
    !, fail.    % assure that the same player is not attacking itself
get_resulting_piece(Piece, 0, Piece).
get_resulting_piece(P1-Attacker, _-Attacked, P1-Attacker):-
    Attacker =< Attacked,
    !.
get_resulting_piece(_-3, _-1, 0).
get_resulting_piece(_-4, _-3, 0).


% % valid_move_to(Player-Board, AttackerPiece, AttackedPiece)
% valid_move_to(_, _, 0):-!.
% valid_move_to(Player-Board, AttackerPiece, AttackedPiece):-
%     other_player(Player, OtherPlayer),
%     get_resulting_piece(Player-AttackerPiece, OtherPlayer-AttackedPiece, ResPiece).


% % replace(+Board, +Xi, +Yi, +Xf, +Yf, -NewBoard)
% replace(Player-Board, Xi, Yi, Xf, Yf, NB):-
%     valid_piece(Board, X, Y, AttackerPiece),

%     other_player(Player, Other),
%     valid_position(Board, Xf, Yf, MoveTo),
%     % TODO: validate distance between the positions, distance must be between 1 and ...

%     get_resulting_piece(Player-AttackerPiece, MoveTo, Other, ResultingPiece),
%     replace_matrix_value(Board, Xf, Yf, ResultingPiece, NB).

% move(+Board, +Player-AttackerPiece, ?Xf-Yf, -NewGameState)
% move(B, P-AttackerPiece, Xf-Yf, NP-NB):-  % the piece to move should already be validated
%     other_player(Player, NP),
%     valid_move_to(Board, Xi, Yi, Xf, Yf, MoveTo, Player-AttackerPiece)
%     valid_position(Board, Xf, Yf, MoveTo),
%     % TODO: validate distance between the positions, distance must be between 1 and ...

%     get_resulting_piece(Player-AttackerPiece, MoveTo, Other, ResultingPiece),
%     replace_matrix_value(Board, Xf, Yf, ResultingPiece, NB).
    
%     other_player(P, NP).


% % want list of possible moves for this piece
% get_valid_moves(MaxMoves, Board, Player, Xi, Yi, ListMoves):-
%     get_valid_moves(MaxMoves, Board, Player, [Xi-Yi-0], [], ListMoves).

adjacent_diff(Row, [(0, -1), (0, 1), (-1, -1), (-1, 0), (1, -1), (1, 0)]):- % row is even
    0 is Row mod 2,
    !.
adjacent_diff(_, [(0, -1), (0, 1), (-1, 1), (-1, 0), (1, 1), (1, 0)]).    % row is odd

% get_adjacent(+Xi, +Yi, +AdjacentDiff, +Board, -ListAdjacent)
get_adjacent(Xi, Yi, Player-Piece, Board, ListAdjacent):-
    % notrace,
    adjacent_diff(Xi, Diff),
    get_adjacent(Xi, Yi, Player-Piece, Diff, Board, ListAdjacent).
get_adjacent(_, _, _, [], _, []).
get_adjacent(Xi, Yi, Player-Piece, [(XDiff, YDiff) | Rest], Board, [(Xf, Yf) | RecursiveResult]):-
    Xf is Xi + XDiff,
    Yf is Yi + YDiff,
    valid_position(Board, Xf, Yf, Value),
    get_resulting_piece(Player-Piece, Value, _),
    !,
    get_adjacent(Xi, Yi, Player-Piece, Rest, Board, RecursiveResult).
get_adjacent(Xi, Yi, Player-Piece, [(_, _) | Rest], Board, RecursiveResult):-
    % if cannot move to it, then just ignore it
    get_adjacent(Xi, Yi, Player-Piece, Rest, Board, RecursiveResult).


% valid_moves(+Board, +Player, -ListOfMoves)
valid_moves(Board, Player, ListOfMoves):-
    % findall((X, Y, Piece), valid_piece(Board, X, Y, Player-Piece), ListOfPieces),
    findall((Xi,Yi,Xf,Yf), (
        valid_piece(Board, Xi, Yi, Player-Piece),
        get_valid_moves_bfs((Xi,Yi), Piece, Player-Board, Moves),
        member((Xf,Yf), Moves)
    ), ListOfMoves).

% associate_distance_to_move(+Moves, Distance, -ResultingMoves)
associate_distance_to_move([], _, []).
associate_distance_to_move([(X, Y) | T], Dist, [(X, Y, Dist) | R]):-
    associate_distance_to_move(T, Dist, R).

/*
Want valid moves of a piece
Need
- max moves of the piece
- current position of the piece
- current board
- current player (gamestate)
- list of visited positions (to avoid cycles)
- list of valid moves (to return)
*/
% filter_empty(+Moves, +Board, -MovesToEmptySpace)
filter_empty([], _, []).
filter_empty([(X,Y) | T], Board, [(X,Y) | TailResult]):-
    get_value(Board, X, Y, 0),  % if empty, then add to result
    !,
    filter_empty(T, Board, TailResult).
filter_empty([_ | T], Board, Result):-
    % not an empty space, so ignore it
    filter_empty(T, Board, Result).

get_valid_moves_bfs((Xi, Yi), Piece, Player-Board, ListMoves):-
    get_valid_moves_bfs(Piece, Player-Board, [(Xi, Yi, 0)], [(Xi,Yi)], ListMoves).

get_valid_moves_bfs(_, _, [], _, []).
get_valid_moves_bfs(MaxMoves, _, [(_,_,MaxMoves) | _], _, []):-!. % q.front()'s distance has reached max, so stop

get_valid_moves_bfs(MaxMoves, Player-Board, [(X,Y,Dist) | RestQueue], Visited, ListMoves):-
    Dist < MaxMoves,
    NewDist is Dist + 1,
    get_adjacent(X, Y, Player-MaxMoves, Board, Moves),
    % trace,
    subtract(Moves, Visited, MovesNotVisited),  % remove visited positions
    
    filter_empty(MovesNotVisited, Board, MovesToEmptySpace),   % get moves to empty spaces
    associate_distance_to_move(MovesToEmptySpace, NewDist, MovesToEmptyWithDist),  % X-Y to X-Y-NewDist
    
    append(MovesNotVisited, RestMoves, ListMoves),    % RestMoves is the recursive result
    append(MovesNotVisited, Visited, NewVisited),
    append(RestQueue, MovesToEmptyWithDist, NewQueue),

    get_valid_moves_bfs(MaxMoves, Player-Board, NewQueue, NewVisited, RestMoves).


% % Given by ChatGPT
% % Function to replace a value in a matrix at row X and column Y
% % replace_matrix_value(+Matrix, +X, +Y, +NewValue, -NewMatrix)
% replace_matrix_value(Matrix, X, Y, NewValue, NewMatrix) :-
%     replace_matrix_value(Matrix, X, Y, NewValue, 1, 1, NewMatrix).

% % Base case: If we reach the desired row X, replace the element at column Y
% replace_matrix_value([], _, _, [], _, _).
% replace_matrix_value([Row|Rest], X, Y, [NewRow|NewRest], X, Y, [NewRow|NewRest1]) :-
%     replace_element(Row, Y, NewValue, NewRow),
%     X1 is X + 1,
%     replace_matrix_value(Rest, X1, Y, NewRest, X1, Y, NewRest1).

% % Recursive case: Keep traversing rows until we reach row X
% replace_matrix_value([Row|Rest], X, Y, [Row|NewRest], CurrX, Y, [Row|NewRest1]) :-
%     CurrX < X,
%     CurrX1 is CurrX + 1,
%     replace_matrix_value(Rest, X, Y, NewRest, CurrX1, Y, NewRest1).

% % Helper function to replace an element at a specific position in a list
% % replace_element(+List, +Index, +NewValue, -NewList)
% replace_element([_|T], 1, NewValue, [NewValue|T]).
% replace_element([H|T], Index, NewValue, [H|NewT]) :-
%     Index > 1,
%     Index1 is Index - 1,
%     replace_element(T, Index1, NewValue, NewT).
