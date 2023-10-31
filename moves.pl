:- ensure_loaded(library(lists)).

% hardcoded board to test things
% board([[-1, -1, -1, -1, 0, 0, -1, -1, -1, -1],[-1, r-1, 0, r-1, 0, 0, 0, g-1, 0, g-1, -1],[0, r-4, r-3, 0, 0, 0, 0, g-3, g-4, 0],[r-1, r-3, r-5, r-4, r-1, 0, g-1, g-4, g-5, g-3, g-1],[0, r-4, r-3, 0, 0, 0, 0, g-3, g-4, 0],[-1, r-1, 0, r-1, 0, 0, 0, g-1, 0, g-1, -1],[-1, -1, -1, -1, 0, 0, -1, -1, -1, -1]]).

% valid_moves(+Board, +Player, -ListOfMoves)
valid_moves(Board, Player, ListOfMoves):-
    findall((Xi,Yi,Xf,Yf), (
        valid_piece(Board, Xi, Yi, Player-Piece),
        get_valid_moves_bfs((Xi,Yi), Piece, Player-Board, Moves),
        member((Xf,Yf), Moves)
    ), ListOfMoves).

% move(+Board, +Player-AttackerPiece, ?Xf-Yf, -NewGameState)
move(Player-Board, (Xi, Yi, Xf, Yf), NP-NB):-  % the piece to move should already be validated
    other_player(Player, NP),
    valid_piece(Board, Xi, Yi, Player-AttackerPiece),
    get_valid_moves_bfs((Xi,Yi), AttackerPiece, Player-Board, Moves),
    memberchk((Xf,Yf), Moves),
    get_value(Board, Xf, Yf, Value),
    get_resulting_piece(Player-AttackerPiece, Value, ResultingPiece),
    execute_move(Board, (Xi, Yi, Xf, Yf), ResultingPiece, NB).

% execute_move(+Board, +Move, -NewBoard)
execute_move(Board, (Xi, Yi, Xf, Yf), ResultingPiece, NB):-
    replace_matrix_value(Board, Xi, Yi, 0, IntermediaryBoard),
    replace_matrix_value(IntermediaryBoard, Xf, Yf, ResultingPiece, NB).


% other_player(?Player, ?OtherPlayer)
other_player(r, g).
other_player(g, r).

% subtract(+List1, +List2, -ResultingList)
% Resulting List = List1 - List2 (i.e. elements of List1 not in List2)
subtract([], _, []) :- !.
subtract([A|T], B, R) :-
    memberchk(A, B), !,
    subtract(T, B, R).
subtract([A|T], B, [A|R]) :-
    subtract(T, B, R).

% valid_piece(+Board, ?X, ?Y, ?Piece)
% Unifies each possible piece (for the computer)
valid_piece(Board, X, Y, Player-Piece):-
    valid_position(Board, X, Y, Player-Piece),
    nth1(X, Board, Line),
    nth1(Y, Line, Player-Piece).    % if Piece is -1 or 0, then this fails (no player)

% get_value(+Board, +X, +Y, -Value)
get_value(Board, X, Y, Value):-
    nth1(X, Board, Line),
    nth1(Y, Line, Value).

% valid_position(+Board, X, Y, -Value)
valid_position(Board, X, Y, Piece):-
    get_value(Board, X, Y, Piece),
    Piece \= -1.   % -1 is an invalid position

% get_resulting_piece(+Attacker, +Attacked, ?ResultingPiece)
get_resulting_piece(Player-_, Player-_, _):-
    !, fail.    % assure that the same player is not attacking itself
get_resulting_piece(Piece, 0, Piece).
get_resulting_piece(P1-Attacker, _-Attacked, P1-Attacker):-
    Attacker =< Attacked,
    !.
get_resulting_piece(_-3, _-1, 0).
get_resulting_piece(_-4, _-3, 0).


adjacent_diff(Row, [(0, -1), (0, 1), (-1, -1), (-1, 0), (1, -1), (1, 0)]):- % row is even
    0 is Row mod 2,
    !.
adjacent_diff(_, [(0, -1), (0, 1), (-1, 1), (-1, 0), (1, 1), (1, 0)]).    % row is odd

% get_adjacent(+Xi, +Yi, +AdjacentDiff, +Board, -ListAdjacent)
get_adjacent(Xi, Yi, Player-Piece, Board, ListAdjacent):-
    adjacent_diff(Xi, Diff),
    get_adjacent(Xi, Yi, Player-Piece, Diff, Board, ListAdjacent).
get_adjacent(_, _, _, [], _, []).
get_adjacent(Xi, Yi, Player-Piece, [(XDiff, YDiff) | Rest], Board, [(Xf, Yf) | RecursiveResult]):-
    Xf is Xi + XDiff,
    Yf is Yi + YDiff,
    valid_position(Board, Xf, Yf, _),
    !,
    get_adjacent(Xi, Yi, Player-Piece, Rest, Board, RecursiveResult).
get_adjacent(Xi, Yi, Player-Piece, [(_, _) | Rest], Board, RecursiveResult):-
    % if cannot move to it, then just ignore it
    get_adjacent(Xi, Yi, Player-Piece, Rest, Board, RecursiveResult).

% associate_distance_to_move(+Moves, Distance, -ResultingMoves)
% each element (X, Y) transformed to (X, Y, Dist)
associate_distance_to_move([], _, []).
associate_distance_to_move([(X, Y) | T], Dist, [(X, Y, Dist) | R]):-
    associate_distance_to_move(T, Dist, R).


% filter_possible_moves(+AttackerPiece, +Moves, +Board, -PossibleMoves)
% returns the possible moves. These are moves that end up in empty spaces or in valid enemy pieces considering the attacker piece
filter_possible_moves(_, [], _, []).
filter_possible_moves(Player-Piece, [(X, Y) | T], Board, [(X, Y) | PossibleMoves]):-
    get_value(Board, X, Y, Value),
    get_resulting_piece(Player-Piece, Value, _),
    !,
    filter_possible_moves(Player-Piece, T, Board, PossibleMoves).

filter_possible_moves(Player-Piece, [(_, _) | T], Board, PossibleMoves):-
    filter_possible_moves(Player-Piece, T, Board, PossibleMoves).

% This filters the moves that end up on enemy squares
% This is used to ensure that a square cannot jump over an enemy square
filter_all_except_enemy_squares(_, [], _, []).
filter_all_except_enemy_squares(Player-Piece, [(X, Y) | T], Board, ResultMoves):-
    other_player(Player, Other),
    get_value(Board, X, Y, Other-Piece),    % if move to enemy square then do not add to result
    !,
    filter_all_except_enemy_squares(Player-Piece, T, Board, ResultMoves).
filter_all_except_enemy_squares(Player-Piece, [(X, Y) | T], Board, [(X, Y) | ResultMoves]):-
    filter_all_except_enemy_squares(Player-Piece, T, Board, ResultMoves).

% filter_empty(+Moves, +Board, -MovesToEmptySpace)
% returns the moves that end up in empty spaces (useful for the bfs)
filter_empty([], _, []).
filter_empty([(X,Y) | T], Board, [(X,Y) | TailResult]):-
    get_value(Board, X, Y, 0),  % if empty, then add to result
    !,
    filter_empty(T, Board, TailResult).
filter_empty([_ | T], Board, Result):-
    % not an empty space, so ignore it
    filter_empty(T, Board, Result).

% if piece is square (4), then can jump over other pieces -> different bfs
get_valid_moves_bfs((Xi, Yi), 4, Player-Board, ListMoves):-
    !,
    % if is golden square, MaxMoves is Piece + 1, else MaxMoves is Piece
    get_valid_moves_bfs_square(4, 4, Player-Board, [(Xi, Yi, 0)], [(Xi,Yi)], ListMoves).
get_valid_moves_bfs((Xi, Yi), Piece, Player-Board, ListMoves):-
    % if is golden square, MaxMoves is Piece + 1, else MaxMoves is Piece
    get_valid_moves_bfs(Piece, Piece, Player-Board, [(Xi, Yi, 0)], [(Xi,Yi)], ListMoves).

get_valid_moves_bfs(_, _, _, [], _, []).
get_valid_moves_bfs(MaxMoves, _, _, [(_,_,MaxMoves) | _], _, []):-!. % q.front()'s distance has reached max, so stop

get_valid_moves_bfs(MaxMoves, Piece, Player-Board, [(X,Y,Dist) | RestQueue], Visited, ListMoves):-
    Dist < MaxMoves,
    NewDist is Dist + 1,
    get_adjacent(X, Y, Player-MaxMoves, Board, ToFilterMoves),  % includes moves to pieces that we cannot attack
    % filters moves: only to empty spaces or to pieces that we can attack
    filter_possible_moves(Player-Piece, ToFilterMoves, Board, Moves),

    subtract(Moves, Visited, MovesNotVisited),  % remove visited positions

    filter_empty(MovesNotVisited, Board, MovesToEmptySpace),   % get moves to empty spaces
    
    associate_distance_to_move(MovesToEmptySpace, NewDist, MovesToEmptyWithDist),  % X-Y to X-Y-NewDist

    append(MovesNotVisited, RestMoves, ListMoves),    % RestMoves is the recursive result
    append(MovesNotVisited, Visited, NewVisited),
    append(RestQueue, MovesToEmptyWithDist, NewQueue),

    get_valid_moves_bfs(MaxMoves, Piece, Player-Board, NewQueue, NewVisited, RestMoves).


get_valid_moves_bfs_square(_, _, _, [], _, []).
get_valid_moves_bfs_square(MaxMoves, _, _, [(_,_,MaxMoves) | _], _, []):-!. % q.front()'s distance has reached max, so stop

get_valid_moves_bfs_square(MaxMoves, Piece, Player-Board, [(X,Y,Dist) | RestQueue], Visited, ListMoves):-
    % We know piece is the square (4)
    % The square can jump over pieces
    Dist < MaxMoves,
    NewDist is Dist + 1,
    % includes moves to pieces that we cannot attack
    get_adjacent(X, Y, Player-MaxMoves, Board, Moves),

    subtract(Moves, Visited, MovesNotVisited),  % remove visited positions

    % cannot jump over enemy square
    filter_all_except_enemy_squares(Player-Piece, MovesNotVisited, Board, MovesForQueue),

    % filters moves: only to empty spaces or to pieces that we can attack
    filter_possible_moves(Player-Piece, MovesNotVisited, Board, PossibleMoves),

    associate_distance_to_move(MovesForQueue, NewDist, MovesWithDist),  % X-Y to X-Y-NewDist
    
    append(PossibleMoves, RestMoves, ListMoves),    % RestMoves is the recursive result
    append(MovesNotVisited, Visited, NewVisited),
    append(RestQueue, MovesWithDist, NewQueue),

    get_valid_moves_bfs_square(MaxMoves, Piece, Player-Board, NewQueue, NewVisited, RestMoves).


% Function to replace a value in a matrix at row X and column Y
% replace_matrix_value(+Matrix, +Row, +Col, +NewValue, -NewMatrix)
replace_matrix_value([], _, _, _, []).
% we reached row X, so replace the element at column Y
replace_matrix_value([Row|T], 1, Y, NewValue, [NewRow|T]):-
    !,
    replace_element(Row, Y, NewValue, NewRow).

% Recursive case: Keep traversing rows until we reach row X
replace_matrix_value([Row|T], X, Y, NewValue, [Row|NewT]):-
    NewX is X - 1,
    replace_matrix_value(T, NewX, Y, NewValue, NewT).

% Helper function to replace an element at a specific position in a list
% replace_element(+List, +Column, +NewValue, -NewList)
replace_element([], _, _, []).
replace_element([_|T], 1, NewValue, [NewValue|T]):- !.
replace_element([H|T], Col, NewValue, [H|NewT]):-
    NewCol is Col - 1,
    replace_element(T, NewCol, NewValue, NewT).
