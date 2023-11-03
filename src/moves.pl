%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                            %
%   This module contains the game logic, like getting valid  %
%   moves, executing moves, and computer moves               %
%                                                            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- ensure_loaded(library(lists)).
:- ensure_loaded(library(random)).

% valid_moves(+Board, +Player, -ListOfMoves)
% returns the list of valid moves for the player
valid_moves(Board-AdvRules, Player, ListOfMoves):-
    findall((Xi,Yi,Xf,Yf), (
        valid_piece(Board, Xi, Yi, Player-Piece),
        get_valid_moves_bfs((Xi,Yi), Piece, Player-Board-AdvRules, Moves),
        member((Xf,Yf), Moves)
    ), ListOfMoves).

% move(+Board, +Player-AttackerPiece, ?Xf-Yf, -NewGameState)
% returns the new game state after playing the designated move, if the move is valid
move(Player-Board-AdvRules, (Xi, Yi, Xf, Yf), NP-NB):-  % the piece to move should already be validated
    other_player(Player, NP),
    valid_piece(Board, Xi, Yi, Player-AttackerPiece),
    get_valid_moves_bfs((Xi,Yi), AttackerPiece, Player-Board-AdvRules, Moves),
    memberchk((Xf,Yf), Moves),
    get_value(Board, Xf, Yf, Value),
    get_resulting_piece(Player-AttackerPiece, Value, ResultingPiece),
    execute_move(Board, (Xi, Yi, Xf, Yf), ResultingPiece, NB).

% execute_move(+Board, +Move, -NewBoard)
% returns the new board after executing the move
execute_move(Board, (Xi, Yi, Xf, Yf), ResultingPiece, NB):-
    replace_matrix_value(Board, Xi, Yi, 0, IntermediaryBoard),
    replace_matrix_value(IntermediaryBoard, Xf, Yf, ResultingPiece, NB).


% other_player(?Player, ?OtherPlayer)
% changes the current player
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
    valid_position(Board, X, Y, Player-Piece), % if Piece is -1 or 0, then this fails (no player)
    nth1(X, Board, Line),
    nth1(Y, Line, Player-Piece).    

% get_value(+Board, +X, +Y, ?Value)
get_value(Board, X, Y, Value):-
    nth1(X, Board, Line),
    nth1(Y, Line, Value).

% valid_position(+Board, X, Y, -Value)
valid_position(Board, X, Y, Piece):-
    get_value(Board, X, Y, Piece),
    Piece \= -1.   % -1 is an invalid position

% get_resulting_piece(+Attacker, +Attacked, ?ResultingPiece)
% check the rules for attacking pieces
get_resulting_piece(Player-_, Player-_, _):-
    !, fail.    % assure that the same player is not attacking itself

get_resulting_piece(Piece, 0, Piece).
get_resulting_piece(P1-Attacker, _-Attacked, P1-Attacker):-
    Attacker =< Attacked,
    !.

get_resulting_piece(_-3, _-1, 0).
get_resulting_piece(_-4, _-3, 0).

% adjacent_diff(+Row, -ListAdjacent)
% returns the list of adjacent offsets for a given row
adjacent_diff(Row, [(0, -1), (0, 1), (-1, -1), (-1, 0), (1, -1), (1, 0)]):- % row is even
    0 is Row mod 2,
    !.
adjacent_diff(_, [(0, -1), (0, 1), (-1, 1), (-1, 0), (1, 1), (1, 0)]).    % row is odd

% get_adjacent(+Xi, +Yi, +AdjacentDiff, +Board, -ListAdjacent)
% returns the list of adjacent positions for a given position
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

% get_valid_moves_bfs(+Piece, +MaxMoves, +Player-Board, +Queue, +Visited, -ListMoves)
% returns the list of valid moves for a piece using bfs

% if piece is square (4), then can jump over other pieces -> different bfs
get_valid_moves_bfs((Xi, Yi), 4, Player-Board-1, ListMoves):-   % advanced rules on
    !,
    % if is golden square, MaxMoves is Piece + 1, else MaxMoves is Piece
    get_valid_moves_bfs_square(4, 4, Player-Board, [(Xi, Yi, 0)], [(Xi,Yi)], ListMoves).

get_valid_moves_bfs((Xi, Yi), Piece, Player-Board-_, ListMoves):-   % advanced rules on
    % if AdvRules = 1 and golden square, MaxMoves is Piece + 1, else MaxMoves is Piece
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

% replace_element(+List, +Column, +NewValue, -NewList)
% Helper function to replace an element at a specific position in a list
replace_element([], _, _, []).
replace_element([_|T], 1, NewValue, [NewValue|T]):- !.
replace_element([H|T], Col, NewValue, [H|NewT]):-
    NewCol is Col - 1,
    replace_element(T, NewCol, NewValue, NewT).

% random_move(+Board, +Player, -Move)
% returns a random move for the computer to play
random_move(Board-AdvRules, Player, (Xi, Yi, Xf, Yf)):-
    valid_moves(Board-AdvRules, Player, ListOfMoves),
    random_member((Xi, Yi, Xf, Yf), ListOfMoves).

% smart_move(+Board, +Player, -Move)
% returns a smart move for the computer to play
smart_move(Board-AdvRules, Player, (Xi, Yi, Xf, Yf)):-
    valid_moves(Board-AdvRules, Player, ListOfMoves),
    get_best_move(Board-AdvRules, Player, ListOfMoves, (Xi, Yi, Xf, Yf)).

% get_best_move(+Board, +Player, +ListOfMoves, -BestMove)
% returns the best move for the computer to play
get_best_move(Board-AdvRules, Player, ListOfMoves, BestMove):-
    evaluate_moves(Board-AdvRules, Player, ListOfMoves, ListOfValues),
    max_member(MaxValue, ListOfValues),
    findall((Xi, Yi, Xf, Yf), (
        nth1(Index, ListOfValues, MaxValue),
        nth1(Index, ListOfMoves, (Xi, Yi, Xf, Yf))
    ), ListOfBestMoves),
    random_member(BestMove, ListOfBestMoves).

% evaluate_moves(+Board, +Player, +ListOfMoves, -ListOfValues)
% returns the list of values for each move, with the same order as the list of moves
evaluate_moves(_, _, [], []).
evaluate_moves(Board-AdvRules, Player, [(Xi, Yi, Xf, Yf) | RestMoves], [Value | RestValues]):-
    move(Player-Board-AdvRules, (Xi, Yi, Xf, Yf), _-NewBoard),
    value(NewBoard, Player, Value),
    evaluate_moves(Board-AdvRules, Player, RestMoves, RestValues).

% value(+Board, +Player, -Value)
% returns the value of the board for the player
value(Board, Player, Value):-
    value(Board, Player, 1, 1, 0, Value).

% value(+Board, +Player, +Row, +Col, +Value, -FinalValue)
% returns the value of the board for the player
% Row and Col are important because central squares are more valuable (especially the golden ones)
value([], _, _, _, Value, Value).
value([Row | Rest], Player, RowIndex, ColIndex, Value, FinalValue):-
    evaluate_row(Row, Player, RowIndex, ColIndex, RowValue),
    NewRowIndex is RowIndex + 1,
    NewValue is Value + RowValue,
    value(Rest, Player, NewRowIndex, ColIndex, NewValue, FinalValue).

% evaluate_row(+Row, +Player, +RowIndex, +ColIndex, -Value)
% returns the value of the row for the player
evaluate_row([], _, _, _, 0).
evaluate_row([Position | Rest], Player, RowIndex, ColIndex, Value):-
    evaluate_position(Position, Player, RowIndex, ColIndex, PositionValue),
    NewColIndex is ColIndex + 1,
    evaluate_row(Rest, Player, RowIndex, NewColIndex, RestValue),
    Value is PositionValue + RestValue.

% evaluate_position(+Position, +Player, +RowIndex, +ColIndex, -Value)
% returns the value of the position for the player
% Position is either a piece (red or green), an empty space (0) or an invalid position (-1)
evaluate_position(-1, _, _, _, 0):- !.
evaluate_position(0, _, _, _, 0):- !.

evaluate_position(PosPlayer-PosPiece, Player, RowIndex, ColIndex, Value):-
    PosPlayer = Player,
    !,
    evaluate_piece(PosPiece, RowIndex, ColIndex, Value).

evaluate_position(_-PosPiece, _, RowIndex, ColIndex, Value):- % PosPlayer \= Player
    % if the piece is not from the player, then its value is negative
    % this is useful to make the computer want to capture enemy pieces if possible
    evaluate_piece(PosPiece, RowIndex, ColIndex, PieceValue),
    Value is -PieceValue.

% evaluate_piece(+Piece, +RowIndex, +ColIndex, -Value)
% returns the value of a piece placed at a certain position
evaluate_piece(5, _, _, 1000):- !.   % the pentagon is the most valuable piece, without it the game is lost

evaluate_piece(_, 2, C, 200):- 
    padding(PaddingSize),
    Center is 6 + 2 * PaddingSize,
    C =:= Center, !.    % golden square on top

evaluate_piece(_, 6, C, 200):-
    padding(PaddingSize),
    Center is 6 + 2 * PaddingSize,
    C =:= Center, !.    % golden square on bottom

evaluate_piece(Piece, Row, Col, Value):-
    % the closer the piece is to the center, the more valuable it is, the center is the square (4, 6)
    % the distance will be approximated by the manhattan distance
    % the value of the piece is the inverse of the MD multiplied by the value of the piece itself
    padding(PaddingSize),
    Center is 6 + 2 * PaddingSize,
    Dist is abs(Row - 4) + abs(Col - Center),
    Inverse is 5 - Dist,
    Value is Inverse * Piece.
