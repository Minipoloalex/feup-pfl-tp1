:-ensure_loaded('display.pl').
:-ensure_loaded('moves.pl').
% read_number(-X)
read_number(X):-
    read_number(X, 0).
read_number(Acc, Acc):-
    peek_code(10), skip_line, !.   % line feed's code is 10
read_number(X, Acc):-
    get_code(Code),
    char_code('0', Zero),
    char_code('9', Nine),
    Code >= Zero, Code =< Nine,
    !,
    Digit is Code - Zero,
    Acc1 is Acc * 10 + Digit,
    read_number(X, Acc1).
read_number(_, _):-
    skip_line, !, fail.

% select_option(+Min, +Max, -Option)
select_option(Min, Max, Option):-
    write('Select an option between '), write(Min), write(' and '), write(Max), write(': '),
    read_number(Option),
    Option >= Min, Option =< Max,
    !.
select_option(Min, Max, Option):-
    write('Invalid option! Try again.'), nl,
    select_option(Min, Max, Option).

read_number_prompt(Prompt, _, X):-
    write(Prompt),
    read_number(X),
    !.
read_number_prompt(Prompt, ErrorPrompt, X):-
    write(ErrorPrompt), nl,
    read_number_prompt(Prompt, ErrorPrompt, X).

% read_position(-X, -Y)
read_position(X, Y):-
    read_number_prompt('Row: ', 'Invalid row!', X),
    X \= 0,
    read_number_prompt('Column: ', 'Invalid column!', Y),
    Y \= 0.

main_menu:-
    repeat,
    write('1. Player vs Player'), nl,
    write('2. Player vs Computer'), nl,
    write('3. Computer vs Computer'), nl,
    write('4. Exit'), nl,
    select_option(1, 4, Option),
    main_menu_option(Option).

main_menu_option(1):-
    play_game(0-0). % both humans
% main_menu_option(2):-
    % select_level(Level),
    % play_game(0-Level).
% main_menu_option(3):-
%     play_computer_computer.
% main_menu_option(4):- !.

play_game(RedLevel-GreenLevel):-
    % initial_state(Board),
    board(Board),
    display_game(Board),
    game_cycle(r-Board, RedLevel-GreenLevel).

% play_human_human:-
%     play(Player1, Player2).

select_level(Level):-
    write('Select level: '), nl,
    write('1. Easy'), nl,
    write('2. Hard'), nl,
    select_option(1, 2, Level).

% play_human_computer:-
%     select_level(Level),

select_piece(Board-Player, Xi, Yi):-
    write('Select a piece to move: '), nl,
    read_position(Xi, Yi),
    valid_piece(Board, Xi, Yi, Player-_).

% choose_move(+Board, +Player, +Level, -Move)
choose_move(Board, Player, 0, (Xi, Yi, Xf, Yf)):-   % human
    repeat,
    select_piece(Board-Player, Xi, Yi),

    write('Select a position to move to: '), nl,
    read_position(Xf, Yf).

% game_cycle(+GameState)
% game_cycle(GameState, _):-
%     game_over(GameState, Winner), !,
%     congratulate(Winner).
game_cycle(Player-Board, LevelsFromMenu):-
    write('Player '), write(Player), write(' turn.'), nl,
    
    level(Player, LevelsFromMenu, Level),
    choose_move(Board, Player, Level, (Xi, Yi, Xf, Yf)),
    move(Player-Board, (Xi, Yi, Xf, Yf), NP-NB),

    display_game(NB),
    !,
    game_cycle(NP-NB, LevelsFromMenu).

level(r, L-_, L).
level(g, _-L, L).
