:-ensure_loaded('display.pl').
:-ensure_loaded('moves.pl').
:-ensure_loaded('menu_helpers.pl').
:-ensure_loaded('main.pl').

normal_max_size(11).

play:-
    repeat,
    print_banner(
        ["0. Exit", "1. Player vs Player", "2. Player vs Computer", "3. Computer vs Player", "4. Computer vs Computer"],
        "PLAY TACTIGON",
        50),
    select_option(0, 4, Option),
    main_menu_option(Option).

main_menu_option(0):- !.
main_menu_option(1):-
    play_game(0-0). % both human
main_menu_option(2):-
    select_level(Level, "2"),
    play_game(0-Level).
main_menu_option(3):-
    select_level(Level, "1"),
    play_game(Level-0).
main_menu_option(4):-
    select_level(RedLevel, "1"),
    select_level(GreenLevel, "2"),
    play_game(RedLevel-GreenLevel).

play_game(RedLevel-GreenLevel):-
    select_advanced_rules(AdvRules),
    select_padding(PaddingSize),
    asserta(padding(PaddingSize)),
    initial_state(PaddingSize, Board),
    clear_screen,

    normal_max_size(NormalSize),
    BoardMaxSize is PaddingSize * 4 + NormalSize,
    display_game(Board, BoardMaxSize),
    game_cycle(r-Board-AdvRules, RedLevel-GreenLevel, BoardMaxSize).

% choose_move(+Board, +Player, +Level, -Move)
choose_move(Board-_, Player, 0, (Xi, Yi, Xf, Yf)):-   % human
    select_piece(Board-Player, Xi, Yi),

    write('Select a position to move to: '), nl,
    read_position(Xf, Yf).
choose_move(GS, P, 0, Move):-
    write('Invalid move! Try again.'), nl,
    choose_move(GS, P, 0, Move).

choose_move(Board-AdvRules, Player, 1, (Xi, Yi, Xf, Yf)):-   % computer dumb (random)
    random_move(Board-AdvRules, Player, (Xi, Yi, Xf, Yf)),
    describe_computer_move(Xi, Yi, Xf, Yf),
    press_enter('Press enter to make the computer move').

choose_move(Board-AdvRules, Player, 2, (Xi, Yi, Xf, Yf)):-   % computer smart
    smart_move(Board-AdvRules, Player, (Xi, Yi, Xf, Yf)),
    describe_computer_move(Xi, Yi, Xf, Yf),
    press_enter('Press enter to make the computer move').

% game_cycle(+GameState, +RedLevel-GreenLevel)
not_in_board(_, []). % base case

not_in_board(Piece, [Row | Rest]):-
    \+ memberchk(Piece, Row),
    not_in_board(Piece, Rest).

win_by_elimination(Board, g):-
    not_in_board(r-5, Board).

win_by_elimination(Board, r):-
    not_in_board(g-5, Board).

win_by_golden(g-Board, g):-
    padding(PaddingSize),
    Col is 6 + 2 * PaddingSize,
    get_value(Board, 2, Col, g-_),
    get_value(Board, 6, Col, g-_).

win_by_golden(r-Board, r):-
    padding(PaddingSize),
    Col is 6 + 2 * PaddingSize,
    get_value(Board, 2, Col, r-_),
    get_value(Board, 6, Col, r-_).

game_over(_-Board, Winner):-
    win_by_elimination(Board, Winner).

game_over(Player-Board, Winner):-
    win_by_golden(Player-Board, Winner).

congratulate(r):-
    write('Player 1 won!'), nl,
    press_enter('Press Enter to continue'),
    !, fail.

congratulate(g):-
    write('Player 2 won!'), nl,
    press_enter('Press Enter to continue'),
    !, fail.

game_cycle(Player-Board-_, _, _):-
    game_over(Player-Board, Winner), !,
    congratulate(Winner).

game_cycle(Player-Board-AdvRules, LevelsFromMenu, BoardMaxSize):-
    player_label(Player, PlayerLabel),
    write('Player '), write(PlayerLabel), write(' turn.'), nl,
    
    level(Player, LevelsFromMenu, Level),
    choose_move(Board-AdvRules, Player, Level, (Xi, Yi, Xf, Yf)),
    move(Player-Board-AdvRules, (Xi, Yi, Xf, Yf), NP-NB),

    clear_screen,
    display_game(NB, BoardMaxSize),
    !,
    game_cycle(NP-NB-AdvRules, LevelsFromMenu, BoardMaxSize).

% level(+Player, +RedLevel-+GreenLevel, -Level)
% Used to identify the level of the given player (0 human, 1 random, 2 greedy/optimal)
level(r, L-_, L).
level(g, _-L, L).
player_label(r, '1').
player_label(g, '2').
