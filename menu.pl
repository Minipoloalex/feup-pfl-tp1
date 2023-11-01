:-ensure_loaded('display.pl').
:-ensure_loaded('moves.pl').
:-ensure_loaded('menu_helpers.pl').
:-ensure_loaded('main.pl').

normal_max_size(11).

play:-
    repeat,
    print_banner(
        ["0. Exit", "1. Player vs Player", "2. Player vs Computer", "3. Computer vs Computer"],
        "PLAY TACTIGON",
        50),
    select_option(0, 3, Option),
    main_menu_option(Option).

main_menu_option(0):- !.
main_menu_option(1):-
    play_game(0-0). % both human
main_menu_option(2):-
    select_level(Level, "Green"),
    play_game(0-Level).
main_menu_option(3):-
    select_level(RedLevel, "Red"),
    select_level(GreenLevel, "Green"),
    play_game(RedLevel-GreenLevel).

play_game(RedLevel-GreenLevel):-
    % initial_state(Board),
    select_padding(PaddingSize),
    initial_state(PaddingSize, Board),
    clear_screen,

    normal_max_size(NormalSize),
    BoardMaxSize is PaddingSize * 4 + NormalSize,
    display_game(Board, BoardMaxSize),
    game_cycle(r-Board, RedLevel-GreenLevel, BoardMaxSize).

% choose_move(+Board, +Player, +Level, -Move)
choose_move(Board, Player, 0, (Xi, Yi, Xf, Yf)):-   % human
    repeat,
    select_piece(Board-Player, Xi, Yi),

    write('Select a position to move to: '), nl,
    read_position(Xf, Yf).

% game_cycle(+GameState, +RedLevel-GreenLevel)

% game_cycle(GameState, _):-
%     game_over(GameState, Winner), !,
%     congratulate(Winner).
game_cycle(Player-Board, LevelsFromMenu, BoardMaxSize):-
    write('Player '), write(Player), write(' turn.'), nl,
    
    level(Player, LevelsFromMenu, Level),
    choose_move(Board, Player, Level, (Xi, Yi, Xf, Yf)),
    move(Player-Board, (Xi, Yi, Xf, Yf), NP-NB),

    clear_screen,
    display_game(NB, BoardMaxSize),
    !,
    game_cycle(NP-NB, LevelsFromMenu, BoardMaxSize).

% level(+Player, +RedLevel-+GreenLevel, -Level)
% Used to identify the level of the given player (0 human, 1 random, 2 greedy/optimal)
level(r, L-_, L).
level(g, _-L, L).
