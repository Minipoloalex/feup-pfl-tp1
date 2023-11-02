clear_screen:-
    write('\33\[2J').

press_enter(Message):-
    write(Message), nl,
    skip_line.

print_text([]).
print_text([FirstCharCode | RestText]):-
    char_code(FirstChar, FirstCharCode),
    write(FirstChar),
    print_text(RestText).

% print_banner(+Options, +Title, +GivenLineLength)
% GivenLineLength must be even
print_banner(Options, Title, GivenLineLength):-
    length(Title, TitleLength),

    0 is GivenLineLength mod 2,
    
    Mod is TitleLength mod 2,
    LineLength is GivenLineLength + Mod,

    TotalEmptySpaces is (LineLength - 2 - TitleLength) // 2,

    print_n(LineLength, '='), nl,
    
    write('='), print_n(TotalEmptySpaces, ' '),
    print_text(Title),
    print_n(TotalEmptySpaces, ' '), write('='), nl,
    
    print_n(LineLength, '='), nl,
    
    EmptyLineSize is LineLength - 2,
    
    write('='), print_n(EmptyLineSize, ' '), write('='), nl,
    
    print_options(Options, LineLength),

    write('='), print_n(EmptyLineSize, ' '), write('='), nl,
    print_n(LineLength, '='), nl.


% print_options(+Options, +LineLength)
print_options([], _).
print_options([Option | OptionsTail], LineLength):-
    length(Option, OptionLength),
    TotalEmptySpaces is LineLength - OptionLength - 3,
    write('= '), print_text(Option), print_n(TotalEmptySpaces, ' '), write('='), nl,
    print_options(OptionsTail, LineLength).


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

read_number_prompt(Prompt, _, X):-
    write(Prompt),
    read_number(X),
    !.
read_number_prompt(Prompt, ErrorPrompt, X):-
    write(ErrorPrompt), nl,
    read_number_prompt(Prompt, ErrorPrompt, X).

% select_option(+Min, +Max, -Option)
select_option(Min, Max, Option):-
    write('Select an option between '), write(Min), write(' and '), write(Max), write(': '),
    read_number(Option),
    Option >= Min, Option =< Max,
    !.
select_option(Min, Max, Option):-
    % might need skip_line here (TODO: check this)
    write('Invalid option! Try again.'), nl,
    select_option(Min, Max, Option).

% read_position(-X, -Y)
read_position(X, Y):-
    read_number_prompt('Row: ', 'Invalid row!', X),
    X \= 0,
    read_number_prompt('Column: ', 'Invalid column!', Y),
    Y \= 0.
select_level(Level, ComputerPlayer):-
    append(" player ", ComputerPlayer, TitleLastPart),
    append("Select the computer level for", TitleLastPart, Title),
    print_banner(["0. Go back", "1. Easy", "2. Hard"], Title, 50),
    select_option(0, 2, Level),
    Level \= 0.


select_piece(Board-Player, Xi, Yi):-
    write('Select a piece to move: '), nl,
    read_position(Xi, Yi),
    valid_piece(Board, Xi, Yi, Player-_),
    !.
select_piece(Board-Player, Xi, Yi):-
    write('Invalid piece!'), nl,
    select_piece(Board-Player, Xi, Yi).

select_padding(PaddingSize):-
    write('Select a padding size between 0 and 4 or just press Enter to select no padding.'), nl,
    write('To go back select an invalid padding size.'), nl,
    read_number(PaddingSize),
    PaddingSize >= 0, PaddingSize =< 4.

select_advanced_rules(AdvancedRules):-
    print_banner(
        ["0. Go Back", "1. Yes", "2. No"],
        "Do you want to play with advanced rules?",
        50
    ),
    select_option(0, 2, AdvancedRules),
    AdvancedRules \= 0.
