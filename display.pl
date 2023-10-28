% note that this is hardcoded and just to check if the initial board is correct
% it does not check for the correct amount of -1's.
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
