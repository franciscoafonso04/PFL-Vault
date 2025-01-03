:- module(board, [initial_state/2, display_game/1]).
:- use_module('validation.pl').
:- use_module('utilities.pl').

% Initializes the game state
initial_state([_, _, Rule], game_state(Board, player1, Rule)) :-
    Board = [
        [black, white, black, white, black, white, black, white, black],
        [white, empty, empty, empty, empty, empty, empty, empty, white],
        [empty, empty, empty, empty, empty, empty, empty, empty, empty],
        [black, empty, empty, empty, empty, empty, empty, empty, black],
        [white, black, white, black, white, black, white, black, white]
    ].

% Displays the game board and other information
display_game(game_state(Board, Player, Rule)) :-
    player_profile(Player, Profile),

    valid_moves(game_state(Board, Player, _), Moves),
    length(Moves, MoveCount),

    write('-------------------------------------------------------------------------------------'), nl,
    write(Profile), write(' | Possible moves: '), write(MoveCount), nl,
    write('-------------------------------------------------------------------------------------'), nl,
    display_board(Board, Rule), nl.

% Displays the board with column and row labels
display_board([Row5, Row4, Row3, Row2, Row1], 1) :-    
    write('5 '), print_row(Row5, 1), nl,
    write('  |\\  |  /|\\  |  /|\\  |  /|\\  |  /|'), nl,
    write('  |  \\|/  |  \\|/  |  \\|/  |  \\|/  |'), nl,
    write('4 '), print_row(Row4, 1), nl, 
    write('  |  /|\\  |  /|\\  |  /|\\  |  /|\\  |'), nl,
    write('  |/  |  \\|/  |  \\|/  |  \\|/  |  \\|'), nl,
    write('3 '), print_row(Row3, 1), nl, 
    write('  |\\  |  /|\\  |  /|\\  |  /|\\  |  /|'), nl,
    write('  |  \\|/  |  \\|/  |  \\|/  |  \\|/  |'), nl,
    write('2 '), print_row(Row2, 1), nl, 
    write('  |  /|\\  |  /|\\  |  /|\\  |  /|\\  |'), nl,
    write('  |/  |  \\|/  |  \\|/  |  \\|/  |  \\|'), nl,
    write('1 '), print_row(Row1, 1), nl,
    write('  1   2   3   4   5   6   7   8   9').

display_board([Row5, Row4, Row3, Row2, Row1], 2) :-    
    write('5 '), print_row(Row5, 2), nl,
    write('                           '), nl,
    write('                           '), nl,
    write('4 '), print_row(Row4, 2), nl,
    write('                           '), nl,
    write('                           '), nl,
    write('3 '), print_row(Row3, 2), nl,
    write('                           '), nl,
    write('                           '), nl,
    write('2 '), print_row(Row2, 2), nl,
    write('                           '), nl,
    write('                           '), nl,
    write('1 '), print_row(Row1, 2), nl,
    write('  1   2   3   4   5   6   7   8   9').


% Helper predicate to print a single cell with optional color
print_cell(black, ColorCode) :-
    put_code(27), write(ColorCode), write('X'), put_code(27), write('[0m').
print_cell(white, ColorCode) :-
    put_code(27), write(ColorCode), write('O'), put_code(27), write('[0m').
print_cell(empty, _) :-
    write('+').

% Base case for printing a row
print_row([], _) :- !.

% Recursive case for printing a row with style 1
print_row([Cell], 1) :-  % Last element, no "---" suffix
    (Cell = black -> print_cell(black, '[34m');
     Cell = white -> print_cell(white, '[31m');
     print_cell(empty, '')).

print_row([Cell | Rest], 1) :-  % Intermediate elements, include "---"
    (Cell = black -> print_cell(black, '[34m'), write('---');
     Cell = white -> print_cell(white, '[31m'), write('---');
     print_cell(empty, ''), write('---')),
    print_row(Rest, 1).

% Recursive case for printing a row with style 2
print_row([Cell], 2) :-  % Last element, no "   " suffix
    (Cell = black -> print_cell(black, '[34m');
     Cell = white -> print_cell(white, '[31m');
     print_cell(empty, '')).

print_row([Cell | Rest], 2) :-  % Intermediate elements, include "   "
    (Cell = black -> print_cell(black, '[34m'), write('   ');
     Cell = white -> print_cell(white, '[31m'), write('   ');
     print_cell(empty, ''), write('   ')),
    print_row(Rest, 2).


