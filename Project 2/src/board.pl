:- module(board, [initial_state/2, display_game/1]).
:- use_module('validation.pl').
:- use_module('utilities.pl').

% Initializes the game state
initial_state(_, game_state(Board, player1)) :-
    Board = [
        [black, white, black, white, black, white, black, white, black],
        [white, empty, empty, empty, empty, empty, empty, empty, white],
        [empty, empty, empty, empty, empty, empty, empty, empty, empty],
        [black, empty, empty, empty, empty, empty, empty, empty, black],
        [white, black, white, black, white, black, white, black, white]
    ].

% Displays the game board and other information
display_game(game_state(Board, Player)) :-
    player_profile(Player, Profile),

    valid_moves(game_state(Board, Player), Moves),
    length(Moves, MoveCount),

    write('-------------------------------------------------------------------------------------'), nl,
    write(Profile), write(' | Possible moves: '), write(MoveCount), nl,
    write('-------------------------------------------------------------------------------------'), nl,
    display_board(Board),
    nl.

% Displays the board with column and row labels
display_board(Board) :-
    write('   + - + - + - + - + - + - + - + - + - +'), nl,
    print_rows(Board, 5),
    write('     1   2   3   4   5   6   7   8   9    ').

% Prints each row of the board, preceded by row number
print_rows([], _).
print_rows([Row | Rest], RowNum) :-
    % Print row number and row cells
    format(' ~w |', [RowNum]),
    print_row(Row),
    nl,

    % Print row separator
    write('   + - + - + - + - + - + - + - + - + - +'), nl,
    NextRowNum is RowNum - 1,
    print_rows(Rest, NextRowNum).

% Prints a single row with cells and separators
print_row([]).

print_row([black | Rest]) :-
    put_code(27), write('[34m'), write(' X '), put_code(27), write('[0m|'),  % Blue for X
    print_row(Rest).

print_row([white | Rest]) :-
    put_code(27), write('[31m'), write(' O '), put_code(27), write('[0m|'),  % Red for O
    print_row(Rest).

print_row([empty | Rest]) :-
    write('   |'),  % Empty cell
    print_row(Rest).
