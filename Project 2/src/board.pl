:- module(board, [initial_state/2, display_game/1]).

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
    write('Current board:'), nl,
    display_board(Board),
    write('Current player: '), write(Player), nl.

% Displays the board with column and row labels
display_board(Board) :-
    % Print column headers
    write('    '),
    print_columns,
    nl,
    % Print board rows with row numbers
    print_rows(Board, 1).

% Prints column numbers
print_columns :- 
    maplist(print_column, [1, 2, 3, 4, 5, 6, 7, 8, 9]).

% Helper to print each column number followed by a space
print_column(Col) :-
    write(Col), write(' ').

% Prints each row of the board, preceded by row number
print_rows([], _).
print_rows([Row | Rest], RowNum) :- 
    % Print row number
    write(RowNum), write(' | '),
    print_row(Row),
    write('|'), nl,
    NextRowNum is RowNum + 1,
    print_rows(Rest, NextRowNum).

% Prints a single row with space between elements
print_row([]).
print_row([Cell | Rest]) :- 
    (Cell == black -> write('X ')   % Blue for black
    ; Cell == white -> write('O ')  % Green for white
    ; write('  ')),                 % Orange for empty
    print_row(Rest).