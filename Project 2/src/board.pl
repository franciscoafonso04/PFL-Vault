% Module declaration with exported predicates
:- module(board, [initial_state/2, display_game/1]).
:- use_module('utilities.pl').  % Utilities module for auxiliary predicates

% -------------------------------------------------------------------------------------------------
% INITIAL STATE PREDICATE

% initial_state(+GameConfig, -GameState)

% Initializes the game state with a predefined board layout.
% - This predicate sets up the initial configuration of the game, defining the board, 
%   the starting player, and the game rule.

% Receives:
%   +GameConfig: A list containing the player types and the rule for the game.
%                Format: [Player1Type, Player2Type, Rule].
%                - Player1Type: Type of the first player (e.g., human, computer).
%                - Player2Type: Type of the second player (e.g., human, computer).
%                - Rule: Defines the movement rule (e.g., normal, easy).
%   -GameState: The resulting game state, represented as game_state(Board, StartingPlayer, Rule).
%                - Board: A 2D list representing the initial layout of the game board.
%                - StartingPlayer: The player who starts the game (always `player1`).
%                - Rule: The rule governing valid moves (normal or easy).

% Notes:
% - The board is a predefined layout where `black` and `white` pieces are strategically placed.
% - The game always starts with `player1` taking the first turn.
% - The rule parameter determines whether diagonal moves are restricted (`normal`) or unrestricted (`easy`).
%
% This predicate is essential for initializing the game and setting the initial conditions before gameplay begins.

initial_state([_, _, Rule], game_state(Board, player1, Rule)) :-
    Board = [
        [black, white, black, white, black, white, black, white, black],
        [white, empty, empty, empty, empty, empty, empty, empty, white],
        [empty, empty, empty, empty, empty, empty, empty, empty, empty],
        [black, empty, empty, empty, empty, empty, empty, empty, black],
        [white, black, white, black, white, black, white, black, white]
    ].

% -------------------------------------------------------------------------------------------------
% DISPLAY GAME PREDICATE

% display_game(+GameState)

% Displays the current game state, including the board layout, current player information, and rule details.
% - This predicate is responsible for printing a user-friendly representation of the game state to the console,
%   allowing players to visualize the current board and track whose turn it is.

% Receives:
%   +GameState: A structure representing the current state of the game, formatted as game_state(Board, Player, Rule).
%       - Board: A 2D list representing the current layout of the board (e.g., positions of `black`, `white`, and `empty` cells).
%       - Player: The current player whose turn it is (e.g., `player1` or `player2`).
%       - Rule: Indicates the active movement rule (e.g., `normal` for restricted diagonals or `easy` for unrestricted diagonals).

% Functionality:
% - Displays the name and profile of the current player.
% - Prints the board in a visually structured format, incorporating the active movement rule.
% - Ensures clarity by using color coding and labeled rows/columns for easy navigation.

% Notes:
% - The player's profile is displayed using the `player_profile/2` predicate for clarity (e.g., "Player 1 - O").
% - The board is displayed with numbered rows and columns to facilitate move selection.
% - Depending on the active rule, the board visualization may vary.

display_game(game_state(Board, Player, Rule)) :-
    player_profile(Player, Profile),

    write('-------------------------------------------------------------------------------------'), nl,
    write(Profile), nl,
    write('-------------------------------------------------------------------------------------'), nl,
    display_board(Board, Rule), nl.

% -------------------------------------------------------------------------------------------------
% DISPLAY BOARD

% Displays the board with column and row labels based on the active rule.
% Receives:
%   +Board: The board matrix to display.
%   +Rule: Indicates the active rule (1 for Normal, 2 for Easy).
display_board([Row5, Row4, Row3, Row2, Row1], 1) :- % Normal Rule (restricted diagonals)
    % Each row is displayed with corresponding labels and connections.
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

display_board([Row5, Row4, Row3, Row2, Row1], 2) :- % Easy Rule (unrestricted diagonals)
    % Each row is displayed without diagonal connection indicators.
    write('5 '), print_row(Row5, 2), nl,
    write('                           '), nl,
    write('4 '), print_row(Row4, 2), nl,
    write('                           '), nl,
    write('3 '), print_row(Row3, 2), nl,
    write('                           '), nl,
    write('2 '), print_row(Row2, 2), nl,
    write('                           '), nl,
    write('1 '), print_row(Row1, 2), nl,
    write('  1   2   3   4   5   6   7   8   9').

% -------------------------------------------------------------------------------------------------
% PRINTING ROWS AND CELLS

% Helper predicate to print a single cell with optional color.
% Receives:
%   +Cell: The piece in the cell (black, white, or empty).
%   +ColorCode: The color code for terminal output.
print_cell(black, ColorCode) :-
    put_code(27), write(ColorCode), write('U'), put_code(27), write('[0m').
print_cell(white, ColorCode) :-
    put_code(27), write(ColorCode), write('O'), put_code(27), write('[0m').
print_cell(empty, _) :-
    write('+').

% Base case for printing a row
print_row([], _) :- !.

% Recursive case for printing a row with style 1 (Normal Rule).
print_row([Cell], 1) :-  % Last element, no "---" suffix
    (Cell = black -> print_cell(black, '[34m');
     Cell = white -> print_cell(white, '[31m');
     print_cell(empty, '')).

print_row([Cell | Rest], 1) :-  % Intermediate elements, include "---"
    (Cell = black -> print_cell(black, '[34m'), write('---');
     Cell = white -> print_cell(white, '[31m'), write('---');
     print_cell(empty, ''), write('---')),
    print_row(Rest, 1).

% Recursive case for printing a row with style 2 (Easy Rule).
print_row([Cell], 2) :-  % Last element, no "   " suffix
    (Cell = black -> print_cell(black, '[34m');
     Cell = white -> print_cell(white, '[31m');
     print_cell(empty, '')).

print_row([Cell | Rest], 2) :-  % Intermediate elements, include "   "
    (Cell = black -> print_cell(black, '[34m'), write('   ');
     Cell = white -> print_cell(white, '[31m'), write('   ');
     print_cell(empty, ''), write('   ')),
    print_row(Rest, 2).