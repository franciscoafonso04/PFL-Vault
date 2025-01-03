% Module declaration and exported predicates
:- module(validation, [valid_moves/2]).
:- use_module('utilities.pl').  % Import utility predicates
:- use_module(library(lists)).  % Import list operations

% -------------------------------------------------------------------------------------------------
% VALID MOVES PREDICATE

% valid_moves(+GameState, -ListOfMoves)

% Determines all valid moves for the current player based on the game state.
% This predicate generates a comprehensive list of moves that adhere to the game's rules and are possible 
% from the player's current position on the board.

% Receives:
%   +GameState: The current state of the game, represented as game_state(Board, Player, Rule).
%       - Board: A 2D list representing the current layout of the board (e.g., positions of `black`, `white`, and `empty` cells).
%       - Player: The player whose moves are being validated (e.g., `player1` or `player2`).
%       - Rule: The active movement rule (`normal` for restricted diagonals or `easy` for unrestricted diagonals).
%   -ListOfMoves: A list of all valid moves for the current player.
%       - Each move is represented as move(Row, Col, Direction), where:
%           - Row: The starting row of the move.
%           - Col: The starting column of the move.
%           - Direction: The direction of the move (e.g., `north`, `south`, `east`, `west`, `northeast`, `northwest`, `southeast`, `southwest`).

% Functionality:
% - Iterates through every possible starting position on the board.
% - For each position, checks all directions to determine valid moves.
% - Validates moves using the `valid_move/4` predicate to ensure they adhere to the rules.
% - Collects all valid moves into a list.

% Workflow:
% 1. Use the `findall/3` predicate to iterate over all possible moves.
% 2. For each move, validate it using `valid_move/4` to ensure it complies with the game's rules.
% 3. Return the list of all valid moves for the current player.

% Notes:
% - The `valid_move/4` predicate is responsible for checking individual moves.
% - This predicate is crucial for AI decision-making and move validation during gameplay.

% Related Predicates:
% - `valid_move/4`: Validates individual moves.
% - `capture_possible/5`: Determines if a move results in a valid capture.

valid_moves(GameState, Moves) :-
    findall(move(Row, Col, Dir),                
        (valid_move(GameState, Row, Col, Dir)), % Check for each valid move
        Moves).                                 % Collect all valid moves into a list

% -------------------------------------------------------------------------------------------------
% VALID MOVE PREDICATE

% valid_move(+GameState, +Row, +Col, +Direction)
% Checks if a specific move is valid for the current player, given the game state.
% - Ensures the move starts from a cell containing the player's piece.
% - Checks that the direction is valid based on the current rule.
% - Verifies that the move results in capturing an opponent's piece.
valid_move(game_state(Board, Player, Rule), Row, Col, Dir) :-
    player_piece(Player, Piece),                    % Get the current player's piece color
    nth1(Row, Board, RowData),                      % Access the row at index Row
    nth1(Col, RowData, Cell),                       % Access the cell at index Col in the row
    Cell = Piece,                                   % Ensure the cell contains the player's piece
    valid_direction(Rule, Row, Col, Dir),           % Check if the direction is valid for the current rule
    capture_possible(Board, Player, Row, Col, Dir). % Check if a capture is possible in the given direction

% -------------------------------------------------------------------------------------------------
% CAPTURE POSSIBLE PREDICATE

% capture_possible(+Board, +Player, +Row, +Col, -Dir)
% Determines if a capture is possible in a given direction starting from a specific cell.
% - The first step must land on an empty cell.
% - The move must eventually reach a cell containing an opponent's piece.
capture_possible(Board, Player, Row, Col, Dir) :-
    next_position(Row, Col, Dir, NextRow, NextCol), % Compute the next position
    within_bounds(NextRow, NextCol),                % Ensure the next position is within bounds
    nth1(NextRow, Board, NextRowData),              % Access the row at the next position
    nth1(NextCol, NextRowData, NextCell),           % Access the cell at the next position
    NextCell = empty,                               % Ensure the next cell is empty

    step_towards_capture(Board, Row, Col, Dir, TargetRow, TargetCol),   % Continue moving
    within_bounds(TargetRow, TargetCol),            % Ensure the target position is within bounds
    nth1(TargetRow, Board, TargetRowData),          % Access the row at the target position
    nth1(TargetCol, TargetRowData, TargetCell),     % Access the cell at the target position
    opponent_piece(Player, Piece),                  % Get the opponent's piece color
    TargetCell = Piece.                             % Ensure the target cell contains the opponent's piece

% -------------------------------------------------------------------------------------
% STEP TOWARDS CAPTURE PREDICATE

% step_towards_capture(+Board, +Row, +Col, +Direction, -TargetRow, -TargetCol)
% Recursively computes the next position in the given direction until it reaches a non-empty cell.
% - The recursion continues through empty cells.
% - Stops at a cell containing any piece (opponent's or player's).
step_towards_capture(Board, Row, Col, Direction, TargetRow, TargetCol) :-
    next_position(Row, Col, Direction, NextRow, NextCol),   % Compute the next position
    within_bounds(NextRow, NextCol),                        % Ensure the next position is within bounds
    nth1(NextRow, Board, NextRowData),                      % Access the row at the next position
    nth1(NextCol, NextRowData, NextCell),                   % Access the cell at the next position
    (
        NextCell = empty ->     % Continue moving if the cell is empty
        step_towards_capture(Board, NextRow, NextCol, Direction, TargetRow, TargetCol) % Continue stepping
    ;
        TargetRow = NextRow,    % Stop at the target position
        TargetCol = NextCol,
        NextCell \= empty       % Ensure the target cell is non-empty
    ).