% Module declaration with exported predicates
:- module(move, [choose_move/3, move/3]).   
:- use_module(library(random)).     % For random selection of moves
:- use_module(library(lists)).      % For list operations
:- use_module('utilities.pl').      % Utility predicates
:- use_module('validation.pl').     % Validation predicates

% -------------------------------------------------------------------------------------------------
% MOVE SELECTION

% choose_move(+GameState, +Level, -Move)

% Determines the move to be executed by a player or AI based on the game state and player type.
% This predicate handles human inputs and implements different strategies for AI decision-making,
% depending on the difficulty level.

% Receives:
%   +GameState: The current state of the game, represented as game_state(Board, Player, Rule).
%       - Board: A 2D list representing the current layout of the board.
%       - Player: The player whose move is being determined (e.g., `player1` or `player2`).
%       - Rule: The active movement rule (e.g., `normal` or `easy`).
%   +PlayerType: The type of player making the move:
%       - `human`: A human player who provides input for their move.
%       - `computer(1)`: AI using a random strategy (Level 1).
%       - `computer(2)`: AI using a strategic heuristic (Level 2).
%   -Move: The selected move to be executed, represented as move(Row, Col, Direction).

% Functionality:
% - For a human player, the predicate reads and validates user input for their move.
% - For AI players:
%     - Level 1 AI selects a random valid move from the list of available moves.
%     - Level 2 AI evaluates all possible moves using the `value/3` heuristic and selects
%       the move that maximizes its advantage while minimizing the opponent's options.

% Workflow:
% 1. For human players:
%    - Prompt the player to enter a move in the format `move(Row, Col, Direction)`.
%    - Validate the move against the list of valid moves.
% 2. For AI players:
%    - Generate all valid moves using `valid_moves/2`.
%    - For Level 1, use `random_member/2` to select a random move.
%    - For Level 2, evaluate each move's impact using the `value/3` heuristic and choose
%      the best move based on a combination of maximizing the player's advantage and
%      minimizing the opponent's options.

% Related Predicates:
% - `valid_moves/2`: Determines the valid moves for a player.
% - `value/3`: Evaluates the game state to calculate heuristic values for AI decision-making.

% Human player's move
% Prompts the user to enter a move and validates it.
choose_move(GameState, human, Move) :-
    write('Enter your "move(row,col,dir)." or "exit."'),
    repeat,
    read(Input),
    (   Input == exit ->                % Exit the game
        write('Exiting the game...'), nl,
        halt                            % Ends the Prolog program
    ;   Input = move(Row, Col, Dir),
        convert_row(Row, NewRow),
        move(NewRow, Col, Dir) = Move,  % Create the move structure
        valid_moves(GameState, Moves),  % Validate against the list of valid moves
        (member(Move, Moves) ->         % If valid, break the loop
            !, true
        ;   write('Invalid move, please try again.'), nl,
            fail                        % Otherwise, prompt again
        )
    ).

% Computer player's move, Level 1 (random move)
% Selects a random move from the list of valid moves.
choose_move(GameState, computer(1), RandomMove) :-
    valid_moves(GameState, Moves),
    random_member(RandomMove, Moves).

% Computer player's move, Level 2 (strategic move)
% Selects a move that maximizes the difference between the player's and the opponent's moves, if there is a draw
% selects a move that minimizes the Opponent possible moves, if there is another draw
% picks a random move from the tied moves
choose_move(GameState, computer(2), BestMove) :-
    current_player(GameState, Player),  % Get the current player (1 or 2)
    switch_player(Player, Opponent),    % Get the opponent
    valid_moves(GameState, Moves),      % Get all valid moves

    % Score each move based on its impact
    findall((Value, OpponentValue, Move),
        (member(Move, Moves),
        move(GameState, Move, NewGameState),            % Simulate the move
        value(NewGameState, Player, PlayerValue),       % Evaluate the state for the current player
        value(NewGameState, Opponent, OpponentValue),   % Evaluate the state for the opponent
        Value is PlayerValue - OpponentValue),          % Calculate the net value of the move
        ScoredMoves),
    
    % Identify the move with the maximum score
    max_member((MaxValue, _, _), ScoredMoves), % Find the maximum value.
    findall((OpponentValue, Move), member((MaxValue, OpponentValue, Move), ScoredMoves), BestMoves),  % Collect all moves with the maximum value
    
    % Break ties by minimizing opponent options
    min_member((MinOpponentValue, _), BestMoves),                          
    findall(Move, member((MinOpponentValue, Move), BestMoves), FinalMoves), 

    % Randomly select from tied moves
    random_member(BestMove, FinalMoves). 

% -------------------------------------------------------------------------------------------------
% MOVE EXECUTION

% move(+GameState, +Move, -NewGameState)

% Executes a move based on the current game state, updating the board, the current player, and preserving the active rule.
% This predicate ensures that moves are validated before execution and correctly updates the game state.

% Receives:
%   +GameState: The current state of the game, formatted as game_state(Board, Player, Rule).
%       - Board: A 2D list representing the current layout of the board (e.g., positions of `black`, `white`, and `empty` cells).
%       - Player: The player executing the move (e.g., `player1` or `player2`).
%       - Rule: The active movement rule (`normal` for restricted diagonals or `easy` for unrestricted diagonals).
%   +Move: The move to execute, represented as move(Row, Col, Dir).
%       - Row: The starting row of the piece being moved.
%       - Col: The starting column of the piece being moved.
%       - Dir: The direction of movement (e.g., `north`, `south`, `east`, `west`, `northeast`, `northwest`, `southeast`, `southwest`).
%   -NewGameState: The resulting state of the game after executing the move, formatted as game_state(NewBoard, NextPlayer, Rule).
%       - NewBoard: The updated board after the move.
%       - NextPlayer: The player who will take the next turn (switches to the other player).
%       - Rule: The movement rule remains unchanged.

% Functionality:
% - Validates the provided move against the list of valid moves for the current player and rule.
% - Simulates the move by capturing opponent pieces and updating the board.
% - Switches the current player to the next player after executing the move.
% - Ensures the game state is properly updated to reflect the changes.

% Workflow:
% 1. Validate the move using the `valid_moves/2` predicate to ensure it adheres to the rules.
% 2. Execute the move using the `capture_move/5` predicate to update the board and handle piece captures.
% 3. Switch the current player to the next player using the `switch_player/2` predicate.
% 4. Return the updated game state as `NewGameState`.

% Notes:
% - The `valid_moves/2` predicate ensures that the move is valid before it is executed.
% - The `capture_move/5` predicate is responsible for simulating the move and updating the board.
% - The `switch_player/2` predicate switches the active player, ensuring turn-based gameplay.

move(game_state(Board, Player, Rule), move(Row, Col, Dir), game_state(NewBoard, NextPlayer, Rule)) :-
    valid_moves(game_state(Board, Player, Rule), Moves),    % Check if move is valid
    member(move(Row, Col, Dir), Moves),                     
    player_piece(Player, Piece),                            % Get the current player's piece
    capture_move(Board, Row, Col, Dir, Piece, TempBoard),   % Execute the move
    switch_player(Player, NextPlayer),                      % Switch to the next player
    NewBoard = TempBoard.

% capture_move(+Board, +Row, +Col, +Dir, +Piece, -NewBoard)
% Simulates a move that captures an opponent's piece and updates the board.
capture_move(Board, Row, Col, Dir, Piece, NewBoard) :-
    next_position(Row, Col, Dir, NewRow, NewCol),           % Calculate the next position
    within_bounds(NewRow, NewCol),                          % Ensure the position is within bounds
    nth1(NewRow, Board, RowData),
    nth1(NewCol, RowData, Cell),
    (
        Cell = empty ->                                     % Continue moving if the cell is empty
        replace_cell(Board, Row, Col, empty, TempBoard),    % Remove piece from current position
        capture_move(TempBoard, NewRow, NewCol, Dir, Piece, NewBoard)   % Recurse to the next position
    ;   % If the cell is occupied, capture the piece
        Cell \= empty, % Stop when the cell is not empty (opponent's piece)
        replace_cell(Board, Row, Col, empty, TempBoard), % Remove piece from original position
        replace_cell(TempBoard, NewRow, NewCol, empty, TempBoard2), % Remove the opponent's piece
        replace_cell(TempBoard2, Row, Col, Piece, NewBoard) % Place piece in the final position
    ).

% -------------------------------------------------------------------------------------------------
% VALUE PREDICATE

% value(+GameState, +Player, -Value)

% Evaluates the current game state to determine the advantage of a given player.
% This predicate calculates the difference between the number of valid moves available
% to the current player and their opponent. It serves as a heuristic for decision-making
% in AI-based gameplay.

% Receives:
%   +GameState: The current state of the game, represented as game_state(Board, Player, Rule).
%       - Board: A 2D list representing the current layout of the board.
%       - Rule: The active movement rule (e.g., `normal` or `easy`).
%   +Player: The player whose advantage is being calculated (e.g., `player1` or `player2`).
%   -Value: The calculated heuristic value representing the advantage of the player.
%       - Positive Value: The player has more valid moves than their opponent.
%       - Negative Value: The opponent has more valid moves than the player.
%       - Zero: Both players have an equal number of valid moves.

% Functionality:
% - Determines the number of valid moves available to the specified player using `valid_moves/2`.
% - Determines the number of valid moves available to the opponent.
% - Computes the difference between the player's valid moves and the opponent's valid moves.

% Workflow:
% 1. Use `valid_moves/2` to calculate the valid moves for the current player.
% 2. Use `switch_player/2` to determine the opponent and calculate their valid moves.
% 3. Subtract the opponent's move count from the player's move count to compute the heuristic value.

% Notes:
% - This predicate is particularly useful for AI decision-making in determining the best move.
% - A higher value indicates a strategic advantage for the player.

% Related Predicates:
% - `valid_moves/2`: Determines the valid moves for a player.
% - `switch_player/2`: Identifies the opponent of the current player.

value(game_state(Board, _, Rule), Player, Value) :-
    switch_player(Player, Opponent),                                % Get opponent
    valid_moves(game_state(Board, Player, Rule), PlayerMoves),      % Calculate valid moves of the current player
    length(PlayerMoves, PlayerMoveCount),                           % Calculate number of valid moves of the current player

    valid_moves(game_state(Board, Opponent, Rule), OpponentMoves),  % Calculate valid moves of the opponent player
    length(OpponentMoves, OpponentMoveCount),                       % Calculate number of valid moves of the opponent player

    Value is PlayerMoveCount - OpponentMoveCount.                   % Compute the difference

% -------------------------------------------------------------------------------------------------

% replace_cell(+Board, +Row, +Col, +NewValue, -NewBoard)
% Updates a specific cell on the board.
replace_cell(Board, Row, Col, NewValue, NewBoard) :-
    nth1(Row, Board, OldRow),
    replace_in_list(OldRow, Col, NewValue, NewRow),
    replace_in_list(Board, Row, NewRow, NewBoard).

% replace_in_list(+List, +Index, +Value, -NewList)
% Updates a specific index in a list.
replace_in_list(List, Index, Value, NewList) :-
    nth1(Index, List, _, Rest),
    nth1(Index, NewList, Value, Rest).