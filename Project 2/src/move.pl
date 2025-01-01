:- module(move, [choose_move/3, move/3]).
:- use_module('utilities.pl').
:- use_module('validation.pl').

% choose_move(+GameState, +Level, +Move)
% Gets the next move for the current player
choose_move(GameState, human, Move) :-
    write('Enter your move (Row, Col, Direction) or "exit" to quit: '), nl,
    read(Input),
    (   Input == exit ->
        write('Exiting the game...'), nl,
        halt  % Ends the Prolog program
    ;   Input = Move,
        valid_moves(GameState, Moves),
        (member(Move, Moves) ->
            true
        ;
            write('Invalid move, please try again.'), nl,
            choose_move(human, GameState, Move)
        )
    ).


% Computer player's move, Level 1 (random move)
choose_move(GameState, computer(1), Move) :-
    valid_moves(GameState, Moves),
    random_member(RandomMove, Moves).

% Computer player's move, Level 2 (strategic move)
choose_move(GameState, computer(2), BestMove) :-
    current_player(GameState, Player),  % Get the current player (player1 or player2)
    switch_player(Player, Opponent),   % Get the opponent

    valid_moves(GameState, Moves), % Get all valid moves
    findall(Value-Move,
        (member(Move, Moves),
        move(GameState, Move, NewGameState), % Simulate the move
        value(NewGameState, Player, PlayerValue),  % Evaluate the state for the current player
        value(NewGameState, Opponent, OpponentValue), % Evaluate the state for the opponent
        Value is PlayerValue - OpponentValue), % Calculate the net value of the move
        ScoredMoves),
    max_member(_-BestMove, ScoredMoves). % Select the move with the highest score


% Executes a move and updates the game state
% move(+GameState, +Move, -NewGameState)
move(game_state(Board, Player), move(Row, Col, Dir), game_state(NewBoard, NextPlayer)) :-
    valid_moves(game_state(Board, Player), Moves), % Check if move is valid
    member(move(Row, Col, Dir), Moves), % Ensure the move is valid

    player_piece(Player, Piece), % Get the player's piece
    capture_move(Board, Row, Col, Dir, Piece, TempBoard), % Execute the move

    switch_player(Player, NextPlayer), % Switch the player
    NewBoard = TempBoard.

% capture_move(+Board, +Row, +Col, +Dir, +Piece, -NewBoard)
capture_move(Board, Row, Col, Dir, Piece, NewBoard) :-
    next_position(Row, Col, Dir, NewRow, NewCol),
    within_bounds(Board, NewRow, NewCol),
    nth1(NewRow, Board, RowData),
    nth1(NewCol, RowData, Cell),
    (
        Cell = empty -> % Continue moving if the cell is empty
        replace_cell(Board, Row, Col, empty, TempBoard), % Remove piece from current position
        capture_move(TempBoard, NewRow, NewCol, Dir, Piece, NewBoard) % Recurse to the next position
    ;
        Cell \= empty, % Stop when the cell is not empty (opponent's piece)
        replace_cell(Board, Row, Col, empty, TempBoard), % Remove piece from original position
        replace_cell(TempBoard, NewRow, NewCol, empty, TempBoard2), % Remove the opponent's piece
        replace_cell(TempBoard2, Row, Col, Piece, NewBoard) % Place piece in the final position
    ).

% value(+GameState, +Player, -Value)
value(game_state(Board, _), Player, Value) :-
    % Get opponent
    switch_player(Player, Opponent),

    % Calculate valid moves for the current player and opponent
    valid_moves(game_state(Board, Player), PlayerMoves),
    length(PlayerMoves, PlayerMoveCount),

    valid_moves(game_state(Board, Opponent), OpponentMoves),
    length(OpponentMoves, OpponentMoveCount),

    % Compute the difference
    Value is PlayerMoveCount - OpponentMoveCount.

replace_cell(Board, Row, Col, NewValue, NewBoard) :-
    nth1(Row, Board, OldRow),
    replace_in_list(OldRow, Col, NewValue, NewRow),
    replace_in_list(Board, Row, NewRow, NewBoard).

replace_in_list(List, Index, Value, NewList) :-
    nth1(Index, List, _, Rest),
    nth1(Index, NewList, Value, Rest).