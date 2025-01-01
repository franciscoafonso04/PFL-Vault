:- module(validation, [valid_moves/2]).


% valid_moves(+GameState, -ListOfMoves)
valid_moves(game_state(Board, Player), Moves) :-
    findall(move(Row, Col, Dir),
        (valid_direction(Dir), valid_move(Board, Player, Row, Col, Dir)),
        Moves).
    %write('Valid Moves for '), write(Player), write(': '), write(Moves), nl, nl.


% valid_move(+Board, +Player, +Row, +Col, +Direction)
valid_move(Board, Player, Row, Col, Dir) :-
    player_piece(Player, Piece), % Map the current player to their piece
    nth1(Row, Board, RowData),
    nth1(Col, RowData, Cell),
    Cell = Piece, % Ensure this cell belongs to the player
    capture_possible(Board, Player, Row, Col, Dir). % Check if a capture is possible in the given direction


% capture_possible(+Board, +Player, +Row, +Col, -Direction)
capture_possible(Board, Player, Row, Col, Direction) :-
    next_position(Row, Col, Direction, NextRow, NextCol),
    within_bounds(Board, NextRow, NextCol),
    nth1(NextRow, Board, NextRowData),
    nth1(NextCol, NextRowData, NextCell),
    NextCell = empty, % Ensure the first step is into an empty cell

    step_towards_capture(Board, Row, Col, Direction, TargetRow, TargetCol),
    within_bounds(Board, TargetRow, TargetCol),
    nth1(TargetRow, Board, TargetRowData),
    nth1(TargetCol, TargetRowData, TargetCell),
    opponent_piece(Player, Piece),
    TargetCell = Piece.


step_towards_capture(Board, Row, Col, Direction, TargetRow, TargetCol) :-
    next_position(Row, Col, Direction, NextRow, NextCol),
    within_bounds(Board, NextRow, NextCol),
    nth1(NextRow, Board, NextRowData),
    nth1(NextCol, NextRowData, NextCell),
    (
        NextCell = empty ->
        step_towards_capture(Board, NextRow, NextCol, Direction, TargetRow, TargetCol) % Continue stepping
    ;
        TargetRow = NextRow,
        TargetCol = NextCol,
        NextCell \= empty
    ).