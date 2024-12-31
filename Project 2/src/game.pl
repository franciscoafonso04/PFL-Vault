:- use_module('utilities.pl').
:- use_module(library(lists)).
:- use_module(library(random)).

:- initialization play.

play :- display_main_menu.

%------------------------------------------------------------------------------------------------------------

display_main_menu :-
    write('Welcome to Collapse!'), nl,
    write('1. Human vs Human'), nl,
    write('2. Human vs Computer'), nl,
    write('3. Computer vs Computer'), nl,
    write('4. Exit'), nl,
    write('Choose an option: '),
    read(Option),
    (
        member(Option, [1, 2, 3, 4]) -> handle_menu_option(Option)
    ;
        write('Invalid option. Please try again.'), nl,
        display_main_menu
    ).


% Handle user input for the menu
handle_menu_option(1) :-
    setup_game(human, human).
handle_menu_option(2) :-
    select_difficulty(Difficulty),
    setup_game(human, computer(Difficulty)).
handle_menu_option(3) :-
    setup_game(computer(1), computer(1)).
handle_menu_option(4) :-
    write('Goodbye!'), nl, !.
handle_menu_option(_) :-
    write('Invalid option. Please try again.'), nl,
    display_main_menu.

% Displays a menu to select difficulty level for the AI
select_difficulty(Difficulty) :-
    write('Select AI Difficulty:'), nl,
    write('1. Easy (Chico)'), nl,
    write('2. Hard (Ramos)'), nl,
    write('Choose an option: '),
    read(Option),
    (   member(Option, [1, 2]) -> Difficulty = Option
    ;   write('Invalid option. Please try again.'), nl,
        select_difficulty(Difficulty)
    ).

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

%------------------------------------------------------------------------------------------------------------

% Sets up the game configuration and starts the initial state
setup_game(Player1Type, Player2Type) :-
    write('Setting up the game...'), nl,
    GameConfig = [player1:Player1Type, player2:Player2Type],
    initial_state(GameConfig, GameState),
    !, % Prevent fallback to other clauses
    game_loop(GameConfig, GameState).

% Gets current player
current_player(game_state(_, Player), Player).

% Initializes the game state
initial_state(_, game_state(Board, player1)) :-
    Board = [
        [black, white, black, white, black, white, black, white, black],
        [white, empty, empty, empty, empty, empty, empty, empty, white],
        [empty, empty, empty, empty, empty, empty, empty, empty, empty],
        [black, empty, empty, empty, empty, empty, empty, empty, black],
        [white, black, white, black, white, black, white, black, white]
    ].


% Main game loop
game_loop(_, GameState) :-
    % Check if the game is over
    game_over(GameState, Winner),
    Winner \= nobody,  % If there is a winner or it's a draw
    !,  % Stop the game loop
    announce_winner(Winner).  % Announce the result

game_loop(GameConfig, GameState) :-
    display_game(GameState),

    current_player(GameState, Player),  % Get the current player (player1 or player2)
    get_player_type(GameConfig, Player, PlayerType),  % Get the player type from GameConfig

    get_next_move(PlayerType, GameState, Move),

    move(GameState, Move, NewGameState),
    !,  % Prevent backtracking into previous moves
    game_loop(GameConfig, NewGameState).  % Continue the game loop


% Displays the game board and other information
display_game(game_state(Board, Player)) :-
    write('Current board:'), nl,
    display_board(Board),
    write('Current player: '), write(Player), nl.


% Determines if the game is over and identifies the winner, should change to more declarative solution after solving more important things
game_over(game_state(Board, _), Winner) :-
    % Check if both players have no valid moves
    valid_moves(game_state(Board, player1), Player1Moves),
    valid_moves(game_state(Board, player2), Player2Moves),

    % Determine the winner or draw based on conditions
    (
        Player1Moves = [], Player2Moves = [] -> Winner = draw
    ;
        Player1Moves = [] -> Winner = player2
    ;
        Player2Moves = [] -> Winner = player1
    ;
        Winner = nobody
    ).

% value(+GameState, +Player, -Value)
value(game_state(Board, _), player1, Value) :-
    valid_moves(game_state(Board, player1), Player1Moves),
    valid_moves(game_state(Board, player2), Player2Moves),
    length(Player1Moves, Player1MoveCount),
    length(Player2Moves, Player2MoveCount),
    Value is Player1MoveCount - Player2MoveCount.

% value(+GameState, +Player, -Value)
value(game_state(Board, _), player2, Value) :-
    valid_moves(game_state(Board, player1), Player1Moves),
    valid_moves(game_state(Board, player2), Player2Moves),
    length(Player1Moves, Player1MoveCount),
    length(Player2Moves, Player2MoveCount),
    Value is Player2MoveCount - Player1MoveCount.

% Gets the next move for the current player
get_next_move(human, _, Move) :-
    repeat,
    write('Enter your move as move(Row, Col, Direction): '),
    read(Input),
    (
        Input = exit -> 
        write('Exiting the game.'), nl, halt % Halts the program
    ;
        Input = move(_, _, _) ->
        Move = Input
    ;
        write('Invalid format. Use move(Row, Col, Direction).'), nl,
        fail
    ).

get_next_move(computer(Difficulty), GameState, Move) :-
    choose_move(GameState, Difficulty, Move).

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
    

%------------------------------------------------------------------------------------------------------------

% valid_moves(+GameState, -ListOfMoves)
valid_moves(game_state(Board, Player), Moves) :-
    findall(move(Row, Col, Dir),
        (valid_direction(Dir), valid_move(Board, Player, Row, Col, Dir)),
        Moves),
    write('Valid Moves for '), write(Player), write(': '), write(Moves), nl, nl.

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

% Level Easy
choose_move(GameState, 1, RandomMove) :-
    valid_moves(GameState, Moves),
    random_member(RandomMove, Moves).

replace_cell(Board, Row, Col, NewValue, NewBoard) :-
    nth1(Row, Board, OldRow),
    replace_in_list(OldRow, Col, NewValue, NewRow),
    replace_in_list(Board, Row, NewRow, NewBoard).

replace_in_list(List, Index, Value, NewList) :-
    nth1(Index, List, _, Rest),
    nth1(Index, NewList, Value, Rest).
