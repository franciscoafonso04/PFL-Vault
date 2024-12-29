:- use_module('utilities.pl').
:- use_module(library(lists)).

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
    setup_game(human, computer).
handle_menu_option(3) :-
    setup_game(computer, computer).
handle_menu_option(4) :-
    write('Goodbye!'), nl, !.
handle_menu_option(_) :-
    write('Invalid option. Please try again.'), nl,
    display_main_menu.

% Displays the board
display_board(Board) :-
    maplist(display_row, Board).

display_row(Row) :-
    write(Row), nl.

%------------------------------------------------------------------------------------------------------------

% Sets up the game configuration and starts the initial state
setup_game(Player1Type, Player2Type) :-
    write('Setting up the game...'), nl,
    GameConfig = [player1:Player1Type, player2:Player2Type, board_size:5],
    initial_state(GameConfig, GameState),
    game_loop(GameState).

% Gets current player
current_player(game_state(_, Player), Player).

% Initializes the game state
initial_state(GameConfig, game_state(Board, player1)) :-
    member(board_size:BoardSize, GameConfig),
    create_test_board(BoardSize, Board).

% Creates a test board with initial pieces
create_test_board(_Size, Board) :-
    Board = [
        [black, black, black, black, black],
        [black, black, black, black, black],
        [white, white, white, white, white],
        [white, white, white, white, white],
        [empty, empty, empty, empty, empty]
    ].

% Main game loop
game_loop(GameState) :-
    display_game(GameState),
    valid_moves(GameState, Moves),
    (
        Moves = [] -> % No valid moves
        (write('No valid moves! Game over.'), nl, !)
    ;
        current_player(GameState, Player),
        write('Valid Moves: '), write(Moves), nl, % Debugging
        get_valid_move(Player, Moves, Move), % Get and validate the move
        move(GameState, Move, NewGameState),
        game_loop(NewGameState)
    ).


get_valid_move(Player, Moves, Move) :-
    repeat,
    (
        get_next_move(Player, _, Move), % Get the player's move
        member(Move, Moves) -> true
    ;
        write('Invalid move. Try again.'), nl, fail
    ).

% Displays the game board and other information
display_game(game_state(Board, Player)) :-
    write('Current board:'), nl,
    display_board(Board),
    write('Current player: '), write(Player), nl.


% Determines if the game is over and identifies the winner
game_over(game_state(Board, _), Winner) :-
    % Check if both players have no valid moves
    valid_moves(game_state(Board, player1), Player1Moves),
    valid_moves(game_state(Board, player2), Player2Moves),
    Player1Moves = [], Player2Moves = [], % Both players have no moves

    % Count pieces for each player
    count_pieces(Board, player1, Player1Count),
    count_pieces(Board, player2, Player2Count),

    % Determine the winner or draw
    (
        Player1Count > Player2Count -> Winner = player1
    ;
        Player2Count > Player1Count -> Winner = player2
    ;
        Winner = draw % Equal pieces -> draw
    ).


% Gets the next move for the current player
get_next_move(human, _, Move) :-
    write('Enter your move as move(Row, Col, Direction): '),
    read(Input),
    (
        Input = move(Row, Col, Dir) ->
        Move = Input
    ;
        write('Invalid format. Use move(Row, Col, Direction).'), nl,
        fail
    ).


get_next_move(computer, GameState, Move) :-
    choose_move(GameState, 1, Move). % Example: using level 1 AI

% Executes a move and updates the game state
move(game_state(Board, Player), move(Row, Col, Dir), game_state(NewBoard, NextPlayer)) :-
    % Placeholder: implement actual move execution logic
    write('Executing move: '), write(move(Row, Col, Dir)), nl,
    NewBoard = Board, % For now, just copy the board
    switch_player(Player, NextPlayer).

% Switches the player
switch_player(player1, player2).
switch_player(player2, player1).

% Player Piece Colors
player_piece(player1, black).
player_piece(player2, white).

count_pieces(Board, Player, Count) :-
    player_piece(Player, Piece),          % Get the piece type for the player
    flatten(Board, FlatBoard),            % Flatten the 2D board into a 1D list
    include(=(Piece), FlatBoard, Pieces), % Filter out only the pieces belonging to the player
    length(Pieces, Count).                % Count the number of pieces

% Announces the winner
announce_winner(Winner) :-
    write('Game over! Winner: '), write(Winner), nl.

%------------------------------------------------------------------------------------------------------------

% valid_moves(+GameState, -ListOfMoves)
valid_moves(game_state(Board, Player), Moves) :-
    write('Checking valid moves for Player: '), write(Player), nl,
    findall(move(Row, Col, Dir),
        (valid_direction(Dir), valid_move(Board, Player, Row, Col, Dir)),
        Moves),
    write('Valid Moves: '), write(Moves), nl.

% valid_move(+Board, +Player, +Row, +Col, +Direction)
valid_move(Board, Player, Row, Col, Dir) :-
    player_piece(Player, Piece), % Map the current player to their piece
    nth1(Row, Board, RowData),
    nth1(Col, RowData, Cell),
    Cell = Piece, % Ensure this cell belongs to the player
    capture_possible(Board, Row, Col, Dir). % Check if a capture is possible in the given direction

% capture_possible(+Board, +Row, +Col, -Direction)
capture_possible(Board, Row, Col, Direction) :-
    step_towards_capture(Board, Row, Col, Direction, TargetRow, TargetCol),
    nth1(TargetRow, Board, TargetRowData),
    nth1(TargetCol, TargetRowData, TargetCell),
    player_piece(player2, OpponentPiece),
    TargetCell = OpponentPiece.


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






