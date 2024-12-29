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
    handle_menu_option(Option).

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

%------------------------------------------------------------------------------------------------------------

% Sets up the game configuration and starts the initial state
setup_game(Player1Type, Player2Type) :-
    write('Setting up the game...'), nl,
    GameConfig = [player1:Player1Type, player2:Player2Type, board_size:5],
    initial_state(GameConfig, GameState),
    write('Initial GameState: '), write(GameState), nl, % Debugging
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
        [empty, empty, empty, empty, empty],
        [empty, empty, empty, empty, empty],
        [black, empty, empty, empty, empty],
        [empty, empty, empty, empty, empty],
        [empty, empty, white, empty, empty]
    ],
    write('Initialized Board:'), nl,
    display_board(Board).

% Main game loop
game_loop(GameState) :-
    display_game(GameState),
    valid_moves(GameState, Moves),
    write('Valid Moves: '), write(Moves), nl,
    (
        Moves = [] ->
        (write('No valid moves! Game over.'), nl, !)
    ;
        current_player(GameState, Player),
        get_next_move(Player, GameState, Move),
        move(GameState, Move, NewGameState),
        game_loop(NewGameState)
    ).

% Displays the game board and other information
display_game(game_state(Board, Player)) :-
    write('Current board:'), nl,
    display_board(Board),
    write('Current player: '), write(Player), nl.

% Displays the board
display_board(Board) :-
    maplist(display_row, Board).

display_row(Row) :-
    write(Row), nl.

% Determines if the game is over and identifies the winner
game_over(game_state(_, _), _) :-
    fail. % Placeholder: replace with actual game-over logic

% Gets the next move for the current player
get_next_move(human, _, Move) :-
    write('Enter your move: '),
    read(Move). % Placeholder: validate input

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
    write('Calculated Moves: '), write(Moves), nl.

% valid_move(+Board, +Player, +Row, +Col, +Direction)
valid_move(Board, Player, Row, Col, Dir) :-
    player_piece(Player, Piece), % Map the current player to their piece
    nth1(Row, Board, RowData),
    nth1(Col, RowData, Cell),
    write('Testing Row: '), write(Row), write(' Col: '), write(Col), write(' Cell: '), write(Cell), nl,
    Cell = Piece, % Ensure this cell belongs to the player
    capture_possible(Board, Row, Col, Dir). % Check if a capture is possible in the given direction

% capture_possible(+Board, +Row, +Col, -Direction)
capture_possible(Board, Row, Col, Direction) :-
    valid_direction(Direction),
    next_position(Row, Col, Direction, R1, C1), % Calculate the target position
    write('Checking Direction: '), write(Direction),
    write(' Target Row: '), write(R1), write(' Target Col: '), write(C1), nl,
    within_bounds(Board, R1, C1), % Ensure the target position is within bounds
    nth1(R1, Board, TargetRow), % Access the correct row using 1-based indexing
    nth1(C1, TargetRow, TargetCell), % Access the correct cell in the row
    write('Accessed Target Cell: '), write(TargetCell), nl,
    TargetCell \= empty, % Ensure the target cell is not empty
    player_piece(player2, OpponentPiece), % Check if it’s the opponent’s piece
    TargetCell = OpponentPiece.




