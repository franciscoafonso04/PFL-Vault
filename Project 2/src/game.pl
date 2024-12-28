:- use_module('utilities.pl').
:- use_module(library(lists)).

play :- display_main_menu.

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

% Sets up the game configuration and starts the initial state
setup_game(Player1Type, Player2Type) :-
    write('Setting up the game...'), nl,
    GameConfig = [player1:Player1Type, player2:Player2Type, board_size:5],
    initial_state(GameConfig, GameState),
    write('Initial GameState: '), write(GameState), nl, % Debugging
    game_loop(GameState).

current_player(game_state(_, Player), Player).

% Initializes the game state
initial_state(GameConfig, GameState) :-
    % Extract configurations
    member(board_size:BoardSize, GameConfig),
    % Create an empty board
    create_empty_board(BoardSize, Board),
    % Define the starting player
    GameState = game_state(Board, player1). % game_state(Board, CurrentPlayer)

% Creates an empty board of the given size
create_empty_board(Size, Board) :-
    length(Row, Size),
    maplist(=(empty), Row), % Fills each row with 'empty' cells
    length(Board, Size),
    maplist(=(Row), Board).

% Main game loop
game_loop(GameState) :-
    display_game(GameState),
    (
        game_over(GameState, Winner) ->
        announce_winner(Winner)
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
game_over(GameState, Winner) :-
    % Implement game-specific logic here
    fail. % Placeholder: replace with actual game-over logic

% Gets the next move for the current player
get_next_move(human, GameState, Move) :-
    write('Enter your move: '),
    read(Move). % Placeholder: validate input

get_next_move(computer, GameState, Move) :-
    choose_move(GameState, 1, Move). % Example: using level 1 AI

% Executes a move and updates the game state
move(GameState, Move, NewGameState) :-
    % Implement move validation and execution
    write('Executing move...'), nl, % Placeholder
    NewGameState = GameState. % Placeholder: update state accordingly

% Announces the winner
announce_winner(Winner) :-
    write('Game over! Winner: '), write(Winner), nl.

% -----------

% valid_moves(+GameState, -ListOfMoves)
valid_moves(game_state(Board, Player), Moves) :-
    findall(move(Row, Col, Dir),
        (valid_direction(Dir), valid_move(Board, Player, Row, Col, Dir)),
        Moves).

% valid_move(+Board, +Player, +Row, +Col, +Direction)
valid_move(Board, Player, Row, Col, Dir) :-
    nth1(Row, Board, RowData),
    nth1(Col, RowData, Piece),
    write('Testing Row: '), write(Row), write(' Col: '), write(Col), write(' Piece: '), write(Piece), nl,
    Piece = Player, % Ensure this is the player's piece
    capture_possible(Board, Row, Col, Dir).

% capture_possible(+Board, +Row, +Col, -Direction)
capture_possible(Board, Row, Col, Direction) :-
    valid_direction(Direction),
    next_position(Row, Col, Direction, R1, C1),
    write('Checking Direction: '), write(Direction), 
    write(' Target Row: '), write(R1), write(' Target Col: '), write(C1), nl,
    within_bounds(Board, R1, C1),           % Ensure (R1, C1) is within bounds
    nth1(R1, Board, NextRow),
    nth1(C1, NextRow, Opponent),           % Get the target piece
    Opponent \= empty, Opponent \= Player. % Ensure it's an opponent's piece


test_valid_moves :-
    InitialBoard = [
    [white, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty],
    [black, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty]
    ],
    GameState = game_state(InitialBoard, black),
    valid_moves(GameState, Moves),
    write(Moves), nl.
