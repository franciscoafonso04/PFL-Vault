:- use_module('utilities.pl').
:- use_module('board.pl').
:- use_module('move.pl').
:- use_module('validation.pl').
:- use_module(library(lists)).
:- use_module(library(random)).

:- initialization play.

play :- display_main_menu.

%------------------------------------------------------------------------------------------------------------

display_main_menu :-

    write('-------------------------------------------------------------------------------------'), nl,
    write(' ______     ______     __         __         ______     ______   ______     ______   '), nl, 
    write('/\\  ___\\   /\\  __ \\   /\\ \\       /\\ \\       /\\  __ \\   /\\  == \\ /\\  ___\\   /\\  ___\\  '), nl, 
    write('\\ \\ \\____  \\ \\ \\_\\ \\  \\ \\ \\____  \\ \\ \\____  \\ \\  __ \\  \\ \\  __/ \\ \\___  \\  \\ \\  __\\  '), nl, 
    write(' \\ \\_____\\  \\ \\_____\\  \\ \\_____\\  \\ \\_____\\  \\ \\_\\ \\_\\  \\ \\_\\    \\/\\_____\\  \\ \\_____\\'), nl, 
    write('  \\/_____/   \\/_____/   \\/_____/   \\/_____/   \\/_/\\/_/   \\/_/     \\/_____/   \\/_____/'), nl, 
    write('-------------------------------------------------------------------------------------'), nl,
    write('Mode selection:'), nl,
    write('1. Human vs Human'), nl,
    write('2. Human vs Computer'), nl,
    write('3. Computer vs Human'), nl,
    write('4. Computer vs Computer'), nl,
    write('5. Exit'), nl,
    write('-------------------------------------------------------------------------------------'), nl,
    write('Choose an option: '),
    read(Option),
    (
        member(Option, [1, 2, 3, 4, 5]) -> handle_menu_option(Option)
    ;
        write('Invalid option. Please try again.'), nl,
        display_main_menu
    ).

% Handle user input for the menu
handle_menu_option(1) :-
    setup_game(human, human).
handle_menu_option(2) :-
    select_difficulty(Difficulty, ''),
    setup_game(human, computer(Difficulty)).
handle_menu_option(3) :-
    select_difficulty(Difficulty, ''),
    setup_game(computer(Difficulty), human).
handle_menu_option(4) :-
    select_difficulty(Difficulty_1, ' 1'),
    select_difficulty(Difficulty_2, ' 2'),
    setup_game(computer(Difficulty_1), computer(Difficulty_2)).
handle_menu_option(5) :-
    write('Goodbye!'), nl, !.
handle_menu_option(_) :-
    write('Invalid option. Please try again.'), nl,
    display_main_menu.

% Displays a menu to select difficulty level for the AI
select_difficulty(Difficulty, Computer) :-
    write('-------------------------------------------------------------------------------------'), nl,
    write('Computer'), write(Computer), write(' Level Selection:'), nl,
    write('1. Level 1'), nl,
    write('2. Level 2'), nl,
    write('-------------------------------------------------------------------------------------'), nl,
    write('Choose an option: '),
    read(Option),
    (   member(Option, [1, 2]) -> Difficulty = Option
    ;   write('Invalid option. Please try again.'), nl,
        select_difficulty(Difficulty)
    ).

%------------------------------------------------------------------------------------------------------------

% Sets up the game configuration and starts the initial state
setup_game(Player1Type, Player2Type) :-
    GameConfig = [player1:Player1Type, player2:Player2Type],
    initial_state(GameConfig, GameState),
    !, % Prevent fallback to other clauses
    game_loop(GameConfig, GameState).

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

    choose_move(GameState, PlayerType, Move),

    move(GameState, Move, NewGameState),
    !,  % Prevent backtracking into previous moves
    game_loop(GameConfig, NewGameState).  % Continue the game loop


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
