% Imports required modules
:- use_module('utilities.pl').  % Utility predicates for general use
:- use_module('board.pl').      % Board-related predicates
:- use_module('move.pl').       % Move execution and validation
:- use_module('validation.pl'). % Validation rules for game states and moves
:- use_module('menu.pl').       % Menu-related predicates for user interaction
:- use_module(library(lists)).  % Built-in list processing library

% -------------------------------------------------------------------------------------------------
% COMMENTS ON STRATEGY

% - `play/0`: Acts as the entry point for the game, providing access to the menu system.
% - `setup_game/3`: Ensures all necessary game parameters are initialized before starting the main game loop.
% - `game_loop/2`: Handles the core game logic, alternating turns, and checking for game-ending conditions.
% - `game_over/2`: Evaluates the game state to determine if one player has won or if the game should continue.
% - Input validation (e.g., valid moves and menu options) ensures a smooth user experience and prevents invalid actions.

% Starts the program by initializing the main game loop
:- initialization play.

% -------------------------------------------------------------------------------------------------
% MAIN GAME ENTRY POINT

% Main predicate to start the game.
% Displays the main menu, allowing players to configure the game mode, rule, and other parameters.
% Calls the `setup_game/3` predicate with the selected options.
play :- 
    display_main_menu(Player1Type, Player2Type, Rule),
    setup_game(Player1Type, Player2Type, Rule).

% -------------------------------------------------------------------------------------------------
% GAME SETUP

% Sets up the initial game configuration and starts the game loop.
% Receives:
%   +Player1Type: The type of Player 1 (human or computer(Difficulty)).
%   +Player2Type: The type of Player 2 (human or computer(Difficulty)).
%   +Rule: The selected rule (1 for Normal, 2 for Easy).
setup_game(Player1Type, Player2Type, Rule) :-
    GameConfig = [Player1Type, Player2Type, Rule],  % GameConfig encapsulates the game parameters
    initial_state(GameConfig, GameState),           % Initializes the game state
    !,                                              % Prevent fallback to other clauses
    game_loop(GameConfig, GameState).               % Starts the main game loop

% -------------------------------------------------------------------------------------------------
% GAME LOOP

% Main game loop that manages the progression of the game.
% Receives:
%   +GameConfig: The configuration of the game (player types and rule).
%   +GameState: The current state of the game (board, player, rule).
game_loop(GameConfig, GameState) :-

    % Displays the current state of the game
    display_game(GameState),       

    % Check if the game is over
    (   game_over(GameState, Winner)    % If the game is over
    ->  % Announce the winner
        write('Game over! Winner: '), write(Winner), nl, 
        % Prompt the user to return to the menu or exit
        write('Go to the "menu." or "exit." the game'),
        repeat, % Loop until valid input is received
        read(Input),
        (   Input = menu -> % If the user selects "menu," go back to menu
            !, play 
        ;   Input = exit -> % If the user selects "exit," end the program
            !, halt
            % If the input is invalid, prompt again
        ;   write('Invalid option. Please try again'),
            fail % Restart the repeat loop
        )
    ;   % Otherwise, continue the game
        current_player(GameState, Player),                  % Get the current player (player1 or player2)
        get_player_type(GameConfig, Player, PlayerType),    % Get the player type from GameConfig
        choose_move(GameState, PlayerType, Move),           % Determine the player's move
        move(GameState, Move, NewGameState),                % Execute the move and update the game state
        game_loop(GameConfig, NewGameState)                 % Recursively call game_loop with the new state
    ).

% -------------------------------------------------------------------------------------------------
% GAME OVER CHECK

% game_over(+GameState, -Winner)

% Determines if the game has ended and identifies the winner.
% This predicate evaluates the current state of the game to decide if there are no more valid moves for
% each of the players, thereby ending the game.

% Receives:
%   +GameState: The current state of the game, represented as game_state(Board, Player, Rule).
%       - Board: A 2D list representing the current layout of the board.
%       - Player: The player whose turn it is (e.g., `player1` or `player2`).
%       - Rule: The active movement rule (e.g., `normal` for restricted diagonals or `easy` for unrestricted diagonals).
%   -Winner: The result of the game:
%       - `player1`: Player 1 is the winner.
%       - `player2`: Player 2 is the winner.

% Functionality:
% - Checks if the current player has no valid moves:
%     - If true, the opponent wins.
% - Utilizes `valid_moves/2` to determine if a player can make any moves.

% Workflow:
% 1. Use `valid_moves/2` to check the current player's possible moves.
% 2. If no moves are available for the current player, the opponent wins.

% Related Predicates:
% - `valid_moves/2`: Determines available moves for a player.
% - `switch_player/2`: Switches the current player to their opponent.

game_over(GameState, Winner) :-
    current_player(GameState, Player),          % Get the current player
    valid_moves(GameState, PlayerMoves),        % Check the current player's valid moves

    (   PlayerMoves = []                        % If the current player has no valid moves
    ->  switch_player(Player, Opponent),        % Switch to the opponent        
        player_profile(Opponent, Winner)        % Declare the opponent as the winner
    ;   fail                                    % Otherwise, the game is not over
    ).
