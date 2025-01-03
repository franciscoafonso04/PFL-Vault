:- use_module('utilities.pl').
:- use_module('board.pl').
:- use_module('move.pl').
:- use_module('validation.pl').
:- use_module('menu.pl').
:- use_module(library(lists)).

:- initialization play.

play :- 
    display_main_menu(Player1Type, Player2Type, Rule),
    setup_game(Player1Type, Player2Type, Rule).

% Sets up the game configuration and starts the initial state
setup_game(Player1Type, Player2Type, Rule) :-
    GameConfig = [Player1Type, Player2Type, Rule],
    initial_state(GameConfig, GameState),
    !, % Prevent fallback to other clauses
    game_loop(GameConfig, GameState).

game_loop(GameConfig, GameState) :-
    display_game(GameState),

    % Check if the game is over
    (   game_over(GameState, Winner)
    ->  write('Game over! Winner: '), write(Winner), nl, 
        write('Go to the "menu." or "exit." the game'),
        repeat, % Start repeat loop
        read(Input),
        (   Input = menu ->
            !, play 
        ;   Input = exit -> 
            !, halt
        ;   write('Invalid option. Please try again'),
            fail % Restart the repeat loop
        )
    ;   % Otherwise, continue the game
        current_player(GameState, Player),  % Get the current player (player1 or player2)
        get_player_type(GameConfig, Player, PlayerType),  % Get the player type from GameConfig
        choose_move(GameState, PlayerType, Move),
        move(GameState, Move, NewGameState),
        game_loop(GameConfig, NewGameState) % Recursively call game_loop with the new state
    ).

% Determines if the game is over and identifies the winner, should change to more declarative solution after solving more important things
game_over(GameState, Winner) :-
    current_player(GameState, Player),         % Get the current player
    valid_moves(GameState, PlayerMoves),       % Check the current player's valid moves

    (   PlayerMoves = []                       % If the current player has no valid moves
    ->  switch_player(Player, Opponent), 
        player_profile(Opponent, Winner)
    ;   fail                                   % Otherwise, the game is not over
    ).
