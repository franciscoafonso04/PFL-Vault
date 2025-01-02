:- use_module('utilities.pl').
:- use_module('board.pl').
:- use_module('move.pl').
:- use_module('validation.pl').
:- use_module(library(lists)).

:- initialization play.

play :- display_main_menu.

%------------------------------------------------------------------------------------------------------------

display_main_menu :-
    write('  ___   _____   __     __       __     ____   ___   ____ '), nl,
    write(' / __) (  _  ) (  )   (  )     /__\\   (  _ \\ / __) ( ___)'), nl,
    write('( (__   )(_)(   )(__   )(__   /(__)\\   )___/ \\__ \\  )__) '), nl,
    write(' \\___) (_____) (____) (____) (__)(__) (__)   (___/ (____)'), nl,
    write('-------------------------------------------------------------------------------------'), nl,
    write('Mode selection '), nl,
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
    select_rule(Rule),
    setup_game(human, human, Rule).
handle_menu_option(2) :-
    select_rule(Rule),
    select_difficulty(Difficulty, ''),
    setup_game(human, computer(Difficulty), Rule).
handle_menu_option(3) :-
    select_difficulty(Difficulty, ''),
    select_rule(Rule),
    setup_game(computer(Difficulty), human, Rule).
handle_menu_option(4) :-
    select_difficulty(Difficulty_1, ' 1'),
    select_difficulty(Difficulty_2, ' 2'),
    select_rule(Rule),
    setup_game(computer(Difficulty_1), computer(Difficulty_2), Rule).
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
    repeat,
    read(DifOption),
    (   member(DifOption, [1, 2]) -> Difficulty = DifOption
    ;   write('Invalid option. Please try again.'), nl,
        fail
    ).

% Displays a menu to select the rule
select_rule(Rule) :-
    write('-------------------------------------------------------------------------------------'), nl,
    write('Rule Selection:'), nl,
    write('1. Normal Rule (Diagonals only allowed in certain cells)'), nl,
    write('2. Easy Rule (Diagonals allowed everywhere)'), nl,
    write('-------------------------------------------------------------------------------------'), nl,
    write('Choose an option: '),
    repeat,
    read(RuleOption),
    (   member(RuleOption, [1, 2]) -> Rule = RuleOption
    ;   write('Invalid option. Please try again.'), nl,
        fail
    ).

%------------------------------------------------------------------------------------------------------------

% Sets up the game configuration and starts the initial state
setup_game(Player1Type, Player2Type, Rule) :-
    GameConfig = [player1:Player1Type, player2:Player2Type, Rule],
    initial_state(GameConfig, GameState),
    !, % Prevent fallback to other clauses
    game_loop(GameConfig, GameState).

game_loop(GameConfig, GameState) :-
    display_game(GameState),

    % Check if the game is over
    (   game_over(GameState, Winner)
    ->  write('Game over! Winner: '), write(Winner), halt, !  % Announce the winner and stop the loop
    ;   % Otherwise, continue the game
        current_player(GameState, Player),  % Get the current player (player1 or player2)
        get_player_type(GameConfig, Player, PlayerType),  % Get the player type from GameConfig
        choose_move(GameState, PlayerType, Move),
        move(GameState, Move, NewGameState),
        game_loop(GameConfig, NewGameState)
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
