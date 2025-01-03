% Module declaration with exported predicates
:- module(menu, [display_main_menu/3]).

% -------------------------------------------------------------------------------------------------
% MAIN MENU DISPLAY

% Displays the main menu, allowing the user to select the game mode.
% Receives:
%   -Player1Type: Placeholder for the type of Player 1 (human or computer/bot).
%   -Player2Type: Placeholder for the type of Player 2 (human or computer/bot).
%   -Rule: Placeholder for the selected rule (Normal or Easy).
display_main_menu(Player1Type, Player2Type, Rule) :-
    write('  ___   _____   __     __       __     ____   ___   ____ '), nl,
    write(' / __) (  _  ) (  )   (  )     /__\\   (  _ \\ / __) ( ___)'), nl,
    write('( (__   )(_)(   )(__   )(__   /(__)\\   )___/ \\__ \\  )__) '), nl,
    write(' \\___) (_____) (____) (____) (__)(__) (__)   (___/ (____)'), nl,
    write('-------------------------------------------------------------------------------------'), nl,
    write('Mode selection '), nl,
    write('1. Player Vs Player'), nl,
    write('2. Player Vs Bot'), nl,
    write('3. Bot Vs Player'), nl,
    write('4. Bot Vs Bot'), nl,
    write('5. Exit'), nl,
    write('-------------------------------------------------------------------------------------'), nl,
    write('Choose an option: '),

    % Repeat loop for input validation
    repeat,
    read(Option),
    (
        member(Option, [1, 2, 3, 4, 5]) ->                          % Valid options
        handle_menu_option(Option, Player1Type, Player2Type, Rule)  % Call the corresponding handler
    ;   write('Invalid option. Please try again'),                  % Invalid input
        fail                                                        % Restart the loop
    ).

% -------------------------------------------------------------------------------------------------
% MENU OPTION HANDLING

% Handles the selection of game mode and configures the appropriate game parameters.
% Receives:
%   +Option: The selected menu option.
%   -Player1Type: The type of Player 1.
%   -Player2Type: The type of Player 2.
%   -Rule: The selected rule for the game.

% Option 1: Player vs Player
handle_menu_option(1, human, human, Rule) :-
    select_rule(Rule).                  % Select game rule

% Option 2: Player vs Bot
handle_menu_option(2, human, computer(Difficulty), Rule) :-
    select_difficulty(Difficulty, ''),      % Select difficulty level for the bot
    select_rule(Rule).                      % Select game rule

% Option 3: Bot vs Player
handle_menu_option(3, computer(Difficulty), human, Rule) :-
    select_difficulty(Difficulty, ''),      % Select difficulty level for the bot
    select_rule(Rule).                      % Select game rule

% Option 4: Bot vs Bot
handle_menu_option(4, computer(Difficulty_1), computer(Difficulty_2), Rule) :-
    select_difficulty(Difficulty_1, ' 1'),  % Select difficulty for Bot 1
    select_difficulty(Difficulty_2, ' 2'),  % Select difficulty for Bot 2
    select_rule(Rule).                      % Select game rule

% Option 5: Exit the game
handle_menu_option(5) :-
    write('Goodbye!'), nl, !.               % Terminate the program

% -------------------------------------------------------------------------------------------------
% DIFFICULTY SELECTION

% Displays a menu to select the difficulty level for the AI.
% Receives:
%   -Difficulty: The selected difficulty level (1 for Easy, 2 for Hard).
%   +Computer: The identifier of the computer/bot (e.g., "1" or "2").
select_difficulty(Difficulty, Computer) :-
    write('-------------------------------------------------------------------------------------'), nl,
    write('Computer'), write(Computer), write(' Level Selection:'), nl,
    write('1. Level 1'), nl,
    write('2. Level 2'), nl,
    write('-------------------------------------------------------------------------------------'), nl,
    write('Choose an option'),
    repeat,
    read(DifOption),
    (   member(DifOption, [1, 2]) ->                    % Valid difficulty levels
        Difficulty = DifOption
    ;   write('Invalid option. Please try again.'), nl, % Invalid input
        fail                                            % Restart the loop
    ).

% -------------------------------------------------------------------------------------------------
% RULE SELECTION

% Displays a menu to select the game rule.
% Receives:
%   -Rule: The selected rule (1 for Normal, 2 for Easy).
select_rule(Rule) :-
    write('-------------------------------------------------------------------------------------'), nl,
    write('Rule Selection:'), nl,
    write('1. Normal Rule - Fanorona Board'), nl,
    write('2. Easy Rule - Omnidirectional'), nl,
    write('-------------------------------------------------------------------------------------'), nl,
    write('Choose an option'),
    repeat,
    read(RuleOption),
    (   member(RuleOption, [1, 2]) ->                   % Valid rules
        Rule = RuleOption
    ;   write('Invalid option. Please try again.'), nl, % Invalid input
        fail                                            % Restart the loop
    ).
