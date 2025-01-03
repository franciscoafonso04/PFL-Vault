:- module(menu, [display_main_menu/3]).


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
    repeat,
    read(Option),
    (
        member(Option, [1, 2, 3, 4, 5]) -> handle_menu_option(Option, Player1Type, Player2Type, Rule)
    ;   write('Invalid option. Please try again'),
        fail
    ).

% Handle user input for the menu
handle_menu_option(1, human, human, Rule) :-
    select_rule(Rule).
handle_menu_option(2, human, computer(Difficulty), Rule) :-
    select_difficulty(Difficulty, ''),
    select_rule(Rule).
handle_menu_option(3, computer(Difficulty), human, Rule) :-
    select_difficulty(Difficulty, ''),
    select_rule(Rule).
handle_menu_option(4, computer(Difficulty_1), computer(Difficulty_2), Rule) :-
    select_difficulty(Difficulty_1, ' 1'),
    select_difficulty(Difficulty_2, ' 2'),
    select_rule(Rule).
handle_menu_option(5) :-
    write('Goodbye!'), nl, !.
handle_menu_option(_) :- nl, display_main_menu.

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
    write('Mode Selection:'), nl,
    write('1. Normal Mode - Fanorona Board'), nl,
    write('2. Free Mode - Omnidirectional'), nl,
    write('-------------------------------------------------------------------------------------'), nl,
    write('Choose an option: '),
    repeat,
    read(RuleOption),
    (   member(RuleOption, [1, 2]) -> Rule = RuleOption
    ;   write('Invalid option. Please try again.'), nl,
        fail
    ).
