:- module(utilities, [valid_direction/1, within_bounds/3, next_position/5, switch_player/2, player_piece/2, opponent_piece/2, get_player_type/3, announce_winner/1]).
:- use_module(library(lists)).

% Valid directions for movement
valid_direction(north).
valid_direction(south).
valid_direction(east).
valid_direction(west).
valid_direction(northeast).
valid_direction(northwest).
valid_direction(southeast).
valid_direction(southwest).

% Checks if a position is within the bounds of the board
within_bounds(Board, Row, Col) :-
    length(Board, NumRows),
    Row > 0, Row =< NumRows, % Check row bounds
    nth0(1, Board, FirstRow), % Get the first row to check column bounds
    length(FirstRow, NumCols),
    Col > 0, Col =< NumCols. % Check column bounds

% Computes the next position based on the current position and direction
next_position(Row, Col, north, NextRow, Col) :- NextRow is Row - 1.
next_position(Row, Col, south, NextRow, Col) :- NextRow is Row + 1.
next_position(Row, Col, east, Row, NextCol) :- NextCol is Col + 1.
next_position(Row, Col, west, Row, NextCol) :- NextCol is Col - 1.
next_position(Row, Col, northeast, NextRow, NextCol) :-
    NextRow is Row - 1,
    NextCol is Col + 1.
next_position(Row, Col, northwest, NextRow, NextCol) :-
    NextRow is Row - 1,
    NextCol is Col - 1.
next_position(Row, Col, southeast, NextRow, NextCol) :-
    NextRow is Row + 1,
    NextCol is Col + 1.
next_position(Row, Col, southwest, NextRow, NextCol) :-
    NextRow is Row + 1,
    NextCol is Col - 1.

% Switches the player
switch_player(player1, player2).
switch_player(player2, player1).

% Player Piece Colors
player_piece(player1, black).
player_piece(player2, white).

% Opponent Piece Colors
opponent_piece(player1, white).
opponent_piece(player2, black).

% Player Type
get_player_type(GameConfig, player1, Player1Type) :- member(player1:Player1Type, GameConfig).
get_player_type(GameConfig, player2, Player2Type) :- member(player2:Player2Type, GameConfig).

% Announces the winner
announce_winner(Winner) :-
    write('Game over! Winner: '), write(Winner), nl.