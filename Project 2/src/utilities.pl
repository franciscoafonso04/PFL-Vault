% Module declaration and exported predicates
:- module(utilities, [valid_direction/4, within_bounds/2, next_position/5, current_player/2, switch_player/2, player_piece/2, opponent_piece/2, player_profile/2, get_player_type/3, convert_row/2]).
:- use_module(library(lists)).  % For list operations

% -------------------------------------------------------------------------------------------------
% VALIDATION UTILITIES

% valid_direction(+Rule, +Row, +Col, +Dir)
% Determines whether a given direction is valid based on the rule and position.
% Rule 1: Diagonal moves are allowed only on cells where (Row + Col) is even.
% Rule 2: Diagonal moves are allowed everywhere.
valid_direction(1, Row, Col, Dir) :-
    (
        member(Dir, [north, south, west, east])                     % Cardinal directions are always valid
    ;
        member(Dir, [northeast, northwest, southeast, southwest]),  % Diagonal directions
        (Row + Col) mod 2 =:= 0                                     % Diagonal moves allowed only on even (Row + Col)
    ).

valid_direction(2, _, _, Dir) :-
        member(Dir, [north, south, west, east, northeast, northwest, southeast, southwest]).    % All directions allowed

% within_bounds(+Row, +Col)
% Checks if a given position (Row, Col) is within the board's bounds.
within_bounds(Row, Col) :-
    Row > 0, Row =< 5,  % Valid row range is 1 to 5
    Col > 0, Col =< 9.  % Valid column range is 1 to 9

% -------------------------------------------------------------------------------------------------
% POSITIONAL UTILITIES

% next_position(+Row, +Col, +Dir, -NextRow, -NextCol)
% Computes the next position based on the current position and direction.
next_position(Row, Col, north, NextRow, Col) :- 
    NextRow is Row - 1.
next_position(Row, Col, south, NextRow, Col) :- 
    NextRow is Row + 1.
next_position(Row, Col, east, Row, NextCol) :- 
    NextCol is Col + 1.
next_position(Row, Col, west, Row, NextCol) :- 
    NextCol is Col - 1.
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

% -------------------------------------------------------------------------------------------------
% GAME STATE UTILITIES

% current_player(+GameState, -Player)
% Extracts the current player from the game state.
current_player(game_state(_, Player, _), Player).

% switch_player(+CurrentPlayer, -NextPlayer)
% Determines the next player to play.
switch_player(player1, player2).
switch_player(player2, player1).

% player_piece(+Player, -Piece)
% Maps a player to their piece color.
player_piece(player1, white).
player_piece(player2, black).

% opponent_piece(+Player, -Piece)
% Maps a player to their opponent's piece color.
opponent_piece(player1, black).
opponent_piece(player2, white).

% player_profile(+Player, -Profile)
% Provides a string representation of the player's profile.
player_profile(player1, 'Player 1 - O').    % 'O' represents Player 1 piece
player_profile(player2, 'Player 2 - U').    % 'U' represents Player 2 piece

% -------------------------------------------------------------------------------------------------
% CONFIGURATION UTILITIES

% get_player_type(+GameConfig, +Player, -PlayerType)
% Extracts the player type (human, computer(Level)) from the game configuration.
get_player_type([Player1Type, _, _], player1, Player1Type).
get_player_type([_, Player2Type, _], player2, Player2Type).

% convert_row(+LogicalRow, -PhysicalRow)
% Converts a logical row index (1 to 5) to a physical row index (5 to 1) for display.
convert_row(1, 5).
convert_row(2, 4).
convert_row(3, 3).
convert_row(4, 2).
convert_row(5, 1).
convert_row(_, _) :- false.