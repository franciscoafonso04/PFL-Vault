# **Collapse_5**

## **Identification of the Game and Group**

| Student Number | Full Name                | Contribution (%) | Tasks Performed                                     |
|----------------|--------------------------|------------------|---------------------------------------------------|
| up202208028    | Alexandre Gonçalves Ramos        | 50 | [Descrição breve das tarefas realizadas]          |
| up202208115   | Francisco Miguel Pires Afonso        | 50 | [Descrição breve das tarefas realizadas]          |


## **Installation and Execution**

To correctly install and execute the game **Collapse_5**, follow these steps:

### **Prerequisites**
- **SICStus Prolog 4.9** must be installed.

### **Steps for Linux**

1. Extract the ZIP file containing the project:

   ```bash
   unzip PFL_TP2_T01_Collapse_5.zip
   ```
2. Navigate to the `src` folder:
   ```bash
   cd PFL_TP2_T01_Collapse_5/src
   ```
3. Start SICStus Prolog by typing:
   ```bash
   sicstus
   ```
4. Load the main file:
    ```prolog
    [game].
   ```
5. If the main menu does not open automatically, start the game using:
   ```prolog
   play.
   ```

### **Steps for Windows**

1. Extract the ZIP file containing the project to a folder, such as `PFL_TP2_T01_Collapse_5`.

2. Open the **SICStus Prolog** application.
3. Navigate to the `src` folder in SICStus and consult the `game.pl` file. 
4. If the main menu does not load automatically, use the command:
    ```prolog
    play.
    ```

## **Description of the Game**

**Collapse** is a strategic two-player game which objective is to prevent your opponent from making a valid capture on their turn. The game is played on a **9x5 grid board**, traditionally used in Fanorona, and involves capturing opponent pieces through collisions.

### **Game Rules**

1. **Setup**:
   - The game starts with pieces placed around the perimeter of a 9x5 board.
   - Players decide their colors (white or black).
   - If playing against a bot, the Player can choose between **white** or **black** by choosing between the **Human vs Computer** and the **Computer vs Human**, respectively.
   - The White player makes the first move, and turns alternate between players.

2. **Gameplay**:
   - On their turn, the active player must move one of their pieces to capture exactly one opponent piece.
   - The piece moves in a straight line along the grid until it collides with an opponent's piece.
   - Capturing rules:
     - The move must result in the capture of one opponent piece.
     - The piece cannot change direction during the move.
     - The piece cannot move in a direction where there is no opponent piece to collide with.
   - Captured pieces are removed from the board.

3. **Game End**:
   - The game ends when a player is unable to make a valid capture on their turn.
   - This includes situations where a player loses all their pieces.
   - The other player is declared the winner.

### **Links for Reference**
- Official rules and design inspiration by Kanare Kato:`
  - [Official Collapse Information from Kanare Abstract](https://kanare-abstract.com/en/pages/collapse)
  - [PDF of Rules (from Kanare Kato)](https://example.com/rules.pdf)
  - [Fanorona Board Information](https://en.wikipedia.org/wiki/Fanorona)


## **Considerations for Game Extensions**

During the development of **Collapse_5**, we focused on providing flexibility in gameplay through the implementation of **optional rules**. These rules allow players to adjust the complexity of the game while maintaining its strategic essence. 

### **1. Normal Rule**
In the standard mode, diagonal moves are restricted to cells where the sum of the coordinates `(X + Y)` is an even number. This restriction aligns with the design of the Fanorona board, where certain diagonal connections are only valid in specific positions. This rule encourages players to carefully plan their moves while navigating these limitations, adding a layer of strategic depth to the game.

### **2. Easy Rule**
For players seeking a simpler experience, the easy mode allows diagonal moves on any cell, removing the restriction of coordinate parity. This mode is ideal for novice players or those who prefer a more relaxed approach to the game.

### **Variable Board Sizes**
The possibility of introducing variable board sizes was considered but deemed unnecessary. The fixed board size ensures a pre-defined arrangement of pieces, which is central to the game's balance and strategic design. Increasing the board size would require significant changes to the initial setup and could distort the core gameplay experience, making it less aligned with the original intent of the game.

Com base no código que analisámos, aqui está uma descrição detalhada para a secção **Game Logic** do README. Segue o formato sugerido e personalizámos conforme o teu projeto:

Claro! Vou aprofundar cada uma das secções mencionadas, referindo funções relevantes do teu código para uma descrição mais técnica e detalhada.

## **Game Logic**

### **Game Configuration Representation**
The game's configuration is defined by:
- **Player Types**: Defined using `player1` and `player2`, which can be either `human` or `computer(Difficulty)` where `Difficulty` represents the AI level.
- **Rule**: A numerical identifier for the diagonal movement rule:
  - `1`: Normal Rule (diagonals only allowed in specific cells).
  - `2`: Easy Rule (diagonals allowed everywhere).

The configuration is passed to the `setup_game/3` predicate, which calls `initial_state/2` to initialize the board and the starting player.

**Relevant Functions**:
- `setup_game/3`: Creates the game configuration.
- `initial_state/2`: Initializes the game board, the current player, and the active rule.

**Example**:
```prolog
setup_game(human, computer(1), 1).
```

This configuration sets up a game where:
- Player 1 is human.
- Player 2 is an AI at difficulty level 1.
- The Normal Rule is applied.

---

### **Internal Game State Representation**
The game state is represented internally using the `game_state(Board, CurrentPlayer, Rule)` structure, where:
- **Board**: A 2D list where each cell contains `black`, `white`, or `empty`.
- **Current Player**: Either `player1` or `player2`.
- **Rule**: Indicates the diagonal rule (`1` or `2`).

**Initial Game State**:
The `initial_state/2` predicate creates the initial board layout:
- The first and last rows are populated with alternating `black` and `white` pieces.
- Other rows are populated with `empty` cells or specific piece configurations.

**Example**:
```prolog
initial_state([player1:human, player2:computer(1), rule(1)], GameState).
```

**Intermediate Game State**:
After a few moves, the board might look like this:
```prolog
game_state(
    [
        [black, white, empty, white, black, empty, black, white, black],
        [white, empty, empty, empty, black, empty, empty, empty, white],
        [empty, empty, empty, empty, empty, empty, empty, empty, empty],
        [black, empty, empty, empty, empty, empty, empty, empty, black],
        [white, black, white, black, white, black, white, black, white]
    ],
    player2,
    1
).
```

**Relevant Functions**:
- `initial_state/2`: Sets up the board and starting player.
- `current_player/2`: Retrieves the current player from the game state.
- `switch_player/2`: Alternates between `player1` and `player2` after a move.

---

### **Move Representation**
Moves are represented as `move(Row, Col, Dir)` where:
- `Row` and `Col` indicate the starting position of the piece.
- `Dir` specifies the direction of movement:
  - Possible values: `north`, `south`, `east`, `west`, `northeast`, `northwest`, `southeast`, `southwest`.

The `move/3` predicate performs a move by:
1. **Validation**: Ensures the move is valid using `valid_moves/2` and `valid_move/5`.
2. **Board Update**: Executes the move and captures pieces using `capture_move/5` and `replace_cell/5`.
3. **State Update**: Switches the current player using `switch_player/2`.

**Example**:
```prolog
move(game_state(Board, player1, 1), move(1, 2, southeast), NewGameState).
```

**Relevant Functions**:
- `valid_moves/2`: Generates all valid moves for the current game state.
- `valid_move/5`: Checks if a specific move is valid based on the rules.
- `capture_move/5`: Captures opponent pieces and updates the board.
- `replace_cell/5`: Updates a specific cell on the board.

---

### **User Interaction**
The interaction system includes:
1. **Main Menu**:
   - Players choose the game mode (e.g., Human vs Human, Computer vs Computer).
   - Implemented using `handle_menu_option/1`.

2. **Rule Selection**:
   - Players choose between the Normal Rule and the Easy Rule.
   - Handled by the `select_rule/1` predicate.

3. **Move Input**:
   - Players input their move in the format `move(Row, Col, Dir)`.
   - Input is validated using `valid_moves/2`, and invalid inputs prompt the user to retry.

4. **AI Moves**:
   - The AI selects moves based on its difficulty level:
     - **Level 1**: Random move.
     - **Level 2**: Strategic move that maximizes its advantage.

**Relevant Functions**:
- `display_main_menu/0`: Displays the main menu.
- `select_rule/1`: Allows rule selection.
- `get_valid_move/3`: Prompts the player for a valid move or processes AI moves.
- `choose_move/3`: Determines the AI's move based on difficulty.

---

### **Example Flow**
1. The game begins with the main menu:
   ```prolog
   display_main_menu.
   ```

2. The player selects **Human vs Computer**, chooses AI difficulty, and selects the rule:
   ```prolog
   handle_menu_option(2).
   ```

3. The game starts, and the current board is displayed. Players alternate turns, inputting moves:
   ```prolog
   move(2, 3, southeast).
   ```

4. The game continues until a player has no valid moves or captures all opponent pieces.

---

### **Summary**
The game's logic is modular, with clear separation of concerns for board management, move validation, and user interaction. This structure ensures that the game is robust and extendable for future enhancements.

---

Se quiseres, posso incluir exemplos de entradas e saídas adicionais ou ajustar mais detalhes. O que achas?
