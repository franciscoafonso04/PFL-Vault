# **PFL_TP2_T01_Collapse_5**

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

---
