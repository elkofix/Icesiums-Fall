# Escape Room Solver Report

## 📌 Introduction

This project solves an escape room-like maze using both logical and heuristic search algorithms. Implementations in **Prolog** and **Python** allow comparison between exhaustive (BFS) and heuristic-driven (A*) search strategies. The environment includes doors, keys, traps, coordinates, and constraints that increase the agent's challenge in navigating from a start to a goal node.

---

## ⚙️ Methodology

We followed an iterative methodology:

1. The maze was represented as a graph with logical and physical constraints.
2. A **BFS** solution was implemented in Prolog for completeness.
3. A **heuristic A\*** algorithm was implemented in Python for efficiency.
4. We created a **Prolog-Python bridge** using `pyswip`, allowing Python to call Prolog predicates directly.
5. The system supports dynamic map loading using `.json` files.

---

## 🧩 Implementation

### 📁 Project Structure

![image](https://github.com/user-attachments/assets/af736137-34ac-4db2-b05a-06365f5714da)

### Game State Management

The state module handles all dynamic game state information:

```pl
% State representation with dynamic predicates
:- dynamic player_location/1.
:- dynamic inventory/1.
:- dynamic door_state/3.
:- dynamic has_piece/2.
:- dynamic object_moved/1.
:- dynamic puzzle_solved/1.
:- dynamic key_dropped/2.
:- dynamic piece_dropped/3.

% Move player with constraint checks
move_player(NewRoom) :-
    constraints:check_move_limit,
    player_location(Current),
    (door_state(Current, NewRoom, unlocked) ; door_state(NewRoom, Current, unlocked)),
    constraints:increment_move_count,
    constraints:increment_room_turn(NewRoom),
    retract(player_location(Current)),
    assertz(player_location(NewRoom)),
    (facts:final_room(NewRoom) ->
        writeln("*   CONGRATULATIONS! YOU HAVE ESCAPED!     *")
    ;
        true
    ).
```
### Dynamic Constraints System

The constraints module implements flexible game rules:

```pl
% Dynamic constraint predicates
:- dynamic max_moves/1.
:- dynamic max_b_visits/1.
:- dynamic can_carry/2.
:- dynamic trap/2.

% Set constraints dynamically
set_max_moves(Limit) :-
    retractall(max_moves(_)),
    assertz(max_moves(Limit)).

% Check inventory limits
check_inventory_limit(Type) :-
    can_carry(Type, Limit),
    count_items_of_type(Type, Count),
    Count < Limit.

% Initialize room turn counters
initialize_constraints :-
    retractall(move_count(_)),
    assertz(move_count(0)),
    retractall(turns_in_room(_, _)),
    findall(Room, facts:room(Room), Rooms),
    forall(member(R, Rooms), assertz(turns_in_room(R, 0))).
```

### Game Configuration

The facts module supports both predefined and custom game setups:

```pl
% Dynamic game configuration
create_custom_game :-
    clear_game_data,
    custom_game_rooms,    % Set up rooms
    custom_game_doors,    % Set up connections
    custom_game_keys,     % Place keys
    custom_game_objects,  % Add interactive objects
    custom_game_puzzles.  % Define puzzles

% Example room setup
custom_game_rooms :-
    writeln('Enter room names, one at a time:'),
    read(RoomInput),
    (RoomInput = done -> true ;
     add_room(RoomInput),
     custom_game_rooms).
```

### Search Algorithms

The search modules implement pathfinding with and without constraints:

```pl
% BFS implementation with constraints
bfs(InitialState, GoalState, Steps) :-
    facts:final_room(FinalRoom),
    InitialQueue = [queueItem(InitialState, [])],
    bfs_loop(InitialQueue, [], FinalRoom, GoalState, Steps).

% State transition with constraints
possible_action(
    state(CurrentRoom, Inventory, UnlockedDoors, SolvedPuzzles, MovedObjects, CollectedPieces),
    state(CurrentRoom, [Key|Inventory], UnlockedDoors, SolvedPuzzles, MovedObjects, CollectedPieces),
    pick_key(Key)
) :-
    facts:key_in_room(CurrentRoom, Key),
    \+ member(Key, Inventory),
    constraints:can_carry(key, KeyLimit),
    count_keys(Inventory, KeyCount),
    KeyCount < KeyLimit.
```

### Interactive Commands

The main module handles player interactions:

```pl
% Player command processing
pick_piece(Piece) :-
    constraints:check_inventory_limit(piece),
    player_location(Room),
    (facts:piece_in_room(Room, Piece, Puzzle) ; piece_dropped(Piece, Puzzle, Room)),
    \+ has_piece(Puzzle, Piece),
    assertz(has_piece(Puzzle, Piece)).

solve_puzzle(Puzzle) :-
    player_location(Room),
    facts:puzzle_room(Puzzle, Room),
    findall(Piece, facts:piece(Puzzle, Piece), AllPieces),
    forall(member(P, AllPieces), has_piece(Puzzle, P)),
    forall(member(P, AllPieces), retract(has_piece(Puzzle, P))),
    assertz(puzzle_solved(Puzzle)).
```

## 🔗 Prolog-Python Bridge

Implemented via the [`pyswip`](https://github.com/yuce/pyswip) library, allowing Python scripts to load `.pl` files and execute Prolog queries programmatically. This enables a hybrid approach where logic reasoning and UI/heuristic computation coexist.

---

## ⚙️ Algorithms

### 🔍 BFS (Breadth-First Search in Prolog)

- Fully exhaustive and complete.
- Navigates while respecting constraints: locked doors, needed keys, and traps.
- Can be **very memory intensive**, recommended to run with increased stack limit:

```bash
swipl --stack-limit=8g
```

### 🔸 A* (A-Star Search in Python)
Uses Manhattan Distance as a heuristic.
```python
def manhattan_heuristic(current, goal):
    x1, y1 = room_coords.get(current, (0, 0))
    x2, y2 = room_coords.get(goal, (0, 0))
    return abs(x1 - x2) + abs(y1 - y2)
```


Considers movement cost, inventory limits, key-door dependencies, and traps.

Significantly faster for large maps or constraint-heavy environments.


#### 🧪 Results
|Algorithm | Language | Completeness | Optimality | Performance|
|----------|----------|------------|------------|-------------|
|BFS | Prolog | ✅ Yes | ✅ Yes | ❌ Slow on large maps|
|A* | Python | ✅ Yes | ✅ Yes | ✅ Fast and memory-efficient|

----

### 🤖 [Placeholder: Adversarial Search]
This section is reserved for future implementation using Adversarial Search with Minimax or Alpha-Beta Pruning techniques to simulate competitive escape scenarios or dynamic threats.

-----

## ▶️ Running the Game

### From prolog

```bash
swipl --stack-limit=8g

?- [prolog/facts].
?- [prolog/rules].
?- [prolog/search].

% Sample execution
?- take_key(a).
?- unlock_door(a, b).
?- bfs(a, e, Path)
```
> ⚠️ Warning: BFS requires a lot of memory due to path branching and constraint checking. Always use the **--stack-limit=8g** flag.

### From python 🐍

```bash
python main.py
```

You'll see the main menu:

```bash
===== Initial Menu =====
1. BFS (Prolog)
2. A* (Python)
0. Exit
Select an option: 
```
> It's very intuitive!

Depending on your selection:
- Option 1 will run the BFS search using Prolog via the bridge.
- Option 2 runs the A* algorithm with optional path visualization, map loading, or goal selection.

✅ Conclusions
- Successfully implemented a solver using both logical and heuristic search.

- The agent can navigate a maze with complex real-world constraints.

- The bridge between Prolog and Python offers flexibility and a great learning opportunity.

- A* shows better performance in constrained environments but requires a well-designed heuristic.

- The architecture allows easy extension, testing, and future AI integrations.
