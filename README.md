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

The map configuration system using JSON:

```py
def load_map_from_json(file_path):
    with open(file_path, 'r') as f:
        data = json.load(f)

    parsed_costs = {}
    for k, v in data.get("room_costs", {}).items():
        room1, room2 = k.strip("()").split(", ")
        parsed_costs[(room1, room2)] = v

    return {
        "doors": data.get("doors", {}),
        "keys_in_rooms": data.get("keys_in_rooms", {}),
        "room_coords": data.get("room_coords", {}),
        "trap_room": data.get("trap_room", ""),
        "trap_limit": data.get("trap_limit", 1),
        "inventory_limit": data.get("inventory_limit", 3),
        "max_moves": data.get("max_moves", 50),
        "room_costs": parsed_costs
    }
```

### Prolog Bridge Implementation

The Python-Prolog integration layer:

```py
class PrologBridge:
    def __init__(self):
        self.prolog = Prolog()
        self._configure_prolog()
        self._load_essential_files()
    
    def _configure_prolog(self):
        """Optimized configuration that doesn't require .pl file changes"""
        try:
            self.prolog.query("set_prolog_flag(trail_limit, 8_589_934_592)")
        except Exception as e:
            print(f"Config warning: {str(e)}")

    def find_escape_plan(self, time_limit=120):
        """Resource-protected search without changing Prolog code"""
        try:
            self.prolog.query("garbage_collect")
            self.prolog.query(f"set_prolog_flag(stack_limit, 8_589_934_592)")
            
            query = """
                with_output_to(
                    codes(Codes),
                    find_escape_solution
                ),
                atom_codes(Output, Codes)
            """
            
            start_time = time.time()
            result = list(self.prolog.query(query))
            exec_time = time.time() - start_time
            
            if not result:
                print(f"Search finished (limit: {time_limit}s)")
                return None
                
            solution = self._parse_solution(result[0]['Output'])
            print(f"Search completed in {exec_time:.2f}s")
            return solution
            
        except Exception as e:
            print(f"Controlled error: {str(e)}")
            return None
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

The A* search algorithm implementation with game-specific constraints:

```py
def a_star_escape(start, goal, game_data):
    doors = game_data["doors"]
    keys_in_rooms = game_data["keys_in_rooms"]
    room_coords = game_data["room_coords"]
    trap_room = game_data["trap_room"]
    trap_limit = game_data["trap_limit"]
    inventory_limit = game_data["inventory_limit"]
    max_moves = game_data["max_moves"]
    room_costs = game_data["room_costs"]
    
    open_list = []
    heapq.heappush(open_list, (0, 0, start, [], [], {trap_room: 0})  # f, g, current, path, inventory, visits

    while open_list:
        f, g, current, path, inventory, visits = heapq.heappop(open_list)

        if current == goal:
            return path + [current], g

        new_path = path + [current]
        new_inventory = inventory.copy()
        new_visits = visits.copy()

        # Collect keys in current room
        for key in keys_in_rooms.get(current, []):
            if key not in new_inventory and len(new_inventory) < inventory_limit:
                new_inventory.append(key)

        # Handle trap room visits
        if current == trap_room:
            new_visits[trap_room] += 1
            if new_visits[trap_room] > trap_limit:
                continue

        # Explore neighbors
        for neighbor, door in doors.get(current, {}).items():
            if door == "unlocked":
                passable = True
            elif door.startswith("locked"):
                key_needed = door.split("(")[1].split(")")[0]
                passable = has_key(new_inventory, key_needed)
            else:
                passable = False

            if passable:
                move_cost = room_costs.get((current, neighbor), 1)
                g_new = g + move_cost
                if g_new > max_moves:
                    continue
                h = manhattan_heuristic(neighbor, goal)
                f_new = g_new + h
                heapq.heappush(
                    open_list,
                    (f_new, g_new, neighbor, new_path, new_inventory.copy(), new_visits.copy())
                )

    return None, 0
```

###  Heuristic Functions

Pathfinding heuristic implementations:

```py
def manhattan_heuristic(current, goal):
    x1, y1 = room_coords.get(current, (0, 0))
    x2, y2 = room_coords.get(goal, (0, 0))
    return abs(x1 - x2) + abs(y1 - y2)

def has_key(inventory, key_needed):
    return key_needed in inventory
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
