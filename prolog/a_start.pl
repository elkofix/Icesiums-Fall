:- module(a_star, [
    find_escape_solution/0
]).

:- use_module(state).
:- use_module(facts).
:- use_module(constraints).

% Main function to find escape solution using A*
find_escape_solution :-
    writeln('Searching for escape solution using A*...'),
    % Get the starting room and final room
    state:player_location(StartRoom),
    facts:final_room(FinalRoom),
    format('Planning escape from ~w to ~w~n', [StartRoom, FinalRoom]),
    
    % Define the initial state based on players current location
    % State format: state(CurrentRoom, Inventory, UnlockedDoors, SolvedPuzzles, MovedObjects, CollectedPieces)
    InitialState = state(StartRoom, [], [], [], [], []),
    
    % Run A* to find a solution
    (a_star(InitialState, GoalState, Solution) ->
        format('Solution found! Steps to escape: ~n'),
        print_solution(Solution),
        format('Total steps required: ~w~n', [length(Solution)])
    ;
        writeln('No escape solution found! The room might be unsolvable.')
    ).

% A* implementation to find optimal path from InitialState to the final room
a_star(InitialState, GoalState, Steps) :-
    % Get the defined final room
    facts:final_room(FinalRoom),
    
    % Calculate initial heuristic value
    calculate_heuristic(InitialState, H),
    % PriorityQueue format: [queueItem(F, State, PathToState)]
    % F = G + H, where G is the current path length and H is the heuristic value
    InitialQueue = [queueItem(H, InitialState, [])],
    a_star_loop(InitialQueue, [], FinalRoom, GoalState, Steps).

% A* loop - empty queue means no solution found
a_star_loop([], _, _, _, _) :- 
    writeln('No solution found!'),
    fail.

% Found goal state - return the path
a_star_loop([queueItem(_, State, Path)|_], Visited, FinalRoom, State, Path) :-
    State = state(Room, _, _, _, _, _),
    Room = FinalRoom, !.  % Goal is to reach the final room

% Process the next state in the queue (lowest F value)
a_star_loop([queueItem(_, State, Path)|Queue], Visited, FinalRoom, GoalState, Steps) :-
    % If already visited this state, skip
    \+ member(State, Visited),
    
    % Get all possible next states and actions from current state
    findall(
        (NextState, Action),
        possible_action(State, NextState, Action),
        Transitions
    ),
    
    % Path cost so far (G value)
    length(Path, G),
    % Next step cost is G+1
    NextG is G + 1,
    
    % Add new states to the queue with their paths and F values
    add_to_queue(Transitions, Path, NextG, Queue, NewQueue),
    
    % Add current state to visited and continue
    a_star_loop(NewQueue, [State|Visited], FinalRoom, GoalState, Steps).

% Skip already visited states
a_star_loop([_|Queue], Visited, FinalRoom, GoalState, Steps) :-
    a_star_loop(Queue, Visited, FinalRoom, GoalState, Steps).

% Add new states to the queue with their paths and F values
add_to_queue([], _, _, Queue, Queue).
add_to_queue([(NextState, Action)|Rest], Path, G, Queue, FinalQueue) :-
    % Add action to path
    append(Path, [Action], NewPath),
    
    % Calculate heuristic for next state
    calculate_heuristic(NextState, H),
    % F = G + H
    F is G + H,
    
    % Insert into queue maintaining priority order (lowest F first)
    insert_queue(queueItem(F, NextState, NewPath), Queue, TempQueue),
    
    % Process remaining transitions
    add_to_queue(Rest, Path, G, TempQueue, FinalQueue).

% Insert queue item in order of F value (lowest first)
insert_queue(Item, [], [Item]) :- !.
insert_queue(Item, [Head|Tail], [Item, Head|Tail]) :-
    Item = queueItem(F1, _, _),
    Head = queueItem(F2, _, _),
    F1 =< F2, !.
insert_queue(Item, [Head|Tail], [Head|NewTail]) :-
    insert_queue(Item, Tail, NewTail).

% Print solution in a readable format
print_solution([]) :- !.
print_solution([Action|Rest]) :-
    format('- ~w~n', [Action]),
    print_solution(Rest).

% Calculate heuristic value for state
% Heuristic combines:
% 1. Room distance to final room (estimated)
% 2. Doors that need to be unlocked
% 3. Puzzles that need to be solved
% 4. Pieces that need to be collected
calculate_heuristic(State, H) :-
    State = state(CurrentRoom, Inventory, UnlockedDoors, SolvedPuzzles, MovedObjects, CollectedPieces),
    
    % Get final room
    facts:final_room(FinalRoom),
    
    % Estimate room distance component
    estimate_room_distance(CurrentRoom, FinalRoom, RoomDistance),
    
    % Count remaining puzzles to solve
    count_remaining_puzzles(SolvedPuzzles, PuzzlesRemaining),
    
    % Count remaining locked doors
    count_locked_doors(UnlockedDoors, DoorsRemaining),
    
    % Count missing pieces
    count_missing_pieces(CollectedPieces, PiecesRemaining),
    
    % Final heuristic is a weighted sum
    % We prioritize: doors > puzzles > pieces > distance
    H is (5 * DoorsRemaining) + (3 * PuzzlesRemaining) + (2 * PiecesRemaining) + RoomDistance.

% Estimating room distance (this is a simplified version)
% In a real implementation, this would use actual graph distance or pre-computed values
estimate_room_distance(CurrentRoom, FinalRoom, 0) :- 
    CurrentRoom = FinalRoom, !.
estimate_room_distance(_, _, 1).  % Simplified - could be improved with actual room layout

% Count puzzles remaining to solve
count_remaining_puzzles(SolvedPuzzles, Remaining) :-
    findall(Puzzle, facts:puzzle_room(Puzzle, _), AllPuzzles),
    subtract(AllPuzzles, SolvedPuzzles, RemainingPuzzles),
    length(RemainingPuzzles, Remaining).

% Count doors remaining to unlock
count_locked_doors(UnlockedDoors, Remaining) :-
    findall((Room1, Room2), facts:door(Room1, Room2, locked), AllLockedDoors),
    subtract(AllLockedDoors, UnlockedDoors, RemainingDoors),
    length(RemainingDoors, Remaining).

% Count missing puzzle pieces
count_missing_pieces(CollectedPieces, Missing) :-
    % Get all puzzle pieces available in the game
    findall((Puzzle, Piece), facts:piece(Puzzle, Piece), AllPieces),
    subtract(AllPieces, CollectedPieces, MissingPieces),
    length(MissingPieces, Missing).

% Define all possible actions from a given state
% State format: state(CurrentRoom, Inventory, UnlockedDoors, SolvedPuzzles, MovedObjects, CollectedPieces)

% Action: Move to another room
possible_action(
    state(CurrentRoom, Inventory, UnlockedDoors, SolvedPuzzles, MovedObjects, CollectedPieces),
    state(NextRoom, Inventory, UnlockedDoors, SolvedPuzzles, MovedObjects, CollectedPieces),
    move_player(NextRoom)
) :-
    % Check if door between rooms exists and is unlocked
    (
        (facts:door(CurrentRoom, NextRoom, unlocked) ; member((CurrentRoom, NextRoom), UnlockedDoors))
        ;
        (facts:door(NextRoom, CurrentRoom, unlocked) ; member((NextRoom, CurrentRoom), UnlockedDoors))
    ).

% Action: Pick up a key
possible_action(
    state(CurrentRoom, Inventory, UnlockedDoors, SolvedPuzzles, MovedObjects, CollectedPieces),
    state(CurrentRoom, [Key|Inventory], UnlockedDoors, SolvedPuzzles, MovedObjects, CollectedPieces),
    pick_key(Key)
) :-
    facts:key_in_room(CurrentRoom, Key),
    \+ member(Key, Inventory),
    % Check inventory limit (simplified version)
    constraints:can_carry(key, KeyLimit),
    count_keys(Inventory, KeyCount),
    KeyCount < KeyLimit.

% Action: Move an object to find a hidden piece
possible_action(
    state(CurrentRoom, Inventory, UnlockedDoors, SolvedPuzzles, MovedObjects, CollectedPieces),
    state(CurrentRoom, Inventory, UnlockedDoors, SolvedPuzzles, [Object|MovedObjects], NewCollectedPieces),
    move_object(Object)
) :-
    facts:object_in_room(CurrentRoom, Object),
    \+ member(Object, MovedObjects),
    % Check if object hides a piece
    (
        % If object hides a piece, add it to collected pieces
        facts:hides_piece(Object, Puzzle, Piece),
        \+ member((Puzzle, Piece), CollectedPieces),
        % Check piece inventory limit
        constraints:can_carry(piece, PieceLimit),
        count_pieces(CollectedPieces, PieceCount),
        PieceCount < PieceLimit,
        % Add piece to collected pieces
        append([(Puzzle, Piece)], CollectedPieces, NewCollectedPieces)
    ;
        % If no piece or cant carry more, just add object to moved objects
        \+ facts:hides_piece(Object, _, _),
        NewCollectedPieces = CollectedPieces
    ).

% Action: Pick up a visible piece
possible_action(
    state(CurrentRoom, Inventory, UnlockedDoors, SolvedPuzzles, MovedObjects, CollectedPieces),
    state(CurrentRoom, Inventory, UnlockedDoors, SolvedPuzzles, MovedObjects, [(Puzzle, Piece)|CollectedPieces]),
    pick_piece(Piece)
) :-
    facts:piece_in_room(CurrentRoom, Piece, Puzzle),
    \+ member((Puzzle, Piece), CollectedPieces),
    % Check piece inventory limit
    constraints:can_carry(piece, PieceLimit),
    count_pieces(CollectedPieces, PieceCount),
    PieceCount < PieceLimit.

% Action: Solve a puzzle
possible_action(
    state(CurrentRoom, Inventory, UnlockedDoors, SolvedPuzzles, MovedObjects, CollectedPieces),
    state(CurrentRoom, Inventory, UnlockedDoors, [Puzzle|SolvedPuzzles], MovedObjects, RemainingPieces),
    solve_puzzle(Puzzle)
) :-
    % Check if puzzle can be solved in this room
    facts:puzzle_room(Puzzle, CurrentRoom),
    \+ member(Puzzle, SolvedPuzzles),
    % Check if player has all pieces for this puzzle
    findall(Piece, facts:piece(Puzzle, Piece), AllPieces),
    all_pieces_collected(Puzzle, AllPieces, CollectedPieces),
    % Remove used pieces from inventory
    remove_puzzle_pieces(Puzzle, CollectedPieces, RemainingPieces).

% Action: Unlock a door
possible_action(
    state(CurrentRoom, Inventory, UnlockedDoors, SolvedPuzzles, MovedObjects, CollectedPieces),
    state(CurrentRoom, RemainingInventory, [(CurrentRoom, NextRoom)|UnlockedDoors], SolvedPuzzles, MovedObjects, CollectedPieces),
    unlock_door(CurrentRoom, NextRoom)
) :-
    % Check if door exists and is locked
    facts:door(CurrentRoom, NextRoom, locked),
    \+ member((CurrentRoom, NextRoom), UnlockedDoors),
    % Check requirements for unlocking
    facts:door_requirements(CurrentRoom, NextRoom, Requirements),
    % Check each requirement
    can_satisfy_requirements(Requirements, Inventory, SolvedPuzzles, ConsumedKeys),
    % Remove consumed keys from inventory
    remove_keys(ConsumedKeys, Inventory, RemainingInventory).

% Helper predicates

% Count keys in inventory
count_keys(Inventory, Count) :-
    include(is_key, Inventory, Keys),
    length(Keys, Count).

% Check if item is a key
is_key(Key) :- atom(Key), sub_atom(Key, 0, 3, _, 'key').

% Count puzzle pieces
count_pieces(CollectedPieces, Count) :-
    length(CollectedPieces, Count).

% Check if all pieces for a puzzle are collected
all_pieces_collected(_, [], _).
all_pieces_collected(Puzzle, [Piece|Rest], CollectedPieces) :-
    member((Puzzle, Piece), CollectedPieces),
    all_pieces_collected(Puzzle, Rest, CollectedPieces).

% Remove used puzzle pieces from inventory
remove_puzzle_pieces(_, [], []).
remove_puzzle_pieces(Puzzle, [(P, Piece)|Rest], Remaining) :-
    P = Puzzle, !,
    remove_puzzle_pieces(Puzzle, Rest, Remaining).
remove_puzzle_pieces(Puzzle, [(P, Piece)|Rest], [(P, Piece)|Remaining]) :-
    P \= Puzzle,
    remove_puzzle_pieces(Puzzle, Rest, Remaining).

% Check if all requirements can be satisfied
can_satisfy_requirements([], _, _, []).
can_satisfy_requirements([has_key(Key)|Rest], Inventory, SolvedPuzzles, [Key|ConsumedKeys]) :-
    member(Key, Inventory),
    can_satisfy_requirements(Rest, Inventory, SolvedPuzzles, ConsumedKeys).
can_satisfy_requirements([puzzle_solved(Puzzle)|Rest], Inventory, SolvedPuzzles, ConsumedKeys) :-
    member(Puzzle, SolvedPuzzles),
    can_satisfy_requirements(Rest, Inventory, SolvedPuzzles, ConsumedKeys).

% Remove consumed keys from inventory
remove_keys([], Inventory, Inventory).
remove_keys([Key|Rest], Inventory, RemainingInventory) :-
    select(Key, Inventory, TempInventory),
    remove_keys(Rest, TempInventory, RemainingInventory).