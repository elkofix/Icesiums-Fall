% search.pl
:- module(search, [
    find_solution/1,
    bfs/3
]).

:- use_module(state).
:- use_module(facts).
:- use_module(rules).
:- use_module(constraints).
:- use_module(library(clpfd)).

% Main predicate to find solution from room A to room D
find_solution(Steps) :-
    % Define the initial state
    % State format: state(CurrentRoom, Inventory, UnlockedDoors, SolvedPuzzles, MovedObjects, CollectedPieces)
    InitialState = state(a, [], [], [], [], []),
    
    % Goal is to reach room d
    bfs(InitialState, GoalState, Steps),
    
    % Verify we reached the goal (room d)
    GoalState = state(d, _, _, _, _, _).

% BFS implementation to find a path from InitialState to a state satisfying the goal
bfs(InitialState, GoalState, Steps) :-
    % Queue format: [queueItem(State, PathToState)]
    InitialQueue = [queueItem(InitialState, [])],
    bfs_loop(InitialQueue, [], GoalState, Steps).

% BFS loop - empty queue means no solution found
bfs_loop([], _, _, _) :- 
    writeln('No solution found!'),
    fail.

% Found goal state - return the path
bfs_loop([queueItem(State, Path)|_], Visited, State, Path) :-
    State = state(d, _, _, _, _, _), !.  % Goal is to reach room d

% Process the next state in the queue
bfs_loop([queueItem(State, Path)|Queue], Visited, GoalState, Steps) :-
    % If already visited this state, skip
    \+ member(State, Visited),
    
    % Get all possible next states and actions from current state
    findall(
        (NextState, Action),
        possible_action(State, NextState, Action),
        Transitions
    ),
    
    % Add new states to the queue with their paths
    add_to_queue(Transitions, Path, Queue, NewQueue),
    
    % Add current state to visited
    bfs_loop(NewQueue, [State|Visited], GoalState, Steps).

% Skip already visited states
bfs_loop([_|Queue], Visited, GoalState, Steps) :-
    bfs_loop(Queue, Visited, GoalState, Steps).

% Add new states to the queue with their paths
add_to_queue([], _, Queue, Queue).
add_to_queue([(NextState, Action)|Rest], Path, Queue, FinalQueue) :-
    % Add action to path
    append(Path, [Action], NewPath),
    % Add new state to queue
    append(Queue, [queueItem(NextState, NewPath)], TempQueue),
    % Process remaining transitions
    add_to_queue(Rest, Path, TempQueue, FinalQueue).

% Define all possible actions from a given state
% State format: state(CurrentRoom, Inventory, UnlockedDoors, SolvedPuzzles, MovedObjects, CollectedPieces)

print_full_solution(Steps) :-
    write_canonical(Steps), nl.

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
        % If no piece or can't carry more, just add object to moved objects
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
is_key(Key) :- facts:key(Key).

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