% constraints.pl
:- module(constraints, [
    max_moves/1,
    max_b_visits/1,
    valid_state/4,
    can_carry/2,
    trap/2, 
    turns_in_room/2,
    move_count/1,
    increment_move_count/0,
    increment_room_turn/1,
    check_move_limit/0,
    check_trap/1,
    check_inventory_limit/1,
    initialize_constraints/0,
    count_items_of_type/2
]).

:- use_module(library(clpfd)).
:- use_module(facts).
:- use_module(state).

:- dynamic max_moves/1.
:- dynamic max_b_visits/1.
:- dynamic can_carry/2.
:- dynamic trap/2.
:- dynamic move_count/1.
:- dynamic turns_in_room/2.

% Default constraints
max_moves(30).  % Default maximum number of moves
max_b_visits(3).  % Maximum visits to room B

% Inventory limits for different item types
can_carry(key, 2).    % Can carry up to 2 keys
can_carry(piece, 4).  % Can carry up to 4 puzzle pieces

% Room traps
trap(c, turns(3)).  % Room C has a trap that activates after 3 turns

% Current move counter
move_count(0).

% Turns spent in each room
turns_in_room(a, 0).
turns_in_room(b, 0).
turns_in_room(c, 0).
turns_in_room(d, 0).

% Increment the global move counter
increment_move_count :-
    retract(move_count(Count)),
    NewCount #= Count + 1,
    assertz(move_count(NewCount)).

% Increment the turn counter for a specific room
increment_room_turn(Room) :-
    retract(turns_in_room(Room, Count)),
    NewCount #= Count + 1,
    assertz(turns_in_room(Room, NewCount)),
    check_trap(Room).

% Check if move limit has been reached
check_move_limit :-
    move_count(Count),
    max_moves(Max),
    Count #< Max.  % Only check, don't show message yet
check_move_limit :-
    move_count(Count),
    max_moves(Max),
    Count #>= Max,
    writeln("You've reached the maximum number of moves!"),
    writeln("Game over. Type 'init_game.' to restart."),
    fail.

% Check if trap is activated in a room
check_trap(Room) :-
    trap(Room, turns(Limit)),
    turns_in_room(Room, Turns),
    Turns #>= Limit,
    format("TRAP ACTIVATED in room ~w after ~w turns!~n", [Room, Turns]),
    writeln("You've been caught in a trap!"),
    writeln("Game over. Type 'init_game.' to restart."),
    fail.
check_trap(_).

% Check inventory limits before picking up items
check_inventory_limit(Type) :-
    can_carry(Type, Limit),
    count_items_of_type(Type, Count),
    Count #< Limit.
check_inventory_limit(Type) :-
    can_carry(Type, Limit),
    count_items_of_type(Type, Count),
    Count #>= Limit,
    format("You cannot carry more items of type ~w. Limit: ~w~n", [Type, Limit]),
    fail.

% Count items of a specific type in inventory
count_items_of_type(key, Count) :-
    findall(K, state:has_key(K), Keys),
    length(Keys, Count).
count_items_of_type(piece, Count) :-
    findall(Piece-Puzzle, state:has_piece(Puzzle, Piece), Pieces),
    length(Pieces, Count).

% Initialize constraints at game start
initialize_constraints :-
    retractall(move_count(_)),
    assertz(move_count(0)),
    retractall(turns_in_room(_, _)),
    assertz(turns_in_room(a, 0)),
    assertz(turns_in_room(b, 0)),
    assertz(turns_in_room(c, 0)),
    assertz(turns_in_room(d, 0)).


% Check if the current state is valid
% This predicate checks if the current state of the game is valid based on the constraints defined above.
valid_state(Room, Keys, Moves, BVisits) :-
    % Verificar límite de movimientos
    max_moves(MaxMoves),
    Moves =< MaxMoves,
    
    % Verificar límite de visitas a B si estamos en esa habitación
    (Room == 'B' -> 
        max_b_visits(MaxB),
        BVisits =< MaxB
    ; true),
    
    % Verificar que las llaves sean válidas
    valid_keys(Keys),
    
    % Verificar que la habitación existe
    room(Room).

% Predicado auxiliar para validar llaves
valid_keys([]).
valid_keys([Key|Rest]) :-
    (key_in_room(_, Key) ; has_key(Key)), 
    valid_keys(Rest).