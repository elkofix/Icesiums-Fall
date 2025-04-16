% state.pl - with constraint additions and fixes
:- module(state, [
    player_location/1, inventory/1, door_state/3,
    has_piece/2, object_moved/1, puzzle_solved/1,
    move_player/1, pick_key/1, pick_piece/1, move_object/1,
    solve_puzzle/1, unlock_door/2, has_key/1, remove_key/1,
    drop_key/1, drop_piece/1, key_dropped/2, piece_dropped/3
]).

:- dynamic player_location/1.
:- dynamic inventory/1.
:- dynamic door_state/3.
:- dynamic has_piece/2.
:- dynamic object_moved/1.
:- dynamic puzzle_solved/1.
:- dynamic key_dropped/2.    % key_dropped(Key, Room)
:- dynamic piece_dropped/3.  % piece_dropped(Piece, Puzzle, Room)
:- dynamic has_key/1.

:- use_module(facts).
:- use_module(constraints).

% Para evitar warnings de predicados discontiguos
:- discontiguous pick_key/1, pick_piece/1.

% Initial state
player_location(a).
inventory([]).

% Initialize door states
:- forall(facts:door(X, Y, State), assertz(door_state(X, Y, State))).

% Move player between rooms with constraint checks
move_player(NewRoom) :-
    constraints:check_move_limit,  % Check if move limit reached
    player_location(Current),
    (door_state(Current, NewRoom, unlocked) ; door_state(NewRoom, Current, unlocked)),
    % Update counters FIRST before changing location
    constraints:increment_move_count,  % Increment move counter
    constraints:increment_room_turn(NewRoom),  % Increment room turn counter
    % Now update location
    retract(player_location(Current)),
    assertz(player_location(NewRoom)),
    format("You moved from ~w to ~w.~n", [Current, NewRoom]),
    !.
move_player(NewRoom) :-
    format("You cannot move to room ~w. The door is locked.~n", [NewRoom]).

% Check if player has a key
has_key(Key) :-
    inventory(Inv),
    member(Key, Inv).

% Remove a key from inventory
remove_key(Key) :-
    inventory(Inv),
    select(Key, Inv, NewInv),  % Remove key from inventory
    retract(inventory(Inv)),
    assertz(inventory(NewInv)),
    format("Used and removed key: ~w~n", [Key]).

% Pick up a key with constraint check
pick_key(Key) :-
    constraints:check_inventory_limit(key),  % Check inventory limit
    player_location(Room),
    (facts:key_in_room(Room, Key) ; key_dropped(Key, Room)),  % Verificar ambas fuentes
    inventory(Inv),
    \+ member(Key, Inv),
    % Si la llave había sido dejada, eliminar ese hecho
    (key_dropped(Key, Room) -> retract(key_dropped(Key, Room)) ; true),
    retract(inventory(Inv)),
    assertz(inventory([Key | Inv])),
    format("You have picked up key: ~w~n", [Key]),
    !.

pick_key(Key) :-
    player_location(Room),
    (facts:key_in_room(Room, Key) ; key_dropped(Key, Room)),
    inventory(Inv),
    member(Key, Inv),
    format("You already have key ~w.~n", [Key]),
    !.
    
pick_key(Key) :-
    format("Cannot pick up key ~w here.~n", [Key]).

% Pick up a puzzle piece with constraint check
pick_piece(Piece) :-
    constraints:check_inventory_limit(piece),  % Check inventory limit
    player_location(Room),
    (facts:piece_in_room(Room, Piece, Puzzle) ; piece_dropped(Piece, Puzzle, Room)),
    \+ has_piece(Puzzle, Piece),
    % Si la pieza había sido dejada, eliminar ese hecho
    (piece_dropped(Piece, Puzzle, Room) -> retract(piece_dropped(Piece, Puzzle, Room)) ; true),
    assertz(has_piece(Puzzle, Piece)),
    format("You have picked up piece ~w of puzzle ~w!~n", [Piece, Puzzle]),
    !.

pick_piece(Piece) :-
    player_location(Room),
    (facts:piece_in_room(Room, Piece, Puzzle) ; piece_dropped(Piece, Puzzle, Room)),
    has_piece(Puzzle, Piece),
    format("You already have piece ~w of puzzle ~w.~n", [Piece, Puzzle]),
    !.
    
pick_piece(Piece) :-
    format("Cannot pick up piece ~w here.~n", [Piece]).

% Dejar una llave en la habitación actual
drop_key(Key) :-
    player_location(Room),
    has_key(Key),
    inventory(Inv),
    select(Key, Inv, NewInv),    % Eliminar la llave del inventario
    retract(inventory(Inv)),
    assertz(inventory(NewInv)),
    assertz(key_dropped(Key, Room)),
    format("Has dejado la llave ~w en la habitación ~w.~n", [Key, Room]),
    !.
drop_key(Key) :-
    \+ has_key(Key),
    format("No tienes la llave ~w en tu inventario.~n", [Key]).

% Dejar una pieza de puzzle en la habitación actual
drop_piece(Piece) :-
    player_location(Room),
    % Buscar a qué puzzle pertenece la pieza
    has_piece(Puzzle, Piece),
    retract(has_piece(Puzzle, Piece)),
    assertz(piece_dropped(Piece, Puzzle, Room)),
    format("Has dejado la pieza ~w del puzzle ~w en la habitación ~w.~n", [Piece, Puzzle, Room]),
    !.
drop_piece(Piece) :-
    format("No tienes la pieza ~w en tu inventario.~n", [Piece]).

% Move an object to find hidden pieces
move_object(Object) :-
    player_location(Room),
    facts:object_in_room(Room, Object),
    \+ object_moved(Object),
    assertz(object_moved(Object)),
    % Check if moving reveals a puzzle piece
    ( facts:hides_piece(Object, Puzzle, Piece) ->
        ( constraints:check_inventory_limit(piece) ->
            assertz(has_piece(Puzzle, Piece)),
            format("You moved ~w and found piece ~w of puzzle ~w!~n", [Object, Piece, Puzzle])
        ;
            format("You moved ~w and found piece ~w of puzzle ~w, but your inventory is full!~n", [Object, Piece, Puzzle])
        )
    ;
        format("You moved ~w but found nothing interesting.~n", [Object])
    ),
    !.
move_object(Object) :-
    player_location(Room),
    facts:object_in_room(Room, Object),
    object_moved(Object),
    format("You've already examined ~w.~n", [Object]),
    !.
move_object(Object) :-
    format("There is no ~w here to move.~n", [Object]).

% Solve a puzzle - now removes pieces from inventory
solve_puzzle(Puzzle) :-
    player_location(Room),
    facts:puzzle_room(Puzzle, Room),
    \+ puzzle_solved(Puzzle),
    % Get all pieces needed for this puzzle
    findall(Piece, facts:piece(Puzzle, Piece), AllPieces),
    % Check if player has all necessary pieces
    forall(member(P, AllPieces), has_piece(Puzzle, P)),
    % Remove all pieces from inventory
    forall(member(P, AllPieces), retract(has_piece(Puzzle, P))),
    % Mark puzzle as solved
    assertz(puzzle_solved(Puzzle)),
    format("Congratulations! You have solved puzzle ~w! The pieces have been consumed.~n", [Puzzle]),
    !.
solve_puzzle(Puzzle) :-
    player_location(Room),
    facts:puzzle_room(Puzzle, Room),
    \+ puzzle_solved(Puzzle),
    format("You don't have all the pieces needed to solve puzzle ~w.~n", [Puzzle]),
    !.
solve_puzzle(Puzzle) :-
    player_location(Room),
    facts:puzzle(Puzzle),
    \+ facts:puzzle_room(Puzzle, Room),
    format("Puzzle ~w cannot be solved in this room.~n", [Puzzle]),
    !.
solve_puzzle(Puzzle) :-
    puzzle_solved(Puzzle),
    format("Puzzle ~w is already solved.~n", [Puzzle]),
    !.
solve_puzzle(Puzzle) :-
    format("Unknown puzzle: ~w~n", [Puzzle]).

% Dynamic door unlocking system - now consumes keys
unlock_door(From, To) :-
    door_state(From, To, locked),
    % Check if there are requirements for this door
    facts:door_requirements(From, To, Requirements),
    % Check if all requirements are met
    check_all_requirements(Requirements),
    % Consume keys used for unlocking
    consume_keys(Requirements),
    % Update door state
    retract(door_state(From, To, locked)),
    assertz(door_state(From, To, unlocked)),
    retract(door_state(To, From, locked)),
    assertz(door_state(To, From, unlocked)),
    format("Door between rooms ~w and ~w has been unlocked!~n", [From, To]),
    !.
unlock_door(From, To) :-
    door_state(From, To, unlocked),
    format("The door between rooms ~w and ~w is already unlocked.~n", [From, To]),
    !.
unlock_door(From, To) :-
    facts:door_requirements(From, To, Requirements),
    format("You cannot unlock this door yet. Requirements: ~w~n", [Requirements]),
    !.
unlock_door(From, To) :-
    format("There is no direct door between rooms ~w and ~w.~n", [From, To]).

% Consume keys used for unlocking
consume_keys([]).
consume_keys([has_key(Key)|Rest]) :-
    remove_key(Key),
    consume_keys(Rest).
consume_keys([_|Rest]) :-  % Skip non-key requirements
    consume_keys(Rest).

% Check if all requirements for a door are met
check_all_requirements([]).
check_all_requirements([Req|Rest]) :-
    check_requirement(Req),
    check_all_requirements(Rest).

% Check individual requirements
check_requirement(has_key(Key)) :-
    has_key(Key), !.
check_requirement(puzzle_solved(Puzzle)) :-
    puzzle_solved(Puzzle), !.
check_requirement(Req) :-
    format("Requirement not met: ~w~n", [Req]),
    fail.