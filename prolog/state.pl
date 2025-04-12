:- module(state, [
    player_location/1, inventory/1, door_state/3,
    has_piece/2, object_moved/1, puzzle_solved/1,
    move_player/1, pick_key/1, pick_piece/1, move_object/1,
    solve_puzzle/1, unlock_door/2, has_key/1
]).

:- dynamic player_location/1.
:- dynamic inventory/1.
:- dynamic door_state/3.
:- dynamic has_piece/2.
:- dynamic object_moved/1.
:- dynamic puzzle_solved/1.

:- use_module(facts).

% Initial state
player_location(a).
inventory([]).

% Initialize door states
:- forall(facts:door(X, Y, State), assertz(door_state(X, Y, State))).

% Move player between rooms
move_player(NewRoom) :-
    player_location(Current),
    (door_state(Current, NewRoom, unlocked) ; door_state(NewRoom, Current, unlocked)),
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

% Pick up a key
pick_key(Key) :-
    player_location(Room),
    facts:key_in_room(Room, Key),
    inventory(Inv),
    \+ member(Key, Inv),
    retract(inventory(Inv)),
    assertz(inventory([Key | Inv])),
    format("You have picked up key: ~w~n", [Key]),
    !.
pick_key(Key) :-
    format("Cannot pick up key ~w here.~n", [Key]).

% Pick up a puzzle piece
pick_piece(Piece) :-
    player_location(Room),
    facts:piece_in_room(Room, Piece, Puzzle),
    \+ has_piece(Puzzle, Piece),
    assertz(has_piece(Puzzle, Piece)),
    format("You have picked up piece ~w of puzzle ~w!~n", [Piece, Puzzle]),
    !.
pick_piece(Piece) :-
    format("Cannot pick up piece ~w here.~n", [Piece]).

% Move an object to find hidden pieces
move_object(Object) :-
    player_location(Room),
    facts:object_in_room(Room, Object),
    \+ object_moved(Object),
    assertz(object_moved(Object)),
    % Check if moving reveals a puzzle piece
    ( facts:hides_piece(Object, Puzzle, Piece) ->
        assertz(has_piece(Puzzle, Piece)),
        format("You moved ~w and found piece ~w of puzzle ~w!~n", [Object, Piece, Puzzle])
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

% Solve a puzzle
solve_puzzle(Puzzle) :-
    player_location(Room),
    facts:puzzle_room(Puzzle, Room),
    \+ puzzle_solved(Puzzle),
    % Get all pieces needed for this puzzle
    findall(Piece, facts:piece(Puzzle, Piece), AllPieces),
    % Check if player has all necessary pieces
    forall(member(P, AllPieces), has_piece(Puzzle, P)),
    assertz(puzzle_solved(Puzzle)),
    format("Congratulations! You have solved puzzle ~w!~n", [Puzzle]),
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

% Dynamic door unlocking system
unlock_door(From, To) :-
    door_state(From, To, locked),
    % Check if there are requirements for this door
    facts:door_requirements(From, To, Requirements),
    % Check if all requirements are met
    check_all_requirements(Requirements),
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
