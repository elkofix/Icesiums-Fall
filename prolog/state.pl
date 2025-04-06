:- module(state, [
    player_location/1, inventory/1, door_state/3,
    has_piece/1, object_moved/1, puzzle_solved/0,
    move_player/1, pick_key/1, move_object/1,
    solve_puzzle/0, unlock_door/2
]).

:- dynamic player_location/1, inventory/1, door_state/3,
           has_piece/1, object_moved/1, puzzle_solved/0.

:- use_module(facts).

% Estado inicial
player_location(a).
inventory([]).

door_state(a, b, locked).
door_state(b, a, locked).
door_state(b, c, locked).
door_state(c, b, locked).

% Mover jugador
move_player(NewRoom) :-
    player_location(Current),
    (door_state(Current, NewRoom, unlocked) ; door_state(NewRoom, Current, unlocked)),
    retract(player_location(Current)),
    assertz(player_location(NewRoom)),
    format("Te moviste de ~w a ~w.~n", [Current, NewRoom]).

% Recoger llave
pick_key(Key) :-
    player_location(Room),
    key_in_room(Room, Key),
    inventory(Inv),
    \+ member(Key, Inv),
    retract(inventory(Inv)),
    assertz(inventory([Key | Inv])),
    format("Has recogido la llave: ~w~n", [Key]).

% Mover objeto
move_object(Object) :-
    player_location(Room),
    object_in_room(Room, Object),
    \+ object_moved(Object),
    assertz(object_moved(Object)),
    ( hides_piece(Object, Piece) ->
        assertz(has_piece(Piece)),
        format("¡Encontraste una pieza: ~w!~n", [Piece])
    ;
        writeln("No había nada debajo.")
    ).

% Resolver puzzle
solve_puzzle :-
    findall(P, piece(P), AllPieces),
    findall(P, has_piece(P), FoundPieces),
    % Piezas visibles
    player_location(Current),
    findall(P, piece_visible_in_room(Current, P), VisiblePieces),
    forall(member(P, VisiblePieces), ( \+ has_piece(P) -> assertz(has_piece(P)) ; true )),
    append(FoundPieces, VisiblePieces, AllCurrent),
    sort(AllCurrent, AllCollected),
    sort(AllPieces, AllExpected),
    AllCollected == AllExpected,
    \+ puzzle_solved,
    assertz(puzzle_solved),
    writeln("¡Puzzle resuelto!").

% Desbloquear puertas
unlock_door(From, To) :-
    door_state(From, To, locked),
    ( From = a, inventory(Inv), member(key1, Inv) ->
        retract(door_state(From, To, locked)),
        assertz(door_state(From, To, unlocked)),
        retract(door_state(To, From, locked)),
        assertz(door_state(To, From, unlocked)),
        writeln("Puerta desbloqueada con llave.")
    ; From = b, puzzle_requirement(b, [puzzle_solved]), puzzle_solved ->
        retract(door_state(From, To, locked)),
        assertz(door_state(From, To, unlocked)),
        retract(door_state(To, From, locked)),
        assertz(door_state(To, From, unlocked)),
        writeln("Puerta desbloqueada tras resolver el puzzle.")
    ;
        writeln("No cumples con los requisitos para desbloquear esa puerta.")
    ).
