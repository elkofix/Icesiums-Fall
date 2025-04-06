% --- Iniciar jugador ---
init_player(Room) :-
    retractall(player_at(_)),
    assertz(player_at(Room)),
    format('Player is now in room ~w.~n', [Room]).

% --- Moverse si la puerta está desbloqueada ---
move_to(NextRoom) :-
    player_at(Current),
    (door(Current, NextRoom, unlocked); door(NextRoom, Current, unlocked)),
    retract(player_at(Current)),
    assertz(player_at(NextRoom)),
    format('Moved from ~w to ~w.~n', [Current, NextRoom]).

move_to(NextRoom) :-
    format('Cannot move to ~w. Door is locked or no direct connection.~n', [NextRoom]), fail.

% --- Tomar una llave si está en la habitación actual ---
take_key :-
    player_at(Room),
    key_in_room(Room, Key),
    \+ has_key(Key),
    assertz(has_key(Key)),
    format('Picked up ~w in room ~w.~n', [Key, Room]).

take_key :-
    format('No key in this room or already taken.~n'), fail.

% --- Desbloquear puerta si se tiene la llave y está al lado ---
unlock_door(Room1, Room2) :-
    player_at(Current),
    (Current = Room1; Current = Room2),
    (door(Room1, Room2, locked); door(Room2, Room1, locked)),
    unlocks(Key, door(Room1, Room2)),
    has_key(Key),
    (   retract(door(Room1, Room2, locked)), assertz(door(Room1, Room2, unlocked))
    ;   retract(door(Room2, Room1, locked)), assertz(door(Room2, Room1, unlocked))
    ),
    format('Unlocked door from ~w to ~w with key ~w.~n', [Room1, Room2, Key]).

unlock_door(_, _) :-
    format('Failed to unlock door. Either wrong location, no key, or already unlocked.~n'), fail.

