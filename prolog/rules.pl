% --- Movimiento permitido ---
can_move(X, Y) :-
    door(X, Y, unlocked).
can_move(X, Y) :-
    door(Y, X, unlocked).

% --- Tomar llave ---
take_key(Room) :-
    key_in_room(Room, Key),
    \+ has_key(Key),
    assertz(has_key(Key)),
    format('You picked up ~w in room ~w.~n', [Key, Room]).

% --- Desbloquear puertas si tienes la llave correcta ---
unlock_door(From, To) :-
    (door(From, To, locked); door(To, From, locked)),
    unlocks(Key, door(From, To)), % o door(To, From)
    has_key(Key),
    (
        retract(door(From, To, locked)), assertz(door(From, To, unlocked))
    ;
        retract(door(To, From, locked)), assertz(door(To, From, unlocked))
    ),
    format('Door from ~w to ~w has been unlocked with ~w.~n', [From, To, Key]).
