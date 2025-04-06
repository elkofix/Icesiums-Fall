% --- Habitaciones ---
room(a).
room(b).
room(c).
room(d).
room(e).

% --- Puertas: door(From, To, Estado) ---
door(a, b, locked).
door(b, c, unlocked).
door(c, d, locked).
door(d, e, unlocked).

% --- Llaves y su ubicación ---
key_in_room(a, key1).
key_in_room(d, key2).

% --- Qué llave abre qué puerta ---
unlocks(key1, door(a, b)).
unlocks(key2, door(c, d)).

% Inventario dinámico
:- dynamic has_key/1.
