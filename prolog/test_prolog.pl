% test_facts.pl - Pruebas unitarias para el módulo facts.pl
:- use_module(library(plunit)).
:- use_module('constraints').
:- use_module('state').
:- use_module('facts').

:- begin_tests(prolog_module).

% Pruebas para verificar la creación y limpieza de datos del juego

setup :-
    clear_constraints(),
    retractall(state:has_key(_)),
    retractall(state:has_piece(_, _)),
    retractall(state:current_room(_)),
    retractall(state:visited_b(_)).


test(clear_game_data) :-
    % Añadir algunos datos de ejemplo
    add_room(test_room),
    add_door(test_room, another_room, locked),
    add_key(test_room, test_key),
    % Limpiar datos
    clear_game_data,
    % Verificar que no existan los datos
    \+ room(test_room),
    \+ door(test_room, another_room, _),
    \+ key_in_room(test_room, test_key).

% Pruebas para las funciones de añadir elementos
test(add_room) :-
    clear_game_data,
    add_room(room1),
    room(room1).

test(add_door) :-
    clear_game_data,
    add_room(room1),
    add_room(room2),
    add_door(room1, room2, locked),
    door(room1, room2, locked).

test(add_key) :-
    clear_game_data,
    add_room(room1),
    add_key(room1, key1),
    key_in_room(room1, key1).

test(add_object) :-
    clear_game_data,
    add_room(room1),
    add_object(room1, box),
    object_in_room(room1, box).

test(add_puzzle) :-
    clear_game_data,
    add_puzzle(puzzle1),
    puzzle(puzzle1).

test(add_piece) :-
    clear_game_data,
    add_puzzle(puzzle1),
    add_piece(puzzle1, piece1),
    piece(puzzle1, piece1).

test(hide_piece) :-
    clear_game_data,
    add_room(room1),
    add_object(room1, box),
    add_puzzle(puzzle1),
    add_piece(puzzle1, piece1),
    hide_piece(box, puzzle1, piece1),
    hides_piece(box, puzzle1, piece1),
    piece_location(puzzle1, piece1, room1).

test(hide_piece_no_location) :-
    clear_game_data,
    add_puzzle(puzzle1),
    add_piece(puzzle1, piece1),
    % Objeto no está en ninguna habitación todavía
    hide_piece(box, puzzle1, piece1),
    hides_piece(box, puzzle1, piece1),
    \+ piece_location(puzzle1, piece1, _).

test(set_puzzle_room) :-
    clear_game_data,
    add_room(room1),
    add_puzzle(puzzle1),
    set_puzzle_room(puzzle1, room1),
    puzzle_room(puzzle1, room1).

test(set_door_requirements) :-
    clear_game_data,
    add_room(room1),
    add_room(room2),
    add_door(room1, room2, locked),
    set_door_requirements(room1, room2, [has_key(key1)]),
    door_requirements(room1, room2, [has_key(key1)]).


% Basic constraint setting tests
test(set_max_moves) :-
    setup,
    set_max_moves(42),
    max_moves(Limit),
    assertion(Limit == 42).

test(set_max_b_visits) :-
    setup,
    set_max_b_visits(5),
    max_b_visits(Limit),
    assertion(Limit == 5).

test(set_carry_limit) :-
    setup,
    set_carry_limit(key, 3),
    can_carry(key, Limit),
    assertion(Limit == 3).

test(add_room_trap) :-
    setup,
    add_room_trap(d, 4),
    trap(d, turns(TurnLimit)),
    assertion(TurnLimit == 4).

test(clear_constraints) :-
    % First set some constraints
    set_max_moves(10),
    set_max_b_visits(2),
    set_carry_limit(key, 1),
    add_room_trap(a, 2),
    
    % Then clear them
    clear_constraints(),
    
    % Check they're gone
    \+ max_moves(_),
    \+ max_b_visits(_),
    \+ can_carry(_, _),
    \+ trap(_, _).

test(load_default_constraints) :-
    setup,
    load_default_constraints(),
    max_moves(MaxMoves),
    max_b_visits(MaxB),
    can_carry(key, KeyLimit),
    can_carry(piece, PieceLimit),
    trap(c, turns(TrapLimit)),
    
    assertion(MaxMoves == 30),
    assertion(MaxB == 3),
    assertion(KeyLimit == 2),
    assertion(PieceLimit == 4),
    assertion(TrapLimit == 3).

test(check_inventory_limit_at_max, [fail]) :-
    setup,
    % Set limit to 2 keys
    set_carry_limit(key, 2),
    % Have 2 keys already
    assertz(state:has_key(gold)),
    assertz(state:has_key(silver)),
    % Should not be able to carry another
    check_inventory_limit(key).

test(count_items_of_type_keys) :-
    setup,
    assertz(state:has_key(gold)),
    assertz(state:has_key(silver)),
    count_items_of_type(key, Count),
    assertion(Count == 2).

test(count_items_of_type_pieces) :-
    setup,
    assertz(state:has_piece(puzzle1, piece1)),
    assertz(state:has_piece(puzzle1, piece2)),
    assertz(state:has_piece(puzzle2, piece1)),
    count_items_of_type(piece, Count),
    assertion(Count == 3).

test(valid_state_exceeds_moves, [fail]) :-
    setup,
    load_default_constraints(),
    valid_state(a, [gold], 31, 2).

test(valid_state_exceeds_b_visits, [fail]) :-
    setup,
    load_default_constraints(),
    valid_state(b, [gold], 10, 4).


% Trap test
test(check_trap_not_triggered) :-
    setup,
    add_room_trap(a, 3),
    retractall(turns_in_room(a, _)),
    assertz(turns_in_room(a, 2)),
    check_trap(a).


% Para ejecutar las pruebas, descomentar la siguiente línea:
% :- run_tests.