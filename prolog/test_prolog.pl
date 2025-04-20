% test_facts.pl - Pruebas unitarias para el módulo facts.pl
:- use_module(library(plunit)).
:- use_module('constraints').
:- use_module('state').
:- use_module('facts').
:- use_module('main').
:- use_module('rules').
:- use_module('search_no_constraints').
:- use_module('search').


:- begin_tests(prolog_module).

% Pruebas para verificar la creación y limpieza de datos del juego

% Custom setup to initialize the game state before tests
setup_test_environments :-
    % Clear any existing game state
    retractall(state:player_location(_)),
    retractall(state:inventory(_)),
    retractall(state:door_state(_, _, _)),
    retractall(state:has_piece(_, _)),
    retractall(state:object_moved(_)),
    retractall(state:puzzle_solved(_)),
    retractall(state:key_dropped(_, _)),
    retractall(state:piece_dropped(_, _, _)),
    retractall(state:has_key(_)),
    
    % Set up initial state
    assertz(state:player_location(a)),
    assertz(state:inventory([])),
    
    % Mock facts for testing
    retractall(facts:room(_)),
    retractall(facts:door(_, _, _)),
    retractall(facts:key_in_room(_, _)),
    retractall(facts:object_in_room(_, _)),
    retractall(facts:piece(_, _)),
    retractall(facts:puzzle_room(_, _)),
    retractall(facts:door_requirements(_, _, _)),
    retractall(facts:final_room(_)),
    
    % Create test rooms
    assertz(facts:room(a)),
    assertz(facts:room(b)),
    assertz(facts:room(c)),
    assertz(facts:room(d)),
    
    % Set up doors
    assertz(facts:door(a, b, unlocked)),
    assertz(facts:door(b, c, locked)),
    assertz(facts:door(c, d, locked)),
    assertz(state:door_state(a, b, unlocked)),
    assertz(state:door_state(b, a, unlocked)),
    assertz(state:door_state(b, c, locked)),
    assertz(state:door_state(c, b, locked)),
    assertz(state:door_state(c, d, locked)),
    assertz(state:door_state(d, c, locked)),
    
    % Mock key, object, and puzzle data
    assertz(facts:key_in_room(a, key1)),
    assertz(facts:key_in_room(b, key2)),
    assertz(facts:object_in_room(a, table)),
    assertz(facts:object_in_room(b, chair)),
    assertz(facts:hides_piece(table, puzzle1, piece1)),
    assertz(facts:piece(puzzle1, piece1)),
    assertz(facts:piece(puzzle1, piece2)),
    assertz(facts:piece_in_room(b, piece2, puzzle1)),
    assertz(facts:puzzle(puzzle1)),
    assertz(facts:puzzle_room(puzzle1, c)),
    assertz(facts:door_requirements(b, c, [has_key(key1)])),
    assertz(facts:door_requirements(c, d, [puzzle_solved(puzzle1)])),
    assertz(facts:final_room(d)),
    
    % Set up constraint mocks
    retractall(constraints:max_inventory_size(_)),
    retractall(constraints:max_moves(_)),
    retractall(constraints:move_count(_)),
    retractall(constraints:room_visits(_, _)),
    assertz(constraints:max_inventory_size(5)),
    assertz(constraints:max_moves(50)),
    assertz(constraints:move_count(0)),
    assertz(constraints:room_visits(a, 0)),
    assertz(constraints:room_visits(b, 0)),
    assertz(constraints:room_visits(c, 0)),
    assertz(constraints:room_visits(d, 0)).

% Setup mock data for tests
test_fixture_setup :-
    % Define rooms
    retractall(facts:room(_)),
    assertz(facts:room(start_room)),
    assertz(facts:room(middle_room)),
    assertz(facts:room(final_room)),
    
    % Set player location
    retractall(state:player_location(_)),
    assertz(state:player_location(start_room)),
    
    % Define the final room
    retractall(facts:final_room(_)),
    assertz(facts:final_room(final_room)),
    
    % Define doors
    retractall(facts:door(_, _, _)),
    assertz(facts:door(start_room, middle_room, unlocked)),
    assertz(facts:door(middle_room, final_room, locked)),
    
    % Define keys and pieces
    retractall(facts:key_in_room(_, _)),
    assertz(facts:key_in_room(start_room, key1)),
    
    % Define puzzle pieces
    retractall(facts:piece_in_room(_, _, _)),
    assertz(facts:piece_in_room(middle_room, piece1, puzzle1)),
    
    % Define objects
    retractall(facts:object_in_room(_, _)),
    assertz(facts:object_in_room(middle_room, cabinet)),
    
    % Define hidden pieces
    retractall(facts:hides_piece(_, _, _)),
    assertz(facts:hides_piece(cabinet, puzzle1, piece2)),
    
    % Define puzzles
    retractall(facts:puzzle_room(_, _)),
    assertz(facts:puzzle_room(puzzle1, middle_room)),
    
    % Define door requirements
    retractall(facts:door_requirements(_, _, _)),
    assertz(facts:door_requirements(middle_room, final_room, [has_key(key1), puzzle_solved(puzzle1)])),
    
    % Define pieces needed for puzzles
    retractall(facts:piece(_, _)),
    assertz(facts:piece(puzzle1, piece1)),
    assertz(facts:piece(puzzle1, piece2)),
    
    % Define constraints
    retractall(constraints:can_carry(_, _)),
    assertz(constraints:can_carry(key, 3)),
    assertz(constraints:can_carry(piece, 5)).

test_fixture_teardown :-
    % Clean up all the facts we've added
    retractall(facts:room(_)),
    retractall(state:player_location(_)),
    retractall(facts:final_room(_)),
    retractall(facts:door(_, _, _)),
    retractall(facts:key_in_room(_, _)),
    retractall(facts:piece_in_room(_, _, _)),
    retractall(facts:object_in_room(_, _)),
    retractall(facts:hides_piece(_, _, _)),
    retractall(facts:puzzle_room(_, _)),
    retractall(facts:door_requirements(_, _, _)),
    retractall(facts:piece(_, _)),
    retractall(constraints:can_carry(_, _)).


setups :-
    % Configurar habitaciones
    retractall(facts:room(_)),
    assertz(facts:room(start_room)),
    assertz(facts:room(middle_room)),
    assertz(facts:room(final_room)),
    
    % Configurar ubicación inicial del jugador
    retractall(state:player_location(_)),
    assertz(state:player_location(start_room)),
    
    % Configurar habitación final
    retractall(facts:final_room(_)),
    assertz(facts:final_room(final_room)),
    
    % Configurar puertas
    retractall(facts:door(_, _, _)),
    assertz(facts:door(start_room, middle_room, unlocked)),
    assertz(facts:door(middle_room, final_room, locked)),
    
    % Configurar llaves
    retractall(facts:key_in_room(_, _)),
    assertz(facts:key_in_room(start_room, key1)),
    
    % Configurar requisitos de puertas
    retractall(facts:door_requirements(_, _, _)),
    assertz(facts:door_requirements(middle_room, final_room, [has_key(key1)])),
    
    % Configurar puzzles y piezas
    retractall(facts:puzzle_room(_, _)),
    retractall(facts:piece(_, _)),
    retractall(facts:piece_in_room(_, _, _)),
    retractall(facts:object_in_room(_, _)),
    retractall(facts:hides_piece(_, _, _)),
    
    assertz(facts:puzzle_room(puzzle1, middle_room)),
    assertz(facts:piece(puzzle1, piece1)),
    assertz(facts:piece(puzzle1, piece2)),
    assertz(facts:piece_in_room(middle_room, piece1, puzzle1)),
    assertz(facts:object_in_room(middle_room, cabinet)),
    assertz(facts:hides_piece(cabinet, puzzle1, piece2)).




setup_test_environment :-
    % Clean slate
    retractall(state:player_location(_)),
    retractall(state:inventory(_)),
    retractall(state:door_state(_, _, _)),
    retractall(state:has_key(_)),
    retractall(state:has_piece(_, _)),
    retractall(state:puzzle_solved(_)),
    
    % Add some test data
    assertz(state:player_location(a)),
    assertz(state:inventory([])),
    
    % Door states
    assertz(state:door_state(a, b, unlocked)),
    assertz(state:door_state(b, c, locked)),
    assertz(state:door_state(c, d, locked)),
    
    % Mock facts if not imported properly
    (predicate_property(facts:door(_, _, _), visible) -> true
    ; (
        assertz(facts:door(a, b, unlocked)),
        assertz(facts:door(b, a, unlocked)),
        assertz(facts:door(b, c, locked)),
        assertz(facts:door(c, b, locked)),
        assertz(facts:door(c, d, locked)),
        assertz(facts:door(d, c, locked)),
        
        assertz(facts:door_requirements(b, c, [has_key(key1)])),
        assertz(facts:door_requirements(c, d, [puzzle_solved(puzzle1)]))
      )),
    
    % Add rooms for tests
    (predicate_property(facts:room(_), visible) -> true
    ; (
        assertz(facts:room(a)),
        assertz(facts:room(b)),
        assertz(facts:room(c)),
        assertz(facts:room(d))
      )).


setup_test :-
    facts:load_predefined_game,
    constraints:load_default_constraints,
    rules:initialize_game.

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

% Test for player initialization
test(player_initialization) :-
    setup_test,
    state:player_location(Room),
    % Asegurarse de que el jugador comience en una habitación válida
    facts:room(Room).

% Test para inventory vacío al inicio
test(initial_inventory_empty) :-
    setup_test,
    state:inventory(Inventory),
    Inventory == [].

% Test para move_count inicialización
test(move_count_initialization) :-
    setup_test,
    constraints:move_count(Count),
    Count == 0.

% Test para verificar movimientos de habitación
test(player_movement) :-
    setup_test,
    state:player_location(InitialRoom),
    % Buscar una habitación adyacente
    facts:door(InitialRoom, NextRoom, _),
    % Desbloquear la puerta
    retractall(state:door_state(InitialRoom, NextRoom, _)),
    assertz(state:door_state(InitialRoom, NextRoom, unlocked)),
    % Mover al jugador
    rules:move_player(NextRoom),
    % Verificar nueva posición
    state:player_location(CurrentRoom),
    CurrentRoom == NextRoom.

% Test para verificar recoger llaves
test(pick_key) :-
    setup_test,
    state:player_location(Room),
    % Crear una llave en la habitación para la prueba
    assertz(facts:key_in_room(Room, test_key)),
    % Verificar que la pueda recoger
    rules:pick_key(test_key),
    % Comprobar que está en el inventario
    state:inventory(Inventory),
    member(test_key, Inventory),
    % Limpiar
    retractall(facts:key_in_room(Room, test_key)).

% Test para recoger piezas de rompecabezas
test(pick_piece) :-
    setup_test,
    state:player_location(Room),
    % Crear una pieza en la habitación
    assertz(facts:piece_in_room(Room, test_piece, test_puzzle)),
    % Recoger la pieza
    rules:pick_piece(test_piece),
    % Verificar que tiene la pieza
    state:has_piece(test_puzzle, test_piece),
    % Limpiar
    retractall(facts:piece_in_room(Room, test_piece, test_puzzle)).

% Test para resolver un rompecabezas
test(solve_puzzle) :-
    setup_test,
    state:player_location(Room),
    % Crear un puzzle en esta habitación
    assertz(facts:puzzle(test_puzzle)),
    assertz(facts:puzzle_room(test_puzzle, Room)),
    assertz(facts:piece(test_puzzle, piece1)),
    assertz(facts:piece(test_puzzle, piece2)),
    % Dar las piezas al jugador
    assertz(state:has_piece(test_puzzle, piece1)),
    assertz(state:has_piece(test_puzzle, piece2)),
    % Intentar resolver
    rules:solve_puzzle(test_puzzle),
    % Verificar que está resuelto
    state:puzzle_solved(test_puzzle),
    % Limpiar
    retractall(facts:puzzle(test_puzzle)),
    retractall(facts:puzzle_room(test_puzzle, _)),
    retractall(facts:piece(test_puzzle, _)).


% Tests for can_move/2
test(can_move_unlocked) :-
    setup_test_environment,
    assertion(rules:can_move(a, b)),
    \+ rules:can_move(b, c).  % Should fail because it's locked and we don't have the key

% Tests for satisfy_requirements/1
test(satisfy_requirements_key) :-
    setup_test_environment,
    \+ rules:satisfy_requirements([has_key(key1)]),
    assertz(state:has_key(key1)),
    assertion(rules:satisfy_requirements([has_key(key1)])).

test(satisfy_requirements_puzzle) :-
    setup_test_environment,
    \+ rules:satisfy_requirements([puzzle_solved(puzzle1)]),
    assertz(state:puzzle_solved(puzzle1)),
    assertion(rules:satisfy_requirements([puzzle_solved(puzzle1)])).

test(satisfy_requirements_multiple) :-
    setup_test_environment,
    \+ rules:satisfy_requirements([has_key(key1), puzzle_solved(puzzle1)]),
    assertz(state:has_key(key1)),
    \+ rules:satisfy_requirements([has_key(key1), puzzle_solved(puzzle1)]),
    assertz(state:puzzle_solved(puzzle1)),
    assertion(rules:satisfy_requirements([has_key(key1), puzzle_solved(puzzle1)])).

% Test para verificar el camino completo encontrado por la función principal
test(find_escape_solution) :-
    setups,
    
    % Capturar salida para verificar el resultado
    with_output_to(string(Output), 
                  find_escape_solution_no_constraints),
    
    % Verificar que se encontró una solución exitosa
    assertion(sub_string(Output, _, _, _, '¡Solución encontrada!')),
    
    cleanup.

cleanup :-
    % Limpiar todo el entorno de prueba
    retractall(facts:room(_)),
    retractall(state:player_location(_)),
    retractall(facts:final_room(_)),
    retractall(facts:door(_, _, _)),
    retractall(facts:key_in_room(_, _)),
    retractall(facts:door_requirements(_, _, _)),
    retractall(facts:puzzle_room(_, _)),
    retractall(facts:piece(_, _)),
    retractall(facts:piece_in_room(_, _, _)),
    retractall(facts:object_in_room(_, _)),
    retractall(facts:hides_piece(_, _, _)).

% Helper predicate tests
test(count_keys) :-
    search:count_keys([key1, key2, sword], Count),
    assertion(Count == 2).

test(is_key) :-
    assertion(search:is_key(key1)),
    assertion(search:is_key(key_blue)),
    \+ search:is_key(potion).

test(count_pieces) :-
    search:count_pieces([(puzzle1, piece1), (puzzle2, piece1)], Count),
    assertion(Count == 2).

test(all_pieces_collected) :-
    % All required pieces are collected
    assertion(search:all_pieces_collected(puzzle1, [piece1, piece2], [(puzzle1, piece1), (puzzle1, piece2)])),
    % Missing piece2
    \+ search:all_pieces_collected(puzzle1, [piece1, piece2], [(puzzle1, piece1)]).

test(remove_puzzle_pieces) :-
    search:remove_puzzle_pieces(
        puzzle1, 
        [(puzzle1, piece1), (puzzle1, piece2), (puzzle2, piece1)], 
        Remaining
    ),
    assertion(Remaining == [(puzzle2, piece1)]).

test(remove_keys) :-
    search:remove_keys([key1, key3], [key1, key2, key3], Remaining),
    assertion(Remaining == [key2]).

% Action tests
test(possible_action_move_to_unlocked_room, [setup(test_fixture_setup), cleanup(test_fixture_teardown)]) :-
    InitialState = state(start_room, [], [], [], [], []),
    search:possible_action(
        InitialState,
        state(middle_room, [], [], [], [], []),
        Action
    ),
    assertion(Action == move_player(middle_room)).

test(possible_action_pick_key, [setup(test_fixture_setup), cleanup(test_fixture_teardown)]) :-
    InitialState = state(start_room, [], [], [], [], []),
    search:possible_action(
        InitialState,
        state(start_room, [key1], [], [], [], []),
        Action
    ),
    assertion(Action == pick_key(key1)).

test(possible_action_move_object, [setup(test_fixture_setup), cleanup(test_fixture_teardown)]) :-
    InitialState = state(middle_room, [], [], [], [], []),
    search:possible_action(
        InitialState,
        state(middle_room, [], [], [], [cabinet], [(puzzle1, piece2)]),
        Action
    ),
    assertion(Action == move_object(cabinet)).

test(possible_action_pick_piece, [setup(test_fixture_setup), cleanup(test_fixture_teardown)]) :-
    InitialState = state(middle_room, [], [], [], [], []),
    search:possible_action(
        InitialState,
        state(middle_room, [], [], [], [], [(puzzle1, piece1)]),
        Action
    ),
    assertion(Action == pick_piece(piece1)).

test(possible_action_solve_puzzle, [setup(test_fixture_setup), cleanup(test_fixture_teardown)]) :-
    InitialState = state(middle_room, [], [], [], [], [(puzzle1, piece1), (puzzle1, piece2)]),
    search:possible_action(
        InitialState,
        state(middle_room, [], [], [puzzle1], [], []),
        Action
    ),
    assertion(Action == solve_puzzle(puzzle1)).

test(possible_action_unlock_door, [setup(test_fixture_setup), cleanup(test_fixture_teardown)]) :-
    InitialState = state(middle_room, [key1], [], [puzzle1], [], []),
    search:possible_action(
        InitialState,
        state(middle_room, [], [(middle_room, final_room)], [puzzle1], [], []),
        Action
    ),
    assertion(Action == unlock_door(middle_room, final_room)).

% BFS and solution tests
test(bfs_simple_path, [setup(test_fixture_setup), cleanup(test_fixture_teardown)]) :-
    % Create a simpler scenario for testing BFS
    retractall(facts:door(_, _, _)),
    assertz(facts:door(start_room, middle_room, unlocked)),
    assertz(facts:door(middle_room, final_room, unlocked)),
    
    InitialState = state(start_room, [], [], [], [], []),
    search:bfs(InitialState, _, Solution),
    % Solution should be to move from start to middle to final room
    assertion(length(Solution, 2)),
    assertion(memberchk(move_player(middle_room), Solution)),
    assertion(memberchk(move_player(final_room), Solution)).

test(full_escape_solution, [setup(test_fixture_setup), cleanup(test_fixture_teardown), nondet]) :-
    % Test the full solution finding capability
    % This tests the integration of all components
    assertz(state:player_location(start_room)),
    % Redirect output to avoid cluttering test results
    with_output_to(string(_), search:find_escape_solution).


test(move_player_to_invalid_room, [fail]) :-
    setup_test_environments,
    state:move_player(c).  % Should fail because door is locked

test(solve_puzzle_missing_pieces, [fail]) :-
    setup_test_environments,
    state:move_player(b),
    state:pick_key(key2),
    state:unlock_door(b, c),
    state:move_player(c),
    state:solve_puzzle(puzzle1).  % Should fail because player doesn't have all pieces


% Para ejecutar las pruebas, descomentar la siguiente línea:


% :- run_tests.