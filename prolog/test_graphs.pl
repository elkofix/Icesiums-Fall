:- module(test_graphs, [
    load_test_graph_1/0,
    
]).

:- use_module(facts).
:- use_module(search).
:- use_module(a_star).
:- use_module(search_no_constraints).

% Grafo de prueba 1
load_test_graph_1 :-
    facts:clear_game_data,
    
    % Definir habitaciones
    facts:add_room(a), facts:add_room(b), facts:add_room(c), facts:add_room(d),
    facts:add_room(e), facts:add_room(f), facts:add_room(g), facts:add_room(h),
    facts:add_room(i), facts:add_room(j), facts:add_room(k), facts:add_room(l),
    

    % Definir habitación final
    facts:set_final_room(l),
    
    % Definir puertas
    facts:add_door(a, b, unlocked),
    facts:add_door(b, c, unlocked),
    facts:add_door(c, d, unlocked),
    facts:add_door(a, e, unlocked),
    facts:add_door(e, f, unlocked),
    facts:add_door(f, g, unlocked),
    facts:add_door(g, h, unlocked),
    facts:add_door(h, e, unlocked),
    facts:add_door(c, i, unlocked),
    facts:add_door(i, j, unlocked),
    facts:add_door(j, k, unlocked),
    facts:add_door(k, l, unlocked),
    facts:add_door(l, i, unlocked),
    facts:add_door(a, i, locked),
    
    % Agregar llaves
    facts:add_key(a, key1),
    facts:add_key(c, key2),
    facts:add_key(f, key3),
    facts:add_key(h, key4),
    
    
    
    % Agregar objetos
    facts:add_object(a, box),
    facts:add_object(a, painting),
    facts:add_object(b, rug),
    facts:add_object(b, bookshelf),
    facts:add_object(c, cabinet),
    
    % Agregar puzzles
    facts:add_puzzle(puzzle1),
    facts:add_puzzle(puzzle2),
    facts:add_puzzle(puzzle3),
    
    % Agregar piezas
    facts:add_piece(puzzle1, p1),
    facts:add_piece(puzzle1, p2),
    facts:add_piece(puzzle1, p3),
    facts:add_piece(puzzle2, p4),
    facts:add_piece(puzzle2, p5),
    facts:add_piece(puzzle2, p6),
    facts:add_piece(puzzle3, p7),
    facts:add_piece(puzzle3, p8),
    
    % Establecer ubicaciones de piezas visibles
    facts:add_visible_piece(a, p3, puzzle1),
    facts:add_visible_piece(c, p4, puzzle2),
    facts:add_visible_piece(f, p7, puzzle3),
    
    
    % Ocultar piezas en objetos
    facts:hide_piece(rug, puzzle1, p1),
    facts:hide_piece(bookshelf, puzzle1, p2),
    facts:hide_piece(box, puzzle2, p5),
    facts:hide_piece(painting, puzzle2, p6),
    facts:hide_piece(cabinet, puzzle3, p8),
    facts:hide_piece(box, puzzle1, key3),
    facts:hide_piece(painting, puzzle2, key4),
    
    % Establecer habitaciones de puzzles
    facts:set_puzzle_room(puzzle1, b),
    facts:set_puzzle_room(puzzle2, c),
     facts:set_puzzle_room(puzzle3, g),
    
    % Establecer requisitos para las puertas
    facts:set_door_requirements(b, c, [puzzle_solved(puzzle1)]),
    facts:set_door_requirements(c, d, [puzzle_solved(puzzle2), has_key(key2)]),
    facts:set_door_requirements(f, g, [has_key(key3)]),
    facts:set_door_requirements(h, e, [has_key(key4)]),
    facts:set_door_requirements(g, h, [puzzle_solved(puzzle3)]),
    facts:set_door_requirements(a, i, [has_key(key1), puzzle_solved(puzzle1)]),
    
    writeln('Test graph 1 loaded successfully!').


run_tests :-
    writeln('Running tests for all graphs...'),
    writeln('Testing graph 1:'),
    load_test_graph_1,
    
    % Medir tiempo de find_escape_solution (BFS)
    get_time(Start1),
    find_escape_solution,  % Este es el predicado exportado por search.pl
    get_time(End1),
    Duration1 is End1 - Start1,
    format('find_escape_solution (BFS) took ~5f seconds~n', [Duration1]),
    nl,
    
    % Medir tiempo de find_escape_solution (A*)
    get_time(Start2),
    a_star:find_escape_solution,  % Este es el predicado exportado por a_star.pl
    get_time(End2),
    Duration2 is End2 - Start2,
    format('find_escape_solution (A*) took ~5f seconds~n', [Duration2]),
    nl,
    
    % Medir tiempo de find_escape_solution_no_constraints
    get_time(Start3),
    search_no_constraints:find_escape_solution,  % Este es el predicado exportado por search_no_constraints.pl
    get_time(End3),
    Duration3 is End3 - Start3,
    format('find_escape_solution_no_constraints took ~5f seconds~n', [Duration3]),
    nl,
    
    writeln('All tests completed!').

:- initialization(run_tests).