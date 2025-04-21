:- module(test_graphs, [
    load_test_graph_1/0,
    load_test_graph_2/0,
    load_test_graph_3/0
    
]).

:- use_module(facts).
:- use_module(search).

% Grafo de prueba 1
load_test_graph_1 :-
    facts:clear_game_data,
    % Definir habitaciones
    facts:add_room(a), facts:add_room(b), facts:add_room(c), facts:add_room(d),
    % Definir puertas
    facts:add_door(a, b, unlocked),
    facts:add_door(b, c, unlocked),
    facts:add_door(c, d, unlocked),
    % Definir habitación final
    facts:set_final_room(d),
    writeln('Test graph 1 loaded successfully!').

% Grafo de prueba 2
load_test_graph_2 :-
    facts:clear_game_data,
    % Definir habitaciones
    facts:add_room(x), facts:add_room(d), facts:add_room(c),
    % Definir puertas
    facts:add_door(x, d, unlocked),
    facts:add_door(d, c, unlocked),
    % Definir habitación final
    facts:set_final_room(c),
    writeln('Test graph 2 loaded successfully!').

% Grafo de prueba 3
load_test_graph_3 :-
    facts:clear_game_data,
    % Definir habitaciones
    facts:add_room(p), facts:add_room(t), facts:add_room(r), facts:add_room(x),
    % Definir puertas
    facts:add_door(p, t, unlocked),
    facts:add_door(t, r, unlocked),
    facts:add_door(r, x, unlocked),
    % Definir habitación final
    facts:set_final_room(s),
    writeln('Test graph 3 loaded successfully!').

% Ejecutar pruebas automáticamente
run_tests :-
    writeln('Running tests for all graphs...'),
    writeln('Testing graph 1:'),
    load_test_graph_1,
    find_escape_solution,
    nl,
    writeln('Testing graph 2:'),
    load_test_graph_2,
    find_escape_solution,
    nl,
    writeln('Testing graph 3:'),
    load_test_graph_3,
    find_escape_solution,
    nl,
    writeln('All tests completed!').

% Ejecutar automáticamente al cargar el archivo
:- initialization(run_tests).