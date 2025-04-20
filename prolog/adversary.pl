% adversary.pl - Módulo para gestionar el guardia y su comportamiento
:- module(adversary, [
    guard_location/1,
    set_initial_guard_location/1,
    initialize_guard/0,
    move_guard/0,
    find_best_move/2,
    minimax/5,
    evaluate_state/3,
    capture_player/0,
    reset_after_capture/0,
    % New functionality for movement types
    guard_movement_type/1,
    set_guard_movement_type/1
]).

:- use_module(facts).
:- use_module(state).
:- use_module(rules).
:- use_module(library(clpfd)).

:- dynamic guard_location/1.
:- dynamic initial_guard_location/1.
:- dynamic guard_movement_type/1.

% Establecer el tipo de movimiento del guardia (predictive o random)
set_guard_movement_type(Type) :-
    retractall(guard_movement_type(_)),
    assertz(guard_movement_type(Type)).

% Establecer la ubicación inicial del guardia
set_initial_guard_location(Room) :-
    retractall(initial_guard_location(_)),
    assertz(initial_guard_location(Room)),
    retractall(guard_location(_)),
    assertz(guard_location(Room)).

% Inicializar el guardia al inicio del juego
initialize_guard :-
    initial_guard_location(Room),
    retractall(guard_location(_)),
    assertz(guard_location(Room)),
    % Establecer movimiento predictivo por defecto si no está definido
    (guard_movement_type(_) -> true ; set_guard_movement_type(predictive)).

% Si no existe ubicación inicial, usar la habitación 'b' por defecto
initialize_guard :-
    \+ initial_guard_location(_),
    assertz(initial_guard_location(b)),
    assertz(guard_location(b)),
    % Establecer movimiento predictivo por defecto
    (guard_movement_type(_) -> true ; set_guard_movement_type(predictive)).

% Mover el guardia según el tipo de movimiento configurado
move_guard :-
    % Solo mover si el juego está en el modo adversario
    main:game_mode(adversary),
    guard_location(CurrentRoom),
    state:player_location(PlayerRoom),
    % No mover si ya capturó al jugador (están en la misma habitación)
    CurrentRoom \= PlayerRoom,
    % Seleccionar el tipo de movimiento
    guard_movement_type(Type),
    (Type = predictive ->
        % Usar minimax para movimiento predictivo
        find_best_move(CurrentRoom, NextRoom)
    ;
        % Usar movimiento aleatorio
        find_random_move(CurrentRoom, NextRoom)
    ),
    % Mover el guardia a la nueva habitación
    retractall(guard_location(_)),
    assertz(guard_location(NextRoom)),
    format("El guardia se mueve de la habitación ~w a la habitación ~w.~n", [CurrentRoom, NextRoom]),
    % Verificar si el guardia capturó al jugador
    capture_player.

% No hacer nada si ya capturó al jugador o no estamos en modo adversario
move_guard.

% Buscar un movimiento aleatorio para el guardia
find_random_move(CurrentRoom, NextRoom) :-
    % Obtener todas las habitaciones conectadas con puertas
    findall(Next, facts:door(CurrentRoom, Next, _), PossibleMoves),
    % Si no hay movimientos posibles, quedarse en la misma habitación
    (PossibleMoves = [] -> 
        NextRoom = CurrentRoom
    ;
        % Seleccionar una habitación al azar
        random_member(NextRoom, PossibleMoves)
    ).

% Verificar si el guardia capturó al jugador (ambos en la misma habitación)
capture_player :-
    guard_location(Room),
    state:player_location(Room),
    format("¡El guardia te ha capturado en la habitación ~w!~n", [Room]),
    reset_after_capture.

% No hacer nada si el guardia no ha capturado al jugador
capture_player.

% Reiniciar posiciones después de una captura
reset_after_capture :-
    % Devolver al jugador a la habitación inicial
    retractall(state:player_location(_)),
    assertz(state:player_location(a)),
    % Devolver al guardia a su posición inicial
    initial_guard_location(InitialRoom),
    retractall(guard_location(_)),
    assertz(guard_location(InitialRoom)),
    format("Has sido devuelto a la habitación inicial 'a'.~n"),
    format("El guardia ha vuelto a su posición inicial en la habitación ~w.~n", [InitialRoom]).

% Encontrar el mejor movimiento para el guardia usando minimax
find_best_move(CurrentRoom, BestMove) :-
    % Obtener todos los movimientos posibles para el guardia
    findall(NextRoom, facts:door(CurrentRoom, NextRoom, _), PossibleMoves),
    % Si no hay movimientos posibles, quedarse en la misma habitación
    (PossibleMoves = [] -> 
        BestMove = CurrentRoom
    ;
        % Usar minimax para evaluar cada movimiento posible
        state:player_location(PlayerRoom),
        % Profundidad máxima para el algoritmo minimax
        MaxDepth = 3,
        % Para cada movimiento posible, calcular su valoración usando minimax
        findall(Value-Move, (
            member(Move, PossibleMoves),
            % Llamar a minimax con profundidad inicial
            minimax(Move, PlayerRoom, MaxDepth, Value, max)
        ), ValueMoves),
        % Ordenar los movimientos por valor y seleccionar el mejor
        keysort(ValueMoves, SortedMoves),
        % El mejor movimiento es el que tiene el valor más alto
        last(SortedMoves, BestValue-BestMove),
        % Si hay empate, elegir aleatoriamente entre los mejores
        findall(M, member(BestValue-M, ValueMoves), BestMoves),
        random_member(BestMove, BestMoves)
    ).

% Algoritmo minimax para encontrar el mejor movimiento
% minimax(+GuardRoom, +PlayerRoom, +Depth, -Value, +Player)
minimax(_, _, 0, Value, _) :-
    % Caso base: evaluar el estado actual cuando llegamos a profundidad 0
    guard_location(GuardRoom),
    state:player_location(PlayerRoom),
    evaluate_state(GuardRoom, PlayerRoom, Value),
    !.

minimax(GuardRoom, PlayerRoom, _, 100, _) :-
    % Caso especial: captura (valor máximo para el guardia)
    GuardRoom = PlayerRoom,
    !.

minimax(GuardRoom, PlayerRoom, Depth, Value, max) :-
    % Turno del guardia (maximizador)
    % Obtener todos los movimientos posibles para el guardia
    findall(NextRoom, facts:door(GuardRoom, NextRoom, _), GuardMoves),
    (GuardMoves = [] ->
        % Si no hay movimientos posibles, evaluar la posición actual
        evaluate_state(GuardRoom, PlayerRoom, Value)
    ;
        % Para cada movimiento posible del guardia
        NextDepth is Depth - 1,
        findall(MoveValue, (
            member(NextGuardRoom, GuardMoves),
            minimax(NextGuardRoom, PlayerRoom, NextDepth, MoveValue, min)
        ), AllMoveValues),
        % Seleccionar el máximo valor
        max_list(AllMoveValues, Value)
    ).

minimax(GuardRoom, PlayerRoom, Depth, Value, min) :-
    % Turno del jugador (minimizador)
    % Obtener todos los movimientos posibles para el jugador
    findall(NextRoom, 
           (facts:door(PlayerRoom, NextRoom, _), state:door_state(PlayerRoom, NextRoom, unlocked)), 
           PlayerMoves),
    (PlayerMoves = [] ->
        % Si no hay movimientos posibles, evaluar la posición actual
        evaluate_state(GuardRoom, PlayerRoom, Value)
    ;
        % Para cada movimiento posible del jugador
        NextDepth is Depth - 1,
        findall(MoveValue, (
            member(NextPlayerRoom, PlayerMoves),
            minimax(GuardRoom, NextPlayerRoom, NextDepth, MoveValue, max)
        ), AllMoveValues),
        % Seleccionar el mínimo valor
        min_list(AllMoveValues, Value)
    ).

% Función de evaluación para el estado del juego
% Distancia más corta = mejor para el guardia
evaluate_state(GuardRoom, PlayerRoom, Value) :-
    GuardRoom = PlayerRoom,
    % El guardia capturó al jugador (valor máximo)
    Value = 100,
    !.

evaluate_state(GuardRoom, PlayerRoom, Value) :-
    % Calcular la distancia entre el guardia y el jugador (valor inverso)
    shortest_path_length(GuardRoom, PlayerRoom, PathLength),
    % Valor más alto = mejor para el guardia
    Value is 50 - PathLength.

% Algoritmo BFS para encontrar la longitud del camino más corto entre dos habitaciones
shortest_path_length(Start, Goal, Length) :-
    shortest_path_length([Start], Goal, [Start], 0, Length).

shortest_path_length([Goal|_], Goal, _, Length, Length) :- !.
shortest_path_length([Current|Rest], Goal, Visited, CurrentLength, Length) :-
    % Obtener las habitaciones adyacentes no visitadas
    findall(Next, 
           (facts:door(Current, Next, _), \+ member(Next, Visited)), 
           NextRooms),
    % Añadir las nuevas habitaciones a la lista de visitados
    append(Visited, NextRooms, NewVisited),
    % Añadir las nuevas habitaciones al final de la cola
    append(Rest, NextRooms, NewQueue),
    % Incrementar la longitud del camino
    NextLength is CurrentLength + 1,
    % Continuar la búsqueda
    shortest_path_length(NewQueue, Goal, NewVisited, NextLength, Length).
shortest_path_length([], _, _, _, 999).  % No se encontró un camino