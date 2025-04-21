:- module(search_no_constraints, [
    find_escape_solution_no_constraints/0
]).

:- use_module(state).
:- use_module(facts).
% No importamos el modulo de restricciones ya que no las usaremos

% Funcion principal para encontrar solución de escape sin restricciones
find_escape_solution_no_constraints :-
    writeln('Buscando solución de escape sin restricciones de inventario...'),
    statistics(walltime, [Start|_]),  % Inicio del cronómetro

    % Obtener la habitación inicial y final
    state:player_location(StartRoom),
    facts:final_room(FinalRoom),
    format('Planificando escape desde ~w hasta ~w~n', [StartRoom, FinalRoom]),
    
    % Definir el estado inicial basado en la ubicación actual del jugador
    % Formato del estado: state(HabitaciónActual, Inventario, PuertasDesbloqueadas, PuzzlesResueltos, ObjetosMovidos, PiezasRecolectadas)
    InitialState = state(StartRoom, [], [], [], [], []),
    
    % Ejecutar BFS para encontrar una solución
    (bfs(InitialState, GoalState, Solution) ->
        statistics(walltime, [End|_]), 
        Time is End - Start,
        length(Solution, Steps),  

        format('¡Solución encontrada! Pasos para escapar: ~n'),
        print_solution(Solution),
        format('Solution found in ~3f seconds! ~n', [Time]),

        format('Total de pasos requeridos: ~w~n', [Steps]) 
    ;
        writeln('¡No se encontró solución de escape! La habitación podría ser irresoluble.')
    ).

% Implementación BFS para encontrar un camino desde el estado inicial hasta la habitación final
bfs(InitialState, GoalState, Steps) :-
    % Obtener la habitación final definida
    facts:final_room(FinalRoom),
    % Formato de cola: [queueItem(Estado, CaminoHastaEstado)]
    InitialQueue = [queueItem(InitialState, [])],
    bfs_loop(InitialQueue, [], FinalRoom, GoalState, Steps).

% Bucle BFS - cola vacía significa que no se encontró solución
bfs_loop([], _, _, _, _) :- 
    writeln('¡No se encontró solución!'),
    fail.

% Estado objetivo encontrado - devolver el camino
bfs_loop([queueItem(State, Path)|_], Visited, FinalRoom, State, Path) :-
    State = state(Room, _, _, _, _, _),
    Room = FinalRoom, !.  % El objetivo es llegar a la habitación final

% Procesar el siguiente estado en la cola
bfs_loop([queueItem(State, Path)|Queue], Visited, FinalRoom, GoalState, Steps) :-
    % Si ya visitamos este estado, saltarlo
    \+ member(State, Visited),
    
    % Obtener todos los posibles estados siguientes y acciones desde el estado actual
    findall(
        (NextState, Action),
        possible_action(State, NextState, Action),
        Transitions
    ),
    
    % Añadir nuevos estados a la cola con sus caminos
    add_to_queue(Transitions, Path, Queue, NewQueue),
    
    % Añadir el estado actual a visitados
    bfs_loop(NewQueue, [State|Visited], FinalRoom, GoalState, Steps).

% Saltar estados ya visitados
bfs_loop([_|Queue], Visited, FinalRoom, GoalState, Steps) :-
    bfs_loop(Queue, Visited, FinalRoom, GoalState, Steps).

% Añadir nuevos estados a la cola con sus caminos
add_to_queue([], _, Queue, Queue).
add_to_queue([(NextState, Action)|Rest], Path, Queue, FinalQueue) :-
    % Añadir acción al camino
    append(Path, [Action], NewPath),
    % Añadir nuevo estado a la cola
    append(Queue, [queueItem(NextState, NewPath)], TempQueue),
    % Procesar las transiciones restantes
    add_to_queue(Rest, Path, TempQueue, FinalQueue).

% Imprimir solución en un formato legible
print_solution([]) :- !.
print_solution([Action|Rest]) :-
    format('- ~w~n', [Action]),
    print_solution(Rest).

% Definir todas las posibles acciones desde un estado dado
% Formato del estado: state(HabitaciónActual, Inventario, PuertasDesbloqueadas, PuzzlesResueltos, ObjetosMovidos, PiezasRecolectadas)

% Acción: Moverse a otra habitación
possible_action(
    state(CurrentRoom, Inventory, UnlockedDoors, SolvedPuzzles, MovedObjects, CollectedPieces),
    state(NextRoom, Inventory, UnlockedDoors, SolvedPuzzles, MovedObjects, CollectedPieces),
    move_player(NextRoom)
) :-
    % Comprobar si existe una puerta entre habitaciones y está desbloqueada
    (
        (facts:door(CurrentRoom, NextRoom, unlocked) ; member((CurrentRoom, NextRoom), UnlockedDoors))
        ;
        (facts:door(NextRoom, CurrentRoom, unlocked) ; member((NextRoom, CurrentRoom), UnlockedDoors))
    ).

% Acción: Recoger una llave (sin comprobar límites)
possible_action(
    state(CurrentRoom, Inventory, UnlockedDoors, SolvedPuzzles, MovedObjects, CollectedPieces),
    state(CurrentRoom, [Key|Inventory], UnlockedDoors, SolvedPuzzles, MovedObjects, CollectedPieces),
    pick_key(Key)
) :-
    facts:key_in_room(CurrentRoom, Key),
    \+ member(Key, Inventory).
    % No hay comprobación de límite de inventario

% Acción: Mover un objeto para encontrar una pieza oculta
possible_action(
    state(CurrentRoom, Inventory, UnlockedDoors, SolvedPuzzles, MovedObjects, CollectedPieces),
    state(CurrentRoom, Inventory, UnlockedDoors, SolvedPuzzles, [Object|MovedObjects], NewCollectedPieces),
    move_object(Object)
) :-
    facts:object_in_room(CurrentRoom, Object),
    \+ member(Object, MovedObjects),
    % Comprobar si el objeto esconde una pieza
    (
        % Si el objeto esconde una pieza, añadirla a las piezas recolectadas
        facts:hides_piece(Object, Puzzle, Piece),
        \+ member((Puzzle, Piece), CollectedPieces),
        % Añadir pieza a las piezas recolectadas (sin comprobar límites)
        append([(Puzzle, Piece)], CollectedPieces, NewCollectedPieces)
    ;
        % Si no hay pieza, solo añadir objeto a objetos movidos
        \+ facts:hides_piece(Object, _, _),
        NewCollectedPieces = CollectedPieces
    ).

% Acción: Recoger una pieza visible (sin comprobar límites)
possible_action(
    state(CurrentRoom, Inventory, UnlockedDoors, SolvedPuzzles, MovedObjects, CollectedPieces),
    state(CurrentRoom, Inventory, UnlockedDoors, SolvedPuzzles, MovedObjects, [(Puzzle, Piece)|CollectedPieces]),
    pick_piece(Piece)
) :-
    facts:piece_in_room(CurrentRoom, Piece, Puzzle),
    \+ member((Puzzle, Piece), CollectedPieces).
    % No hay comprobación de límite de inventario

% Acción: Resolver un rompecabezas
possible_action(
    state(CurrentRoom, Inventory, UnlockedDoors, SolvedPuzzles, MovedObjects, CollectedPieces),
    state(CurrentRoom, Inventory, UnlockedDoors, [Puzzle|SolvedPuzzles], MovedObjects, RemainingPieces),
    solve_puzzle(Puzzle)
) :-
    % Comprobar si el rompecabezas puede resolverse en esta habitación
    facts:puzzle_room(Puzzle, CurrentRoom),
    \+ member(Puzzle, SolvedPuzzles),
    % Comprobar si el jugador tiene todas las piezas para este rompecabezas
    findall(Piece, facts:piece(Puzzle, Piece), AllPieces),
    all_pieces_collected(Puzzle, AllPieces, CollectedPieces),
    % Eliminar las piezas usadas del inventario
    remove_puzzle_pieces(Puzzle, CollectedPieces, RemainingPieces).

% Acción: Desbloquear una puerta
possible_action(
    state(CurrentRoom, Inventory, UnlockedDoors, SolvedPuzzles, MovedObjects, CollectedPieces),
    state(CurrentRoom, RemainingInventory, [(CurrentRoom, NextRoom)|UnlockedDoors], SolvedPuzzles, MovedObjects, CollectedPieces),
    unlock_door(CurrentRoom, NextRoom)
) :-
    % Comprobar si la puerta existe y está bloqueada
    facts:door(CurrentRoom, NextRoom, locked),
    \+ member((CurrentRoom, NextRoom), UnlockedDoors),
    % Comprobar requisitos para desbloquear
    facts:door_requirements(CurrentRoom, NextRoom, Requirements),
    % Comprobar cada requisito
    can_satisfy_requirements(Requirements, Inventory, SolvedPuzzles, ConsumedKeys),
    % Eliminar llaves consumidas del inventario
    remove_keys(ConsumedKeys, Inventory, RemainingInventory).

% Predicados auxiliares

% Comprobar si todas las piezas para un rompecabezas están recolectadas
all_pieces_collected(_, [], _).
all_pieces_collected(Puzzle, [Piece|Rest], CollectedPieces) :-
    member((Puzzle, Piece), CollectedPieces),
    all_pieces_collected(Puzzle, Rest, CollectedPieces).

% Eliminar piezas de rompecabezas usadas del inventario
remove_puzzle_pieces(_, [], []).
remove_puzzle_pieces(Puzzle, [(P, Piece)|Rest], Remaining) :-
    P = Puzzle, !,
    remove_puzzle_pieces(Puzzle, Rest, Remaining).
remove_puzzle_pieces(Puzzle, [(P, Piece)|Rest], [(P, Piece)|Remaining]) :-
    P \= Puzzle,
    remove_puzzle_pieces(Puzzle, Rest, Remaining).

% Comprobar si todos los requisitos pueden satisfacerse
can_satisfy_requirements([], _, _, []).
can_satisfy_requirements([has_key(Key)|Rest], Inventory, SolvedPuzzles, [Key|ConsumedKeys]) :-
    member(Key, Inventory),
    can_satisfy_requirements(Rest, Inventory, SolvedPuzzles, ConsumedKeys).
can_satisfy_requirements([puzzle_solved(Puzzle)|Rest], Inventory, SolvedPuzzles, ConsumedKeys) :-
    member(Puzzle, SolvedPuzzles),
    can_satisfy_requirements(Rest, Inventory, SolvedPuzzles, ConsumedKeys).

% Eliminar llaves consumidas del inventario
remove_keys([], Inventory, Inventory).
remove_keys([Key|Rest], Inventory, RemainingInventory) :-
    select(Key, Inventory, TempInventory),
    remove_keys(Rest, TempInventory, RemainingInventory).