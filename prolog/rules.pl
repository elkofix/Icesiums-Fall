
:- module(rules, [
    can_move/2,
    initialize_game/0,
    game_stats/0,
    satisfy_requirements/1,
]).

:- use_module(state).
:- use_module(facts).
:- use_module(constraints).
:- use_module(library(clpfd)).

% Check if player can move between rooms
can_move(From, To) :-
    state:door_state(From, To, unlocked).

%==========
can_move(From, To) :-
    state:door_state(From, To, locked),
    facts:door_requirements(From, To, Reqs),
    satisfy_requirements(Reqs).
    
satisfy_requirements(Reqs) :-
    maplist(satisfy_requirement, Reqs).

% Predicado auxiliar para verificar requisitos
satisfy_requirements([]).
satisfy_requirements([Req|Rest]) :-
    satisfy_requirement(Req),
    satisfy_requirements(Rest).

satisfy_requirement(has_key(Key)) :-
    state:has_key(Key).
satisfy_requirement(puzzle_solved(Puzzle)) :-
    state:puzzle_solved(Puzzle).

%=========

% Initialize game state
initialize_game :-
    retractall(state:player_location(_)),
    retractall(state:inventory(_)),
    retractall(state:has_piece(_, _)),
    retractall(state:object_moved(_)),
    retractall(state:puzzle_solved(_)),
    retractall(state:door_state(_, _, _)),
    retractall(state:key_dropped(_, _)),          % Limpiar llaves dejadas
    retractall(state:piece_dropped(_, _, _)),     % Limpiar piezas dejadas
    
    % Initialize constraints
    constraints:initialize_constraints,
    
    % Set initial state
    assertz(state:player_location(a)),
    assertz(state:inventory([])),
    
    % Set up doors based on facts
    forall(facts:door(X, Y, State), assertz(state:door_state(X, Y, State))),
    
    writeln("Game initialized! You are in room A."),
    writeln("Type 'help.' for available commands."),
    game_stats.

% Display game stats including constraints
game_stats :-
    constraints:move_count(Moves),
    constraints:max_moves(MaxMoves),
    RemainingMoves #= MaxMoves - Moves,
    format("Game stats:~n"),
    format("- Moves used: ~w/~w (~w remaining)~n", [Moves, MaxMoves, RemainingMoves]),
    
    % Room turns
    writeln("- Room visits:"),
    forall(facts:room(R), (
        constraints:turns_in_room(R, Turns),
        format("  * Room ~w: ~w time(s)~n", [R, Turns])
    )),
    
    % Traps
    findall(Room-Limit, constraints:trap(Room, turns(Limit)), Traps),
    (Traps = [] -> 
        true
    ;
        writeln("- Active traps:"),
        forall(member(Room-Limit, Traps), (
            constraints:turns_in_room(Room, Turns),
            SafeTurns #= Limit - Turns,
            format("  * Room ~w: Activates after ~w turn(s) - ~w turn(s) safe remaining~n", 
                  [Room, Limit, SafeTurns])
        ))
    ),
    
    % Inventory limits
    constraints:can_carry(key, KeyLimit),
    constraints:can_carry(piece, PieceLimit),
    constraints:count_items_of_type(key, KeyCount),
    constraints:count_items_of_type(piece, PieceCount),
    format("- Inventory limits:~n"),
    format("  * Keys: ~w/~w~n", [KeyCount, KeyLimit]),
    format("  * Puzzle pieces: ~w/~w~n", [PieceCount, PieceLimit]).


    puzzle_requirements_met(Puzzle) :-
    findall(Piece, facts:piece(Puzzle, Piece), Pieces),
    findall(P, (member(P, Pieces), state:has_piece(Puzzle, P)), Collected),
    length(Pieces, Total),
    length(Collected, Total).