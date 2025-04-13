% rules.pl
:- module(rules, [
    can_move/2,
    initialize_game/0,
    game_stats/0
]).

:- use_module(state).
:- use_module(facts).
:- use_module(constraints).
:- use_module(library(clpfd)).

% Check if player can move between rooms
can_move(From, To) :-
    state:door_state(From, To, unlocked).

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