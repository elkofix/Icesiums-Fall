    % constraints.pl - Modified for dynamic constraints
    :- module(constraints, [
        max_moves/1,
        max_b_visits/1,
        valid_state/4,
        can_carry/2,
        trap/2, 
        turns_in_room/2,
        move_count/1,
        increment_move_count/0,
        increment_room_turn/1,
        check_move_limit/0,
        check_trap/1,
        check_inventory_limit/1,
        initialize_constraints/0,
        count_items_of_type/2,
        % New dynamic constraint functions
        set_max_moves/1,
        set_max_b_visits/1,
        set_carry_limit/2,
        add_room_trap/2,
        clear_constraints/0,
        load_default_constraints/0,
        create_custom_constraints/0
    ]).

    :- use_module(library(clpfd)).
    :- use_module(facts).
    :- use_module(state).

    :- dynamic max_moves/1.
    :- dynamic max_b_visits/1.
    :- dynamic can_carry/2.
    :- dynamic trap/2.
    :- dynamic move_count/1.
    :- dynamic turns_in_room/2.

    % Dynamic constraint modification functions
    set_max_moves(Limit) :-
        retractall(max_moves(_)),
        assertz(max_moves(Limit)).

    set_max_b_visits(Limit) :-
        retractall(max_b_visits(_)),
        assertz(max_b_visits(Limit)).

    set_carry_limit(ItemType, Limit) :-
        retractall(can_carry(ItemType, _)),
        assertz(can_carry(ItemType, Limit)).

    add_room_trap(Room, TurnLimit) :-
        retractall(trap(Room, _)),
        assertz(trap(Room, turns(TurnLimit))).

    % Clear all constraints
    clear_constraints :-
        retractall(max_moves(_)),
        retractall(max_b_visits(_)),
        retractall(can_carry(_, _)),
        retractall(trap(_, _)).

    % Load default constraints
    load_default_constraints :-
        clear_constraints,
        % Default maximum moves
        set_max_moves(30),
        % Default maximum visits to room B
        set_max_b_visits(3),
        % Default inventory limits
        set_carry_limit(key, 2),
        set_carry_limit(piece, 4),
        % Default room traps
        add_room_trap(c, 3).

    % Interactive constraint setup
    create_custom_constraints :-
        clear_constraints,
        writeln('Let\'s set up game constraints.'),
        
        % Set max moves
        writeln('Enter maximum moves allowed (default is 30):'),
        read(MaxMoves),
        set_max_moves(MaxMoves),
        
        % Set max B visits
        writeln('Enter maximum visits to room b (default is 3):'),
        read(MaxBVisits),
        set_max_b_visits(MaxBVisits),
        
        % Set key carry limit
        writeln('Enter maximum keys player can carry (default is 2):'),
        read(KeyLimit),
        set_carry_limit(key, KeyLimit),
        
        % Set piece carry limit
        writeln('Enter maximum puzzle pieces player can carry (default is 4):'),
        read(PieceLimit),
        set_carry_limit(piece, PieceLimit),
        
        % Set room traps
        writeln('Do you want to add room traps? (yes/no):'),
        read(AddTraps),
        (AddTraps = yes ->
            custom_room_traps
        ;
            writeln('No traps will be added.')
        ),
        
        writeln('Custom constraints have been set successfully!').

    % Helper for adding room traps
    custom_room_traps :-
        writeln('Enter room and turn limit for trap (format: room-turns) or "done":'),
        read(TrapInput),
        (TrapInput = done ->
            writeln('Trap setup complete.')
        ;
            TrapInput = Room-Turns,
            add_room_trap(Room, Turns),
            writeln('Trap added. Enter another trap or "done":'),
            custom_room_traps
        ).

    % Current move counter
    move_count(0).

    % Increment the global move counter
    increment_move_count :-
        retract(move_count(Count)),
        NewCount #= Count + 1,
        assertz(move_count(NewCount)).

    % Increment the turn counter for a specific room
    increment_room_turn(Room) :-
        retract(turns_in_room(Room, Count)),
        NewCount #= Count + 1,
        assertz(turns_in_room(Room, NewCount)),
        check_trap(Room).

    % Check if move limit has been reached
    check_move_limit :-
        move_count(Count),
        max_moves(Max),
        Count #< Max.  % Only check, don't show message yet
    check_move_limit :-
        move_count(Count),
        max_moves(Max),
        Count #>= Max,
        writeln("You've reached the maximum number of moves!"),
        writeln("Game over. Type 'init_game.' to restart."),
        fail.

    % Check if trap is activated in a room
    check_trap(Room) :-
        trap(Room, turns(Limit)),
        turns_in_room(Room, Turns),
        Turns #>= Limit,
        format("TRAP ACTIVATED in room ~w after ~w turns!~n", [Room, Turns]),
        writeln("You've been caught in a trap!"),
        writeln("Game over. Type 'init_game.' to restart."),
        fail.
    check_trap(_).

    % Check inventory limits before picking up items
    check_inventory_limit(Type) :-
        can_carry(Type, Limit),
        count_items_of_type(Type, Count),
        Count #< Limit.
    check_inventory_limit(Type) :-
        can_carry(Type, Limit),
        count_items_of_type(Type, Count),
        Count #>= Limit,
        format("You cannot carry more items of type ~w. Limit: ~w~n", [Type, Limit]),
        fail.

    % Count items of a specific type in inventory
    count_items_of_type(key, Count) :-
        findall(K, state:has_key(K), Keys),
        length(Keys, Count).
    count_items_of_type(piece, Count) :-
        findall(Piece-Puzzle, state:has_piece(Puzzle, Piece), Pieces),
        length(Pieces, Count).

    % Initialize constraints for all rooms in the game
    initialize_constraints :-
        retractall(move_count(_)),
        assertz(move_count(0)),
        retractall(turns_in_room(_, _)),
        % Initialize turn counters for all defined rooms
        findall(Room, facts:room(Room), Rooms),
        forall(member(R, Rooms), assertz(turns_in_room(R, 0))).

    % Check if the current state is valid
    % This predicate checks if the current state of the game is valid based on the constraints defined above.
    valid_state(Room, Keys, Moves, BVisits) :-
        % Verificar límite de movimientos
        max_moves(MaxMoves),
        Moves =< MaxMoves,
        
        % Verificar límite de visitas a B si estamos en esa habitación
        (Room == b -> 
            max_b_visits(MaxB),
            BVisits =< MaxB
        ; true),
        
        % Verificar que las llaves sean válidas
        valid_keys(Keys),
        
        % Verificar que la habitación existe
        facts:room(Room).

    % Predicado auxiliar para validar llaves
    valid_keys([]).
    valid_keys([Key|Rest]) :-
        (facts:key_in_room(_, Key) ; state:has_key(Key)), 
        valid_keys(Rest).