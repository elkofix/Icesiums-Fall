    % facts.pl - Modified to support dynamic game configuration
    :- module(facts, [
        room/1, door/3, key_in_room/2,
        object_in_room/2, hides_piece/3, piece/2,
        piece_location/3, puzzle_room/2,
        door_requirements/3, puzzle/1,
        piece_in_room/3,
        % Dynamic definition functions
        add_room/1, add_door/3, add_key/2,
        add_object/2, add_puzzle/1, add_piece/2,
        hide_piece/3, set_puzzle_room/2,
        set_door_requirements/3, set_final_room/1,
        % Game configuration functions
        clear_game_data/0,
        load_predefined_game/0,
        create_custom_game/0,
        set_game_mode/1,
        choose_game_mode/0
    ]).
    :- use_module(adversary).
    :- use_module(state).

    :- dynamic room/1, door/3, key_in_room/2,
        object_in_room/2, hides_piece/3, piece/2,
        piece_location/3, puzzle_room/2,
        door_requirements/3, puzzle/1,
        piece_in_room/3, game_mode/1. 
    % Dynamic definition functions

    :- (current_predicate(game_mode/1) -> true ; assertz(game_mode(standard))).

    set_game_mode(Mode) :-
        retractall(game_mode(_)),
        assertz(game_mode(Mode)).

    choose_game_mode :-
        writeln("Do you want to play with an adversary?"),
        writeln("1. No - Standard mode"),
        writeln("2. Yes - Adversary mode"),
        writeln("Enter your choice (1 or 2):"),
        read(ModeChoice),
        (ModeChoice = 1 ->
            set_game_mode(standard),
            true
        ; ModeChoice = 2 ->
            set_game_mode(adversary),
            true
        ;
            writeln("Invalid choice. Please enter 1 or 2."),
            choose_game_mode
        ).


    add_room(Room) :-
        assertz(room(Room)).

    add_door(From, To, State) :-
        assertz(door(From, To, State)).

    add_key(Room, Key) :-
        assertz(key_in_room(Room, Key)).

    add_object(Room, Object) :-
        assertz(object_in_room(Room, Object)).

    add_puzzle(Puzzle) :-
        assertz(puzzle(Puzzle)).

    add_piece(Puzzle, Piece) :-
        assertz(piece(Puzzle, Piece)).

    hide_piece(Object, Puzzle, Piece) :-
        assertz(hides_piece(Object, Puzzle, Piece)),
        (object_in_room(Room, Object) -> 
            assertz(piece_location(Puzzle, Piece, Room))
        ;
            true
        ).

    set_puzzle_room(Puzzle, Room) :-
        assertz(puzzle_room(Puzzle, Room)).

    set_door_requirements(From, To, Requirements) :-
        assertz(door_requirements(From, To, Requirements)).

    add_visible_piece(Room, Piece, Puzzle) :-
        assertz(piece_in_room(Room, Piece, Puzzle)),
        assertz(piece_location(Puzzle, Piece, Room)).

    set_final_room(Room) :-
        retractall(final_room(_)),
        assertz(final_room(Room)).

    % Function to clean all game data
    clear_game_data :-
        retractall(room(_)),
        retractall(door(_, _, _)),
        retractall(key_in_room(_, _)),
        retractall(object_in_room(_, _)),
        retractall(hides_piece(_, _, _)),
        retractall(piece(_, _)),
        retractall(piece_location(_, _, _)),
        retractall(puzzle_room(_, _)),
        retractall(door_requirements(_, _, _)),
        retractall(puzzle(_)),
        retractall(piece_in_room(_, _, _)),
        retractall(final_room(_)).

    % Load the predefined game setup
    load_predefined_game :-
        clear_game_data,
        
        add_room(a), add_room(b), add_room(c), add_room(d),
        set_final_room(d),

        % Add doors
        add_door(a, b, locked),
        add_door(b, a, locked),
        add_door(b, c, locked),
        add_door(c, b, locked),
        add_door(c, d, locked),
        add_door(d, c, locked),
        
        % Add keys
        add_key(a, key1),
        add_key(b, key2),
        
        % Add objects
        add_object(a, box),
        add_object(a, painting),
        add_object(b, rug),
        add_object(b, bookshelf),
        add_object(c, cabinet),
        
        % Add puzzles
        add_puzzle(puzzle1),
        add_puzzle(puzzle2),
        
        % Add pieces
        add_piece(puzzle1, p1),
        add_piece(puzzle1, p2),
        add_piece(puzzle1, p3),
        add_piece(puzzle2, p4),
        add_piece(puzzle2, p5),
        add_piece(puzzle2, p6),
        
        % Set piece locations
        add_visible_piece(a, p3, puzzle1),
        add_visible_piece(c, p4, puzzle2),
        
        % Hide pieces in objects
        hide_piece(rug, puzzle1, p1),
        hide_piece(bookshelf, puzzle1, p2),
        hide_piece(box, puzzle2, p5),
        hide_piece(painting, puzzle2, p6),
        
        % Set puzzle rooms
        set_puzzle_room(puzzle1, b),
        set_puzzle_room(puzzle2, c),
        
        % Set door requirements
        set_door_requirements(a, b, [has_key(key1)]),
        set_door_requirements(b, a, [has_key(key1)]),
        set_door_requirements(b, c, [puzzle_solved(puzzle1)]),
        set_door_requirements(c, b, [puzzle_solved(puzzle1)]),
        set_door_requirements(c, d, [puzzle_solved(puzzle2), has_key(key2)]),
        set_door_requirements(d, c, [puzzle_solved(puzzle2), has_key(key2)]),
        
        (game_mode(adversary) ->
            adversary:set_initial_guard_location(d)
        ;
            true
        ),
        writeln('Predefined game loaded successfully!').

    % Interactive game creation
    create_custom_game :-
        clear_game_data,
        writeln('Creating a custom escape room game.'),
        writeln('Let\'s set up the rooms first.'),
        custom_game_rooms.
        (game_mode(adversary) ->
            custom_game_guard_location,
            % Ask about guard movement type
            ask_guard_movement_type
        ;
            true
        ).

    custom_game_rooms :-
        writeln('Enter room names, one at a time (enter "done" when finished):'),
        read(RoomInput),
        (RoomInput = done ->
            writeln('Room setup complete.'),
            custom_game_doors
        ;
            add_room(RoomInput),
            writeln('Room added. Enter another room or "done":'),
            custom_game_rooms
        ).

    custom_game_doors :-
        writeln('Now let\'s set up the doors between rooms.'),
        writeln('Format: from_room-to_room-state (locked/unlocked) or "done":'),
        read(DoorInput),
        (DoorInput = done ->
            writeln('Door setup complete.'),
            custom_game_keys
        ;
            DoorInput = From-To-State,
            add_door(From, To, State),
            writeln('Door added. Enter another door or "done":'),
            custom_game_doors
        ).

    custom_game_keys :-
        writeln('Now let\'s place keys in rooms.'),
        writeln('Format: room-key_name or "done":'),
        read(KeyInput),
        (KeyInput = done ->
            writeln('Key setup complete.'),
            custom_game_objects
        ;
            KeyInput = Room-Key,
            add_key(Room, Key),
            writeln('Key added. Enter another key placement or "done":'),
            custom_game_keys
        ).

    custom_game_objects :-
        writeln('Now let\'s place objects in rooms.'),
        writeln('Format: room-object_name or "done":'),
        read(ObjInput),
        (ObjInput = done ->
            writeln('Object setup complete.'),
            custom_game_puzzles
        ;
            ObjInput = Room-Obj,
            add_object(Room, Obj),
            writeln('Object added. Enter another object placement or "done":'),
            custom_game_objects
        ).

    custom_game_puzzles :-
        writeln('Now let\'s create puzzles.'),
        writeln('Enter puzzle names, one at a time (enter "done" when finished):'),
        read(PuzzleInput),
        (PuzzleInput = done ->
            writeln('Puzzle setup complete.'),
            custom_game_pieces
        ;
            add_puzzle(PuzzleInput),
            writeln('Puzzle added. Enter another puzzle or "done":'),
            custom_game_puzzles
        ).

    custom_game_pieces :-
        writeln('Now let\'s assign pieces to puzzles.'),
        writeln('Format: puzzle-piece_name or "done":'),
        read(PieceInput),
        (PieceInput = done ->
            writeln('Piece setup complete.'),
            custom_game_visible_pieces
        ;
            PieceInput = Puzzle-Piece,
            add_piece(Puzzle, Piece),
            writeln('Piece added to puzzle. Enter another piece or "done":'),
            custom_game_pieces
        ).

    custom_game_visible_pieces :-
        writeln('Now let\'s place visible pieces in rooms.'),
        writeln('Format: room-piece-puzzle or "done":'),
        read(VisibleInput),
        (VisibleInput = done ->
            writeln('Visible piece setup complete.'),
            custom_game_hidden_pieces
        ;
            VisibleInput = Room-Piece-Puzzle,
            add_visible_piece(Room, Piece, Puzzle),
            writeln('Visible piece added. Enter another visible piece or "done":'),
            custom_game_visible_pieces
        ).

    custom_game_hidden_pieces :-
        writeln('Now let\'s hide pieces in objects.'),
        writeln('Format: object-puzzle-piece or "done":'),
        read(HiddenInput),
        (HiddenInput = done ->
            writeln('Hidden piece setup complete.'),
            custom_game_puzzle_rooms
        ;
            HiddenInput = Object-Puzzle-Piece,
            hide_piece(Object, Puzzle, Piece),
            writeln('Hidden piece added. Enter another hidden piece or "done":'),
            custom_game_hidden_pieces
        ).

    custom_game_puzzle_rooms :-
        writeln('Now let\'s set which room each puzzle must be solved in.'),
        writeln('Format: puzzle-room or "done":'),
        read(PuzzleRoomInput),
        (PuzzleRoomInput = done ->
            writeln('Puzzle room setup complete.'),
            custom_game_door_requirements
        ;
            PuzzleRoomInput = Puzzle-Room,
            set_puzzle_room(Puzzle, Room),
            writeln('Puzzle room set. Enter another puzzle room or "done":'),
            custom_game_puzzle_rooms
        ).

    custom_game_door_requirements :-
        writeln('Finally, let\'s set the requirements for each door.'),
        writeln('Format: from_room-to_room-[req1,req2,...] or "done":'),
        writeln('Requirements can be has_key(KeyName) or puzzle_solved(PuzzleName)'),
        read(ReqInput),
        (ReqInput = done ->
            writeln('Door requirements setup complete.'),
            custom_game_final_room
        ;
            ReqInput = From-To-Reqs,
            set_door_requirements(From, To, Reqs),
            writeln('Door requirement set. Enter another door requirement or "done":'),
            custom_game_door_requirements
        ).

    custom_game_final_room :-
        writeln('Finally, which room is the final goal (exit) room?'),
        findall(R, room(R), Rooms),
        format('Available rooms: ~w~n', [Rooms]),
        read(FinalRoom),
        (room(FinalRoom) ->
            set_final_room(FinalRoom),
            writeln('Final room set successfully!'),
            writeln('Custom game created successfully!')
        ;
            writeln('Invalid room. Please choose from the available rooms:'),
            format('~w~n', [Rooms]),
            custom_game_final_room
        ).

    custom_game_guard_location :-
        writeln('Let\'s set the initial guard location.'),
        findall(R, room(R), Rooms),
        format('Available rooms: ~w~n', [Rooms]),
        writeln('Enter the room where the guard should start:'),
        read(GuardRoom),
        (room(GuardRoom) ->
            adversary:set_initial_guard_location(GuardRoom),
            format('Guard will start in room ~w.~n', [GuardRoom])
        ;
            writeln('Invalid room. Please choose from the available rooms:'),
            format('~w~n', [Rooms]),
            custom_game_guard_location
        ).

    ask_guard_movement_type :-
        writeln('How should the guard move?'),
        writeln('1. Predictive'),
        writeln('2. Random'),
        read(Choice),
        (Choice = 1 ->
            adversary:set_guard_movement_type(predictive),
            writeln('Guard will use predictive movement.')
        ; Choice = 2 ->
            adversary:set_guard_movement_type(random),
            writeln('Guard will use random movement.')
        ;
            writeln('Invalid choice. Please select 1 or 2:'),
            ask_guard_movement_type
        ).

    % Predicado principal para generar el JSON del grafo de dependencias
    generate_dependency_graph_json(JSON) :-
        findall(Room, room(Room), Rooms),
        findall(door(From, To, State), state:door_state(From, To, State), Doors), % Usar door_state en lugar de door
        player_location(CurrentRoom),
        format_rooms(Rooms, CurrentRoom, RoomsJSON),
        format_doors(Doors, DoorsJSON),
        format(atom(JSON), '{\n  "nodes": [\n~w\n  ],\n  "links": [\n~w\n  ]\n}', [RoomsJSON, DoorsJSON]).

    % Formatea las habitaciones incluyendo la ubicación actual del jugador
    format_rooms(Rooms, CurrentRoom, RoomsJSON) :-
        maplist(format_room(CurrentRoom), Rooms, RoomAtoms),
        atomic_list_concat(RoomAtoms, ',\n', RoomsJSON).

    format_room(CurrentRoom, Room, RoomJSON) :-
        (game_mode(adversary), adversary:guard_location(Room) -> 
            HasGuard = true
        ; 
            HasGuard = false
        ),
        (Room == CurrentRoom ->
            Current = true
        ;
            Current = false
        ),
        (state:has_key(_) ->  % Usamos el predicado has_key/1 del mdulo state
            HasKeys = true
        ;
            HasKeys = false
        ),
        format(atom(RoomJSON), '    {"id": "~w", "name": "~w", "has_guard": ~w, "current": ~w, "has_keys": ~w}', 
            [Room, Room, HasGuard, Current, HasKeys]).

    % Formatea las puertas usando el estado actual
    format_doors(Doors, DoorsJSON) :-
        maplist(format_door, Doors, DoorAtoms),
        atomic_list_concat(DoorAtoms, ',\n', DoorsJSON).

    format_door(door(From, To, State), DoorJSON) :-
        ( door_requirements(From, To, Reqs) ->
            format_requirements(Reqs, ReqsJSON),
            format(atom(DoorJSON), '    {"source": "~w", "target": "~w", "state": "~w", "requirements": [~w]}', 
                [From, To, State, ReqsJSON])
        ;
            format(atom(DoorJSON), '    {"source": "~w", "target": "~w", "state": "~w", "requirements": []}', 
                [From, To, State])
        ).

    format_requirements(Reqs, ReqsJSON) :-
        maplist(term_to_atom, Reqs, AtomList),
        maplist(wrap_in_quotes, AtomList, QuotedList),
        atomic_list_concat(QuotedList, ',', ReqsJSON).

    wrap_in_quotes(Atom, Quoted) :-
        format(atom(Quoted), '"~w"', [Atom]).