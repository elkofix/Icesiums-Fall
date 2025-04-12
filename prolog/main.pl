
:- use_module(state).
:- use_module(facts).
:- use_module(rules).

% Help command
help :-
    writeln("Available commands:"),
    writeln("- look. : Look around the room"),
    writeln("- move_player(Room). : Move to another room"),
    writeln("- pick_key(Key). : Pick up a key"),
    writeln("- pick_piece(Piece). : Pick up a puzzle piece"),
    writeln("- move_object(Object). : Examine an object"),
    writeln("- inventory. : View your inventory"),
    writeln("- puzzle_status(Puzzle). : Check status of a puzzle"),
    writeln("- solve_puzzle(Puzzle). : Try to solve a puzzle"),
    writeln("- unlock_door(From, To). : Unlock a door"),
    writeln("- init_game. : Reset the game"),
    writeln("- help. : Show this help").

% Look around the current room
look :-
    state:player_location(Room),
    format("You are in room ~w.~n~n", [Room]),
    
    % Interactive objects
    findall(Obj, facts:object_in_room(Room, Obj), Objects),
    (Objects = [] ->
        writeln("No interactive objects here.")
    ;
        writeln("Interactive objects:"),
        forall(member(O, Objects), (
            (state:object_moved(O) ->
                format("- ~w (already examined)~n", [O])
            ;
                format("- ~w [Use move_object(~w)]~n", [O, O])
            )
        ))
    ),
    
    % Visible puzzle pieces
    findall(Piece-Puzzle, facts:piece_in_room(Room, Piece, Puzzle), Pieces),
    (Pieces = [] ->
        true
    ;
        writeln("\nAvailable puzzle pieces:"),
        forall(member(Piece-Puzzle, Pieces), 
               (state:has_piece(Puzzle, Piece) -> 
                    format("- ~w (of puzzle ~w) [Already collected]~n", [Piece, Puzzle])
               ;
                    format("- ~w (of puzzle ~w) [Use pick_piece(~w)]~n", [Piece, Puzzle, Piece]))
        )
    ),
    
    % Keys
    findall(K, facts:key_in_room(Room, K), Keys),
    state:inventory(Inv),
    (Keys = [] ->
        true
    ;
        writeln("\nAvailable keys:"),
        forall(member(K, Keys), 
               (member(K, Inv) ->
                    format("- ~w [Already collected]~n", [K])
               ;
                    format("- ~w [Use pick_key(~w)]~n", [K, K]))
        )
    ),
    
    % Doors
    findall(To, facts:door(Room, To, _), Doors),
    (Doors = [] ->
        writeln("\nNo exits.")
    ;
        writeln("\nAvailable exits:"),
        forall(member(D, Doors), (
            (state:door_state(Room, D, unlocked) ->
                format("- Room ~w [Use move_player(~w)]~n", [D, D])
            ;   % If locked, show requirements
                facts:door_requirements(Room, D, Reqs),
                format("- Room ~w (LOCKED) - Requirements: ~w~n", [D, Reqs])
            )
        ))
    ).

% Show inventory
inventory :-
    state:inventory(Items),
    writeln("Your inventory:"),
    (Items = [] ->
        writeln("  Empty")
    ;
        forall(member(Item, Items), format("- ~w~n", [Item]))
    ),
    
    % Show collected puzzle pieces
    writeln("\nCollected puzzle pieces:"),
    findall(Puzzle, facts:puzzle(Puzzle), Puzzles),
    (Puzzles = [] ->
        writeln("  None")
    ;
        forall(member(P, Puzzles), (
            format("Puzzle ~w: ", [P]),
            findall(Piece, state:has_piece(P, Piece), Pieces),
            format("~w~n", [Pieces])
        ))
    ),
    
    % Show solved puzzles
    writeln("\nSolved puzzles:"),
    findall(P, state:puzzle_solved(P), SolvedPuzzles),
    (SolvedPuzzles = [] ->
        writeln("  None")
    ;
        forall(member(SP, SolvedPuzzles), format("- ~w~n", [SP]))
    ).

% Check puzzle status
puzzle_status(Puzzle) :-
    facts:puzzle(Puzzle),
    (state:puzzle_solved(Puzzle) ->
        format("Puzzle ~w is already solved.~n", [Puzzle])
    ;
        findall(Piece, facts:piece(Puzzle, Piece), AllPieces),
        findall(Piece, state:has_piece(Puzzle, Piece), CollectedPieces),
        format("Puzzle ~w status:~n", [Puzzle]),
        format("- Total pieces needed: ~w~n", [AllPieces]),
        format("- Pieces collected: ~w~n", [CollectedPieces]),
        facts:puzzle_room(Puzzle, Room),
        format("- Must be solved in room: ~w~n", [Room])
    ).

% Alias for init_game
init_game :-
    rules:initialize_game.

% Entry point
:- initialization((
    writeln("Welcome to the Prolog Escape Room!"),
    writeln("Type 'help.' to see available commands."),
    init_game
)).
