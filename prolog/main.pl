% main.pl
:- use_module(state).
:- use_module(facts).
:- use_module(rules).
:- use_module(constraints).
% :- use_module(search).
:- use_module(library(clpfd)).

% Help command
help :-
    writeln("Available commands:"),
    writeln("- look. : Look around the room"),
    writeln("- move_player(Room). : Move to another room"),
    writeln("- pick_key(Key). : Pick up a key"),
    writeln("- drop_key(Key). : Drop a key in the current room"),
    writeln("- pick_piece(Piece). : Pick up a puzzle piece"),
    writeln("- drop_piece(Piece). : Drop a puzzle piece in the current room"),
    writeln("- move_object(Object). : Examine an object"),
    writeln("- inventory. : View your inventory"),
    writeln("- puzzle_status(Puzzle). : Check status of a puzzle"),
    writeln("- solve_puzzle(Puzzle). : Try to solve a puzzle (consumes pieces)"),
    writeln("- unlock_door(From, To). : Unlock a door (consumes keys)"),
    writeln("- init_game. : Reset the game"),
    writeln("- game_stats. : View game statistics and constraints"),
    writeln("- find_escape_plan. : Find the solution to escape to room D"),
    writeln("- help. : Show this help").

% Look around the current room
look :-
    state:player_location(Room),
    format("You are in room ~w.~n~n", [Room]),
    
    % Display turn count for this room
    constraints:turns_in_room(Room, Turns),
    format("Turns spent in this room: ~w~n", [Turns]),
    
    % Check for trap in this room
    (constraints:trap(Room, turns(Limit)) ->
        RemainingTurns #= Limit - Turns,
        format("WARNING: This room has a trap that activates after ~w turns! (~w turns remaining)~n", 
              [Limit, RemainingTurns])
    ;
        true
    ),
    
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
    
    % Visible puzzle pieces (both original and dropped)
    findall(Piece-Puzzle, facts:piece_in_room(Room, Piece, Puzzle), OriginalPieces),
    findall(Piece-Puzzle, state:piece_dropped(Piece, Puzzle, Room), DroppedPieces),
    append(OriginalPieces, DroppedPieces, AllPieces),
    (AllPieces = [] ->
        true
    ;
        writeln("\nAvailable puzzle pieces:"),
        forall(member(Piece-Puzzle, AllPieces), 
               (state:has_piece(Puzzle, Piece) -> 
                    format("- ~w (of puzzle ~w) [Already collected]~n", [Piece, Puzzle])
               ;
                    format("- ~w (of puzzle ~w) [Use pick_piece(~w)]~n", [Piece, Puzzle, Piece]))
        )
    ),
    
    % Keys (both original and dropped)
    findall(K, facts:key_in_room(Room, K), OriginalKeys),
    findall(K, state:key_dropped(K, Room), DroppedKeys),
    append(OriginalKeys, DroppedKeys, AllKeys),
    state:inventory(Inv),
    (AllKeys = [] ->
        true
    ;
        writeln("\nAvailable keys:"),
        forall(member(K, AllKeys), 
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
    ),
    
    % Show move count
    constraints:move_count(Moves),
    constraints:max_moves(MaxMoves),
    RemainingMoves #= MaxMoves - Moves,
    format("\nMoves used: ~w/~w (~w remaining)~n", [Moves, MaxMoves, RemainingMoves]).

% Show inventory
inventory :-
    state:inventory(Items),
    writeln("Your inventory:"),
    (Items = [] ->
        writeln("  Empty")
    ;
        forall(member(Item, Items), format("- ~w~n", [Item]))
    ),
    
    % Show inventory limits
    constraints:can_carry(key, KeyLimit),
    constraints:count_items_of_type(key, KeyCount),
    format("Keys: ~w/~w~n", [KeyCount, KeyLimit]),
    
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
    
    % Show piece inventory limit
    constraints:can_carry(piece, PieceLimit),
    constraints:count_items_of_type(piece, PieceCount),
    format("Puzzle pieces: ~w/~w~n", [PieceCount, PieceLimit]),
    
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

% Game stats
game_stats :-
    rules:game_stats.

% Alias for init_game
init_game :-
    rules:initialize_game.

% Entry point
:- initialization((
    writeln("Welcome to the Prolog Escape Room!"),
    writeln("Type 'help.' to see available commands."),
    init_game
)).