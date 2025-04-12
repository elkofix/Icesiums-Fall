:- module(rules, [
    can_move/2,
    initialize_game/0
]).

:- use_module(state).
:- use_module(facts).

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
    
    % Set initial state
    assertz(state:player_location(a)),
    assertz(state:inventory([])),
    
    % Set up doors based on facts
    forall(facts:door(X, Y, State), assertz(state:door_state(X, Y, State))),
    
    writeln("Game initialized! You are in room A."),(
    writeln("Type 'help.' for available commands.").
)