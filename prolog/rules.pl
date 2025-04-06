:- module(rules, [
    can_move/2
]).

:- use_module(state).

can_move(From, To) :-
    ( state:door_state(From, To, unlocked)
    ; state:door_state(To, From, unlocked)
    ).
