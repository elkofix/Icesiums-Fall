:- consult('facts.pl').
:- consult('rules.pl').

run_test :-
    init_player(a),
    take_key,
    unlock_door(a, b),
    move_to(b),
    move_to(c),
    format('Test passed. Reached room c successfully.~n').
