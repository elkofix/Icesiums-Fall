:- module(facts, [
    room/1, door/3, object_in_room/2, hides_item/2,
    item_in_room/2, visible_item/2, piece/1, puzzle/2,
    door_requirement/3, player_location/1, player_inventory/1,
    puzzle_pieces_required/2, puzzle_resolved/1,
    hanoi_state/1, hanoi_goal/1
]).

room(a).
room(b).
room(c).

door(a, b, locked).
door(b, c, locked).

object_in_room(a, rug).
object_in_room(b, box).
object_in_room(b, bookshelf).
object_in_room(c, table).

hides_item(rug, piece(p1)).
hides_item(box, piece(p2)).
hides_item(bookshelf, piece(p3)).

item_in_room(b, piece(p4)). % pieza visible
visible_item(b, piece(p4)).

item_in_room(a, key1).
hides_item(rug, key1).

piece(p1).
piece(p2).
piece(p3).
piece(p4).

puzzle(b, puzzle1).  % rompecabezas de piezas
puzzle(c, hanoi1).   % Torre de Hanoi

puzzle_pieces_required(puzzle1, [p1, p2, p3, p4]).
puzzle_resolved(puzzle1) :- 
    player_inventory(Inventory),
    puzzle_pieces_required(puzzle1, Pieces),
    subset(Pieces, Inventory).

puzzle_resolved(hanoi1) :-
    hanoi_state(State),
    hanoi_goal(State).

door_requirement(a, b, [key(key1)]).
door_requirement(b, c, [puzzle(puzzle1), puzzle(hanoi1)]).

:- dynamic player_location/1.
:- dynamic player_inventory/1.
:- dynamic hanoi_state/1.

player_location(a).
player_inventory([]).

hanoi_state([[3,2,1],[],[]]).
hanoi_goal([[],[],[3,2,1]]).
