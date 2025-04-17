% facts.pl
:- module(facts, [
    room/1, door/3, key_in_room/2,
    object_in_room/2, hides_piece/3, piece/2,
    piece_location/3, puzzle_room/2,
    door_requirements/3, puzzle/1,
    piece_in_room/3
]).

key(key1).
key(key2).


% Rooms
room(a). room(b). room(c). room(d).

% Doors (from, to, initial state)
door(a, b, locked).
door(b, a, locked).
door(b, c, locked).
door(c, b, locked).
door(c, d, locked).
door(d, c, locked).

% Door requirements - Each door can have multiple requirements
% Format: door_requirements(From, To, [list of requirements])
% Requirements can be: has_key(KeyName) or puzzle_solved(PuzzleName)
door_requirements(a, b, [has_key(key1)]).
door_requirements(b, a, [has_key(key1)]).
door_requirements(b, c, [puzzle_solved(puzzle1)]).
door_requirements(c, b, [puzzle_solved(puzzle1)]).
door_requirements(c, d, [puzzle_solved(puzzle2), has_key(key2)]).
door_requirements(d, c, [puzzle_solved(puzzle2), has_key(key2)]).

% Keys in rooms
key_in_room(a, key1).
key_in_room(b, key2).

% Visible puzzle pieces in rooms
piece_in_room(a, p3, puzzle1).
piece_in_room(c, p4, puzzle2).

% Objects in rooms
object_in_room(a, box).
object_in_room(a, painting).
object_in_room(b, rug).
object_in_room(b, bookshelf).
object_in_room(c, cabinet).

% Puzzles
puzzle(puzzle1).
puzzle(puzzle2).

% Pieces for each puzzle
piece(puzzle1, p1).
piece(puzzle1, p2).
piece(puzzle1, p3).
piece(puzzle2, p4).
piece(puzzle2, p5).
piece(puzzle2, p6).

% Where pieces are located (for tracking)
piece_location(puzzle1, p1, b).
piece_location(puzzle1, p2, b).
piece_location(puzzle1, p3, a).
piece_location(puzzle2, p4, c).
piece_location(puzzle2, p5, a).
piece_location(puzzle2, p6, a).

% Which objects hide pieces
hides_piece(rug, puzzle1, p1).
hides_piece(bookshelf, puzzle1, p2).
hides_piece(box, puzzle2, p5).
hides_piece(painting, puzzle2, p6).

% Which room each puzzle must be solved in
puzzle_room(puzzle1, b).
puzzle_room(puzzle2, c).

% Guard position
:- dynamic guard_position/1.
guard_position(b).  % El guardia comienza en la habitación B