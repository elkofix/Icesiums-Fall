class State:
    def __init__(self, room, keys, pieces, moves, path, visits):
        self.room = room
        self.keys = list(keys)
        self.pieces = list(pieces)
        self.moves = moves
        self.path = list(path)
        self.visits = dict(visits)

    def __eq__(self, other):
        return (self.room == other.room and
                set(self.keys) == set(other.keys) and
                set(self.pieces) == set(other.pieces) and
                self.moves == other.moves and
                self.visits == other.visits)

    def __hash__(self):
        return hash((self.room, tuple(sorted(self.keys)), tuple(sorted(self.pieces)), self.moves, frozenset(self.visits.items())))

    def __lt__(self, other):
        return (self.room, self.moves) < (other.room, other.moves)
