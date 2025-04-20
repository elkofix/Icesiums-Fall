rooms = ["A", "B", "C", "D"]

doors = {
    "A": {"B": "locked(key1)"},
    "B": {"A": "unlocked", "C": "locked(key2)"},
    "C": {"B": "unlocked", "D": "unlocked"},
}

keys_in_rooms = {
    "A": ["key1"],
    "B": ["key2"],
}

trap_room = "B"
trap_limit = 3
inventory_limit = 2
max_moves = 30

room_coords = {
    "A": (0, 0),
    "B": (1, 0),
    "C": (2, 0),
    "D": (3, 0),
}
