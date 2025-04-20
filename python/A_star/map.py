rooms = ["A", "B", "C", "D", "E", "F", "G", "H", "I"]

doors = {
    "A": {"B": "locked(key1)", "F": "locked(key1)", "G": "locked(key2)"},
    "B": {"A": "locked(key1)", "C": "locked(key2)"},
    "C": {"B": "locked(key2)", "D": "unlocked"},
    "D": {"C": "unlocked", "E": "unlocked"},
    "E": {"D": "unlocked", "H": "unlocked", "I": "unlocked"},
    "F": {"A": "locked(key1)", "I": "locked(key1)"},
    "G": {"A": "locked(key2)", "H": "unlocked"},
    "H": {"E": "unlocked", "G": "unlocked"},
    "I": {"F": "locked(key1)", "E": "unlocked"}
}

room_costs = {
    ("A", "B"): 1, ("B", "A"): 1,
    ("B", "C"): 1, ("C", "B"): 1,
    ("C", "D"): 1, ("D", "C"): 1,
    ("D", "E"): 1, ("E", "D"): 1,
    ("E", "H"): 1, ("H", "E"): 1,
    ("A", "F"): 5, ("F", "A"): 5, 
    ("A", "G"): 1, ("G", "A"): 1,
    ("G", "H"): 2, ("H", "G"): 2,
    ("F", "I"): 5, ("I", "F"): 5,
    #("I", "E"): 1, ("E", "I"): 1,
    ("E", "I"):1, ("I", "E"): 1,

}

keys_in_rooms = {
    "A": ["key1"],
    "A": ["key2"],
    "B": ["key2"],
    "C": ["key1"],
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
    "E": (4, 0),
    "H": (4, 1),
    "F": (-1, 0),
    "G": (0, 1),
    "I": (4, -1),
}
