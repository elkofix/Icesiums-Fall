from map import room_coords

def manhattan_heuristic(current, goal):
    x1, y1 = room_coords.get(current, (0, 0))
    x2, y2 = room_coords.get(goal, (0, 0))
    return abs(x1 - x2) + abs(y1 - y2)

def has_key(inventory, key_needed):
    return key_needed in inventory
