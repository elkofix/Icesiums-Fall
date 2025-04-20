import json

def load_map_from_json(file_path):
    with open(file_path, 'r') as f:
        data = json.load(f)

    parsed_costs = {}
    for k, v in data.get("room_costs", {}).items():
        room1, room2 = k.strip("()").split(", ")
        parsed_costs[(room1, room2)] = v

    return {
        "doors": data.get("doors", {}),
        "keys_in_rooms": data.get("keys_in_rooms", {}),
        "room_coords": data.get("room_coords", {}),
        "trap_room": data.get("trap_room", ""),
        "trap_limit": data.get("trap_limit", 1),
        "inventory_limit": data.get("inventory_limit", 3),
        "max_moves": data.get("max_moves", 50),
        "room_costs": parsed_costs
    }

