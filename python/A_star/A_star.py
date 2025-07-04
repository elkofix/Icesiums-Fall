import heapq
from .map import doors, keys_in_rooms, trap_room, trap_limit, inventory_limit, max_moves, room_costs
from .heuristic import manhattan_heuristic, has_key

def a_star_escape(start, goal, game_data):
    doors = game_data["doors"]
    keys_in_rooms = game_data["keys_in_rooms"]
    room_coords = game_data["room_coords"]
    trap_room = game_data["trap_room"]
    trap_limit = game_data["trap_limit"]
    inventory_limit = game_data["inventory_limit"]
    max_moves = game_data["max_moves"]
    room_costs = game_data["room_costs"]
    open_list = []
    heapq.heappush(open_list, (0, 0, start, [], [], {trap_room: 0}))  # f, g, current, path, inventory, visits

    while open_list:
        f, g, current, path, inventory, visits = heapq.heappop(open_list)

        if current == goal:
            return path + [current], g

        new_path = path + [current]
        new_inventory = inventory.copy()
        new_visits = visits.copy()

        # Recolectar llaves
        for key in keys_in_rooms.get(current, []):
            if key not in new_inventory and len(new_inventory) < inventory_limit:
                new_inventory.append(key)

        # Trampa
        if current == trap_room:
            new_visits[trap_room] += 1
            if new_visits[trap_room] > trap_limit:
                continue

        # Vecinos
        for neighbor, door in doors.get(current, {}).items():
            if door == "unlocked":
                passable = True
            elif door.startswith("locked"):
                key_needed = door.split("(")[1].split(")")[0]
                passable = has_key(new_inventory, key_needed)
            else:
                passable = False

            if passable:
                move_cost = room_costs.get((current, neighbor), 1) # Default cost is 1
                g_new = g + move_cost
                if g_new > max_moves:
                    continue
                h = manhattan_heuristic(neighbor, goal)
                f_new = g_new + h
                heapq.heappush(
                    open_list,
                    (f_new, g_new, neighbor, new_path, new_inventory.copy(), new_visits.copy())
                )

    return None, 0
