import heapq

# Mapa
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


def print_result(name, path):
    if path:
        print(f"{name}: Ruta encontrada -> {' -> '.join(path)}")
    else:
        print(f"{name}: No se encontró ruta válida")


def manhattan_heuristic(current, goal):
    coords = {"A": (0, 0), "B": (1, 0), "C": (2, 0), "D": (3, 0)}
    x1, y1 = coords[current]
    x2, y2 = coords[goal]
    return abs(x1 - x2) + abs(y1 - y2)


def has_key(inventory, key_needed):
    return key_needed in inventory


def a_star_escape(start, goal):
    open_list = []
    heapq.heappush(open_list, (0, 0, start, [], [], {trap_room: 0}))  # f, g, current, path, inventory, visits

    while open_list:
        f, g, current, path, inventory, visits = heapq.heappop(open_list)

        if current == goal:
            return path + [current]

        new_path = path + [current]
        new_inventory = inventory.copy()
        new_visits = visits.copy()

        # Recolectar llaves
        for key in keys_in_rooms.get(current, []):
            if key not in new_inventory and len(new_inventory) < inventory_limit:
                new_inventory.append(key)

        # Contar visitas a la sala trampa
        if current == trap_room:
            new_visits[trap_room] += 1
            if new_visits[trap_room] > trap_limit:
                continue

        # Expandir vecinos
        for neighbor, door in doors.get(current, {}).items():
            if door == "unlocked":
                passable = True
            elif door.startswith("locked"):
                key_needed = door.split("(")[1].split(")")[0]
                passable = has_key(new_inventory, key_needed)
            else:
                passable = False

            if passable:
                g_new = g + 1
                if g_new > max_moves:
                    continue
                h = manhattan_heuristic(neighbor, goal)
                f_new = g_new + h
                heapq.heappush(
                    open_list,
                    (f_new, g_new, neighbor, new_path, new_inventory.copy(), new_visits.copy())
                )

    return None


# Ejecutar prueba
print("=== Búsqueda A* con llaves y trampa ===")
result = a_star_escape("A", "D")
print_result("A*", result)
