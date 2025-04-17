
def manhattan_distance(current, goal, room_order=None):
    """
    Heurística de distancia de Manhattan simplificada para habitaciones lineales.
    Asume un orden A=1, B=2, C=3, D=4, etc.
    """
    if not room_order:
        room_order = {'A': 1, 'B': 2, 'C': 3, 'D': 4}
    return abs(room_order[current] - room_order[goal])

def combined_heuristic(current, goal, graph, collected_keys):
    """
    Heurística combinada que considera:
    - Distancia a la salida
    - Puertas bloqueadas en el camino
    """
    base_distance = manhattan_distance(current, goal)
    
    # Penalización por puertas bloqueadas
    locked_penalty = 0
    if current == 'A' and 'key_B' not in collected_keys:
        locked_penalty += 2
    if current in ['A', 'B'] and 'key_C' not in collected_keys:
        locked_penalty += 2
    if current in ['A', 'B', 'C'] and 'key_D' not in collected_keys:
        locked_penalty += 2
    
    return base_distance + locked_penalty