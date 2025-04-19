from .heuristic import manhattan_distance
import heapq
from .state import State

def a_star_escape(graph, start_room, goal_room):
    max_moves = graph.max_moves
    max_b_visits = graph.trap_threshold
    max_keys = graph.inventory_limits['key']
    max_pieces = graph.inventory_limits['piece']

    initial_state = State(
        room=start_room,
        keys=[],
        pieces=[],
        moves=0,
        path=[start_room],
        visits={}
    )
    
    heap = [(0, initial_state)]  # (f = g + h, state)
    visited = set()

    # ----------- INICIO del algoritmo A* -----------
    while heap:
        _, current = heapq.heappop(heap)

        # Meta alcanzada
        if current.room == goal_room:
            return current.path, current.moves

        # Identificador del estado para evitar ciclos
        state_id = (
            current.room,
            frozenset(current.keys),
            frozenset(current.pieces),
            tuple(sorted(current.visits.items()))
        )
        if state_id in visited:
            continue
        visited.add(state_id)

        # Obtener la sala actual
        room_obj = graph.get_room(current.room)
        keys = list(current.keys)
        pieces = list(current.pieces)

        # Recolectar llaves si no se excede el inventario
        if room_obj.has_keys and room_obj.has_keys not in keys:
            if len(keys) < max_keys:
                keys.append(room_obj.has_keys)

        # Recolectar piezas si no se excede el inventario
        if room_obj.has_pieces and room_obj.has_pieces not in pieces:
            if len(pieces) < max_pieces:
                pieces.append(room_obj.has_pieces)

        # Control de visitas a la sala trampa
        visits = dict(current.visits)
        visits[current.room] = visits.get(current.room, 0) + 1
        if current.room == graph.trap_room and visits[current.room] > max_b_visits:
            continue

        # Límite de movimientos
        if current.moves >= max_moves:
            continue

        # Explorar conexiones disponibles
        for neighbor_name, cost, is_locked, key_required, puzzle_required in room_obj.connections:
            # Verificación de llaves y puzzles requeridos
            if is_locked:
                if key_required and key_required not in keys:
                    continue
                if puzzle_required and puzzle_required not in pieces:
                    continue

            new_state = State(
                room=neighbor_name,
                keys=list(keys),
                pieces=list(pieces),
                moves=current.moves + cost,
                path=current.path + [neighbor_name],
                visits=dict(visits)
            )

            # Heurística A*
            h = manhattan_distance(graph, neighbor_name, goal_room)
            f = new_state.moves + h
            heapq.heappush(heap, (f, new_state))

    # ------------- FIN del algoritmo A* -------------
    return None, float('inf')
