import heapq
from integration.pyswip_bridge import PrologBridge

class HybridSolver:
    """
    Implementación unificada del solver híbrido con soporte para Prolog y lógica de A*.
    """
    def __init__(self, graph, heuristic):
        self.graph = graph
        self.heuristic = heuristic
        self.prolog = PrologBridge()
        self.visited = set()  # Para evitar estados repetidos
        self.verbose = False

    def solve(self, start, goal, verbose=False):
        """Versión híbrida que consulta restricciones a Prolog"""
        self.verbose = verbose
        open_set = []

        # Estado inicial validado por Prolog
        
        if not self.prolog.validate_state(start, [], 0, 0):
            if verbose:
                print("Estado inicial inválido según Prolog")
            return None, None

        initial_state = State(start, [], 0, [start], 0)
        initial_f = self.heuristic(start, goal)
        heapq.heappush(open_set, (initial_f, initial_state))

        while open_set:
            current_f, current_state = heapq.heappop(open_set)

            if verbose:
                self.print_state_info(current_state)

            # Verificar si llegamos al objetivo
            if current_state.room == goal:
                if self.prolog.check_solution(current_state.path):
                    if verbose:
                        print("\n¡Solución validada por Prolog!")
                    return current_state.path, current_state.moves
                continue

            # Verificar restricciones de estado
            if not self._is_state_valid(current_state):
                continue

            # Recolectar llave si existe en esta habitación
            new_keys = set(current_state.keys)
            room = self.graph.get_room(current_state.room)
            if room.has_key and room.has_key not in new_keys:
                new_keys.add(room.has_key)
                if verbose:
                    print(f"¡Recogida llave {room.has_key} en habitación {current_state.room}!")

            # Obtener movimientos validados por Prolog
            prolog_moves = self.prolog.get_possible_moves(
                current_state.room,
                new_keys
            )
            room = self.graph.get_room(current_state.room)
            print(f"Conexiones desde {current_state.room}: {[conn[0] for conn in room.connections]}")

            # Explorar solo movimientos válidos
            for neighbor, cost, is_locked, key_needed, puzzle_needed in self._get_valid_moves(current_state, prolog_moves, new_keys):
                print("Entró al for!")
                if is_locked and key_needed not in new_keys:
                    if self.verbose:
                        print(f"Movimiento bloqueado: {current_state.room} -> {neighbor} (falta llave {key_needed})")
                    continue

                if puzzle_needed:  # Si se requiere resolver un puzzle, puedes manejarlo aquí
                    if self.verbose:
                        print(f"Movimiento requiere resolver puzzle: {current_state.room} -> {neighbor} (puzzle {puzzle_needed})")
                    # Aquí podrías agregar lógica para manejar puzzles si es necesario
                    continue

                # Crear nuevo estado
                new_b_visits = current_state.b_visits
                if neighbor == self.graph.trap_room:
                    new_b_visits += 1

                new_state = State(
                    neighbor,
                    new_keys,
                    current_state.moves + cost,
                    current_state.path + [neighbor],
                    new_b_visits
                )

                # Calcular f_score y agregar a la cola
                if self._is_state_valid(new_state):
                    if self.verbose:
                        print(f"Agregando a open_set: {new_state.room}, llaves={list(new_state.keys)}, movimientos={new_state.moves}")
                    f_score = (current_state.moves + cost) + self.heuristic(neighbor, goal)
                    heapq.heappush(open_set, (f_score, new_state))

        return None, None  # No se encontró solución

    def _is_state_valid(self, state):
        """Verifica si el estado cumple con todas las restricciones"""
        # Verificar límites de movimientos
        if state.moves >= self.graph.max_moves:
            if self.verbose:
                print(f"Límite de movimientos alcanzado: {state.moves}")
            return False

        # Verificar límite de visitas a la habitación B
        if state.b_visits >= self.graph.trap_threshold:
            if self.verbose:
                print(f"Límite de visitas a B alcanzado: {state.b_visits}")
            return False

        # Verificar con Prolog
        if self.verbose:
            print(f"Validando estado con Prolog: {state.room}, llaves = {list(state.keys)}, moves = {state.moves}, b_visits = {state.b_visits}")
        if not self.prolog.validate_state(state.room, list(state.keys), state.moves, state.b_visits):
            if self.verbose:
                print(f"Estado inválido según restricciones de Prolog {state.room}, llaves {state.keys}, movimientos {state.moves}, visitas a B {state.b_visits}")
            return False

        # Verificar si ya visitamos este estado
        state_key = (state.room, frozenset(state.keys), state.b_visits)
        if state_key in self.visited:
            return False
        self.visited.add(state_key)

        return True

    def _get_valid_moves(self, current_state, prolog_moves, current_keys):
        """Filtra movimientos basado en validación de Prolog y disponibilidad de llaves"""
        valid_moves = []
        room = self.graph.get_room(current_state.room)

        for neighbor, cost, is_locked, key_needed, puzzle_needed in room.connections:
            if neighbor in prolog_moves:
                if self.verbose:
                    print(f"Evaluando conexión: {current_state.room} -> {neighbor} (costo: {cost}, bloqueada: {is_locked}, llave necesaria: {key_needed}, puzzle necesario: {puzzle_needed})")
                if is_locked and key_needed not in current_keys:
                    if self.verbose:
                        print(f"Movimiento bloqueado: {current_state.room} -> {neighbor} (falta llave {key_needed})")
                    continue
                valid_moves.append((neighbor, cost, is_locked, key_needed, puzzle_needed))
        return valid_moves

    def print_state_info(self, state):
        """Muestra información del estado actual, compatible con la nueva estructura de conexiones"""
        print(f"\n--- Estado actual: {state.room} ---")
        print(f"Camino recorrido: {' -> '.join(state.path)}")
        print(f"Llaves recolectadas: {state.keys or 'Ninguna'}")
        print(f"Movimientos realizados: {state.moves}/{self.graph.max_moves}")
        print(f"Visitas a la habitación B: {state.b_visits}/{self.graph.trap_threshold}")

        print("Evaluando conexiones disponibles:")
        room = self.graph.get_room(state.room)

        for connection in room.connections:
            neighbor = connection[0]
            is_locked = connection[2]
            key_needed = connection[3]
            puzzle_needed = connection[4]

            status = ""
            if is_locked:
                requirements = []
                if key_needed:
                    requirements.append(f"llave {key_needed}")
                if puzzle_needed:
                    requirements.append(f"puzzle {puzzle_needed}")
                status = f" (BLOQUEADA - necesita {', '.join(requirements)})" if requirements else " (BLOQUEADA)"

            print(f"  - {neighbor}{status}")


class State:
    """
    Representa un estado en el espacio de búsqueda.
    """
    def __init__(self, room, keys, moves, path, b_visits):
        self.room = room
        self.keys = set(keys)
        self.moves = moves
        self.path = path
        self.b_visits = b_visits

    def __lt__(self, other):
        """Comparación para la cola de prioridad"""
        return self.moves < other.moves