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

        initial_state = State(start, [], 0, [start], 0, [])
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

            # Recolectar piezas (visibles y ocultas)
            new_pieces = list(current_state.pieces)
            found_new_piece = False

            # 1. Recoger piezas visibles
            for piece, puzzle in self.prolog.get_visible_pieces(current_state.room):
                if not any(p == piece and puz == puzzle for (puz, p) in new_pieces):
                    if self.prolog.pick_piece(piece):
                        new_pieces.append((puzzle, piece))
                        found_new_piece = True
                        if verbose:
                            print(f"🧩 Recogida pieza visible {piece} del puzzle {puzzle} en habitación {current_state.room}!")

            # 2. Interactuar con objetos para encontrar piezas ocultas
            for obj, piece, puzzle in self.prolog.get_hidden_pieces(current_state.room):
                if not any(p == piece and puz == puzzle for (puz, p) in new_pieces):
                    if self.prolog.try_move_object(obj):
                        if self.prolog.pick_piece(piece):
                            new_pieces.append((puzzle, piece))
                            found_new_piece = True
                            if verbose:
                                print(f"🧱 Movido objeto {obj} y se encontró pieza oculta: {piece} del puzzle {puzzle}")

            # Si encontramos nuevas piezas, actualizamos el estado
            if found_new_piece:
                current_state.pieces = new_pieces
                if verbose:
                    print(f"Piezas recolectadas actualizadas: {new_pieces}")

            # Intentar resolver puzzles si tenemos todas las piezas necesarias
            self._try_solve_puzzles(current_state)

            # Obtener movimientos validados por Prolog
            prolog_moves = self.prolog.get_possible_moves(
                current_state.room,
                new_keys
            )
            
            if verbose:
                print(f"Conexiones desde {current_state.room}: {[conn[0] for conn in room.connections]}")

            # Explorar solo movimientos válidos
            for neighbor, cost, is_locked, key_needed, puzzle_needed in self._get_valid_moves(current_state, prolog_moves, new_keys):
                if verbose:
                    print("Evaluando movimiento a:", neighbor)

                if is_locked and key_needed not in new_keys:
                    if verbose:
                        print(f"Movimiento bloqueado: {current_state.room} -> {neighbor} (falta llave {key_needed})")
                    continue

                if puzzle_needed:
                    if verbose:
                        print(f"Movimiento requiere resolver puzzle: {current_state.room} -> {neighbor} (puzzle {puzzle_needed})")

                    # Verificar si el puzzle está resuelto usando el método de PrologBridge
                    if not self.prolog.try_solve_puzzle(puzzle_needed):
                        if verbose:
                            print(f"❌ Puzzle {puzzle_needed} no resuelto, omitiendo movimiento.")
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
                    new_b_visits,
                    new_pieces
                )

                # Calcular f_score y agregar a la cola
                if self._is_state_valid(new_state):
                    if verbose:
                        print(f"Agregando a open_set: {new_state.room}, llaves={list(new_state.keys)}, movimientos={new_state.moves}")
                    f_score = (current_state.moves + cost) + self.heuristic(neighbor, goal)
                    heapq.heappush(open_set, (f_score, new_state))

        return None, None  # No se encontró solución

    def _try_solve_puzzles(self, current_state):
        """Intenta resolver puzzles si tenemos todas las piezas necesarias"""
        # Obtener todos los puzzles del juego
        puzzles = set(puzzle for (puzzle, _) in current_state.pieces)
        
        for puzzle in puzzles:
            # Verificar si ya está resuelto
            if self.prolog.try_solve_puzzle(puzzle):
                if self.verbose:
                    print(f"¡Puzzle {puzzle} resuelto con éxito!")
                # Actualizar lista de piezas (se eliminan al resolver)
                current_state.pieces = [(puz, piece) for (puz, piece) in current_state.pieces if puz != puzzle]

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
        state_key = (state.room, frozenset(state.keys), frozenset((puz, piece) for (puz, piece) in state.pieces), state.b_visits)
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
                valid_moves.append((neighbor, cost, is_locked, key_needed, puzzle_needed))
        return valid_moves

    def print_state_info(self, state):
        """Muestra información del estado actual, compatible con la nueva estructura de conexiones"""
        print(f"\n--- Estado actual: {state.room} ---")
        print(f"Camino recorrido: {' -> '.join(state.path)}")
        print(f"Llaves recolectadas: {state.keys or 'Ninguna'}")
        print(f"Movimientos realizados: {state.moves}/{self.graph.max_moves}")
        print(f"Visitas a la habitación B: {state.b_visits}/{self.graph.trap_threshold}")
        
        # Mostrar piezas recolectadas agrupadas por puzzle
        pieces_by_puzzle = {}
        for puzzle, piece in state.pieces:
            if puzzle not in pieces_by_puzzle:
                pieces_by_puzzle[puzzle] = []
            pieces_by_puzzle[puzzle].append(piece)
        
        print("Piezas de puzzle recolectadas:")
        for puzzle, pieces in pieces_by_puzzle.items():
            print(f"- Puzzle {puzzle}: {pieces}")

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
    def __init__(self, room, keys, moves, path, b_visits, pieces):
        self.room = room
        self.keys = set(keys)
        self.moves = moves
        self.path = path
        self.b_visits = b_visits
        self.pieces = pieces  # Lista de tuplas (puzzle, piece)

    def __lt__(self, other):
        """Comparación para la cola de prioridad"""
        return self.moves < other.moves