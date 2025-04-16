import heapq

class State:
    """
    Representa un estado en el espacio de búsqueda.
    
    Atributos:
        room (str): Habitación actual
        keys (set): Conjunto de llaves recolectadas
        moves (int): Número de movimientos realizados
        path (list): Camino seguido hasta ahora
        b_visits (int): Veces que ha visitado la habitación B
    """
    def __init__(self, room, keys, moves, path, b_visits, solved_puzzles=None):
        self.room = room
        self.keys = set(keys)
        self.moves = moves
        self.path = path
        self.b_visits = b_visits
        self.solved_puzzles = set(solved_puzzles) if solved_puzzles else set()
    
    def __lt__(self, other):
        """Comparación para la cola de prioridad"""
        return self.moves < other.moves

class AStarSolver:
    """
    Implementación del algoritmo A* para resolver el escape room.
    
    Args:
        graph (EscapeRoomGraph): Grafo del escape room
        heuristic (function): Función heurística para estimar costos
    """
    def __init__(self, graph, heuristic):
        self.graph = graph
        self.heuristic = heuristic
        self.verbose = False
    
    def solve(self, start, goal, verbose=False):
        self.verbose = verbose
        open_set = []
        visited = set()
        
        initial_state = State(start, [], 0, [start], 0)
        heapq.heappush(open_set, (self.heuristic(start, goal), initial_state))
        
        while open_set:
            current_f, current_state = heapq.heappop(open_set)
            
            if verbose:
                self._print_state_info(current_state)
            
            if current_state.room == goal:
                if verbose:
                    print("\n¡Solución encontrada!")
                return current_state.path, current_state.moves
                
            if not self._is_state_valid(current_state):
                continue
                
            new_keys = set(current_state.keys)
            room = self.graph.get_room(current_state.room)
            if room.has_key and room.has_key not in new_keys:
                new_keys.add(room.has_key)
                if verbose:
                    print(f"¡Recogida llave {room.has_key} en habitación {current_state.room}!")
            
            # Obtener movimientos válidos (ahora incluye puzzles)
            for neighbor, cost, is_locked, key_needed, puzzle_needed in self._get_valid_moves(
                current_state.room, new_keys, current_state.b_visits
            ):
                new_b_visits = current_state.b_visits
                if neighbor == self.graph.trap_room:
                    new_b_visits += 1
                    
                next_state = State(
                    neighbor,
                    new_keys,
                    current_state.moves + cost,
                    current_state.path + [neighbor],
                    new_b_visits
                )
                
                if self._should_visit_state(next_state, visited):
                    f_score = next_state.moves + self.heuristic(neighbor, goal)
                    heapq.heappush(open_set, (f_score, next_state))
                    visited.add((next_state.room, frozenset(next_state.keys), next_state.b_visits))
        
        return None, None
    
    def _is_state_valid(self, state):
        """Verifica si el estado cumple con las restricciones"""
        if state.moves >= self.graph.max_moves:
            if self.verbose:
                print(f"Límite de movimientos alcanzado: {state.moves}")
            return False
            
        if state.b_visits >= self.graph.trap_threshold:
            if self.verbose:
                print(f"Límite de visitas a B alcanzado: {state.b_visits}")
            return False
            
        return True
    
    def _process_current_state(self, state):
        """Procesa el estado actual (recolecta llaves)"""
        new_keys = set(state.keys)
        room = self.graph.get_room(state.room)
        
        if room.has_key and room.has_key not in new_keys:
            new_keys.add(room.has_key)
            if self.verbose:
                print(f"¡Llave recolectada: {room.has_key}!")
        
        return new_keys, state.moves
    
    def _get_valid_moves(self, current_room, keys, b_visits):
        """Obtiene movimientos válidos considerando llaves y puzzles"""
        valid_moves = []
        room = self.graph.get_room(current_room)
        
        for neighbor, cost, is_locked, key_needed, puzzle_needed in room.connections:
            if not is_locked:
                valid_moves.append((neighbor, cost, is_locked, key_needed, puzzle_needed))
                continue
                
            # Verificar requisitos para puertas bloqueadas
            key_ok = (key_needed is None) or (key_needed in keys)
            puzzle_ok = (puzzle_needed is None) or self._is_puzzle_solved(puzzle_needed)
            
            if key_ok and puzzle_ok:
                valid_moves.append((neighbor, cost, is_locked, key_needed, puzzle_needed))
            elif self.verbose:
                reqs = []
                if not key_ok:
                    reqs.append(f"falta llave {key_needed}")
                if not puzzle_ok:
                    reqs.append(f"puzzle {puzzle_needed} no resuelto")
                print(f"Puerta {current_room}-{neighbor} bloqueada ({', '.join(reqs)})")
        
        return valid_moves

    def _is_puzzle_solved(self, puzzle):
        """Consulta a Prolog si un puzzle está resuelto"""
        try:
            query = f"puzzle_solved({puzzle})"
            return bool(list(self.prolog.query(query)))
        except Exception as e:
            print(f"Error al verificar puzzle {puzzle}: {str(e)[:100]}...")
            return False
    
    def _create_next_state(self, current_state, neighbor, cost, is_locked, key_needed, keys):
        """Crea un nuevo estado para el movimiento"""
        new_b_visits = current_state.b_visits
        if neighbor == self.graph.trap_room:
            new_b_visits += 1
            
        return State(
            neighbor,
            keys,
            current_state.moves + cost,
            current_state.path + [neighbor],
            new_b_visits
        )
    
    def _should_visit_state(self, state, visited):
        """Determina si se debe visitar el estado"""
        state_key = (state.room, frozenset(state.keys), state.b_visits)
        return state_key not in visited
    
    def _calculate_f_score(self, state, goal):
        """Calcula f(n) = g(n) + h(n)"""
        return state.moves + self.heuristic(state.room, goal)
    
    def _print_state_info(self, state):
        """Muestra información del estado actual"""
        print(f"\n--- Estado actual: {state.room} ---")
        print(f"Camino: {' -> '.join(state.path)}")
        print(f"Llaves: {state.keys or 'Ninguna'}")
        print(f"Movimientos: {state.moves}/{self.graph.max_moves}")
        print(f"Visitas a B: {state.b_visits}/{self.graph.trap_threshold}")