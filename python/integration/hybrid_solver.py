import heapq
from heuristic_search.solver import AStarSolver
from integration.pyswip_bridge import PrologBridge

class HybridSolver(AStarSolver):
    def __init__(self, graph, heuristic):
        super().__init__(graph, heuristic)
        self.prolog = PrologBridge()
        self.visited = set()  # Para evitar estados repetidos
    
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
        initial_f = 0 + self.heuristic(start, goal)
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
            
            # Explorar solo movimientos válidos
            for neighbor, cost, is_locked, key_needed in self._get_valid_moves(current_state, prolog_moves, new_keys):
                print("DEBUG: room.connections =", room.connections)
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
                    f_score = (current_state.moves + cost) + self.heuristic(neighbor, goal)
                    heapq.heappush(open_set, (f_score, new_state))
        
        return None, None  # No se encontró solución
    
    def _get_valid_moves(self, current_state, prolog_moves, current_keys):
        """Filtra movimientos basado en validación de Prolog y disponibilidad de llaves"""
        valid_moves = []
        room = self.graph.get_room(current_state.room)
        
        for neighbor, cost, is_locked, key_needed in room.connections:
            if neighbor in prolog_moves:
                if is_locked:
                    if key_needed in current_keys:
                        valid_moves.append((neighbor, cost, is_locked, key_needed))
                        
                    elif self.verbose:
                        print(f"Puerta {current_state.room}-{neighbor} bloqueada (falta {key_needed})")
                else:
                    valid_moves.append((neighbor, cost, is_locked, key_needed))
        
        return valid_moves
    
    def _is_state_valid(self, state):
        """Verifica si el estado cumple con todas las restricciones"""
        # Verificar límites de Python
        if state.moves >= self.graph.max_moves:
            if self.verbose:
                print(f"Límite de movimientos alcanzado: {state.moves}")
            return False
            
        if state.b_visits >= self.graph.trap_threshold:
            if self.verbose:
                print(f"Límite de visitas a B alcanzado: {state.b_visits}")
            return False
        
        # Verificar con Prolog
        if not self.prolog.validate_state(state.room, list(state.keys), state.moves, state.b_visits):
            if self.verbose:
                print("Estado inválido según restricciones de Prolog")
            return False
        
        # Verificar si ya visitamos este estado
        state_key = (state.room, frozenset(state.keys), state.b_visits)
        if state_key in self.visited:
            return False
        self.visited.add(state_key)
        
        return True
    
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

# Definición de State (debe coincidir con la de solver.py)
class State:
    def __init__(self, room, keys, moves, path, b_visits):
        self.room = room
        self.keys = set(keys)
        self.moves = moves
        self.path = path
        self.b_visits = b_visits
    
    def __lt__(self, other):
        return self.moves < other.moves