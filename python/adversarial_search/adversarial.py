# adversarial.py
from pyswip import Prolog
from integration.pyswip_bridge import PrologBridge
import math

class AdversarialAI:
    def __init__(self, graph):
        self.prolog = PrologBridge()
        self.graph = graph
        self.max_depth = 3  # Profundidad máxima para Minimax
        
    def get_guard_move(self, guard_room, player_room):
        """
        Decide el mejor movimiento para el guardia usando Minimax con poda Alpha-Beta.
        """
        best_move = None
        best_value = -math.inf
        alpha = -math.inf
        beta = math.inf
        
        # Obtener movimientos posibles del guardia
        possible_moves = self._get_guard_moves(guard_room)
        
        for move in possible_moves:
            # Simular movimiento del guardia
            value = self._minimax(move, player_room, 1, False, alpha, beta)
            
            if value > best_value:
                best_value = value
                best_move = move
                
            alpha = max(alpha, best_value)
            if beta <= alpha:
                break
                
        return best_move

    def _minimax(self, guard_room, player_room, depth, is_maximizing, alpha, beta):
        """
        Implementación de Minimax con poda Alpha-Beta.
        """
        # Casos base
        if depth == self.max_depth:
            return self._evaluate_state(guard_room, player_room)
            
        if guard_room == player_room:
            return math.inf if is_maximizing else -math.inf
            
        # Turno del guardia (maximizador)
        if is_maximizing:
            max_eval = -math.inf
            for move in self._get_guard_moves(guard_room):
                eval = self._minimax(move, player_room, depth + 1, False, alpha, beta)
                max_eval = max(max_eval, eval)
                alpha = max(alpha, eval)
                if beta <= alpha:
                    break
            return max_eval
            
        # Turno del jugador (minimizador)
        else:
            min_eval = math.inf
            for move in self._get_player_moves(player_room):
                eval = self._minimax(guard_room, move, depth + 1, True, alpha, beta)
                min_eval = min(min_eval, eval)
                beta = min(beta, eval)
                if beta <= alpha:
                    break
            return min_eval

    def _get_guard_moves(self, current_room):
        """
        Obtiene movimientos posibles para el guardia (sin restricciones de puertas).
        """
        room = self.graph.get_room(current_room)
        return [conn[0] for conn in room.connections]

    def _get_player_moves(self, current_room):
        """
        Obtiene movimientos posibles para el jugador (considera puertas bloqueadas).
        """
        # Consultar a Prolog los movimientos válidos
        try:
            query = f"player_location({current_room}), findall(R, (can_move({current_room}, R)), Moves)"
            moves = list(self.prolog.query(query))
            if moves:
                return moves[0]['Moves']
        except:
            pass
        
        # Fallback: movimientos básicos
        room = self.graph.get_room(current_room)
        return [conn[0] for conn in room.connections if not conn[2]]  # Solo puertas no bloqueadas

    def _evaluate_state(self, guard_room, player_room):
        """
        Función de evaluación para Minimax.
        Valores más altos son mejores para el guardia.
        """
        # Distancia Manhattan inversa (el guardia quiere minimizar distancia)
        distance = self._manhattan_distance(guard_room, player_room)
        
        # Penalizar si el jugador está cerca de la salida
        exit_penalty = 0
        player_to_exit = self._manhattan_distance(player_room, 'D')
        if player_to_exit <= 1:
            exit_penalty = -10
            
        # El guardia quiere minimizar distancia y maximizar exit_penalty
        return -distance + exit_penalty

    def _manhattan_distance(self, room1, room2):
        """Distancia Manhattan simplificada entre habitaciones"""
        order = {'A': 0, 'B': 1, 'C': 2, 'D': 3}
        return abs(order[room1] - order[room2])