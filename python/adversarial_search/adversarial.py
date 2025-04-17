from pyswip import Prolog
from integration.pyswip_bridge import PrologBridge
import math
import random

class AdversarialAI:
    def __init__(self, graph):
        self.prolog = PrologBridge()
        self.graph = graph
        self.max_depth = 3  # Profundidad máxima para Minimax
        
    def get_guard_move(self, guard_room, player_room):
        """
        Decide el mejor movimiento para el guardia usando Minimax con poda Alpha-Beta.
        Versión simplificada sin dependencia de habitaciones específicas.
        """
        if guard_room == player_room:
            return guard_room
            
        best_move = guard_room  # Por defecto, quedarse en el mismo lugar
        best_value = -math.inf
        alpha = -math.inf
        beta = math.inf
        
        possible_moves = self._get_guard_moves(guard_room)
        
        if not possible_moves:
            return guard_room
            
        # Ordenar movimientos por proximidad al jugador
        possible_moves.sort(key=lambda m: self._manhattan_distance(m, player_room))
        
        for move in possible_moves:
            value = self._minimax(move, player_room, 1, False, alpha, beta)
            
            # Pequeña aleatoriedad para evitar patrones predecibles
            if value > best_value or (value == best_value and random.random() > 0.7):
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
        if depth == self.max_depth:
            return self._evaluate_state(guard_room, player_room)
            
        if guard_room == player_room:
            return math.inf if is_maximizing else -math.inf
            
        if is_maximizing:
            max_eval = -math.inf
            for move in self._get_guard_moves(guard_room):
                eval = self._minimax(move, player_room, depth + 1, False, alpha, beta)
                max_eval = max(max_eval, eval)
                alpha = max(alpha, eval)
                if beta <= alpha:
                    break
            return max_eval
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
        """Obtiene movimientos posibles para el guardia"""
        room = self.graph.get_room(current_room)
        return [conn[0] for conn in room.connections]

    def _get_player_moves(self, current_room):
        """Obtiene movimientos posibles para el jugador"""
        try:
            query = f"player_location({current_room}), findall(R, (can_move({current_room}, R)), Moves)"
            moves = list(self.prolog.query(query))
            if moves:
                return moves[0]['Moves']
        except:
            room = self.graph.get_room(current_room)
            return [conn[0] for conn in room.connections if not conn[2]]

    def _evaluate_state(self, guard_room, player_room):
        """
        Función de evaluación simplificada:
        1. Premia acercarse al jugador
        2. Penaliza dejar al jugador avanzar
        3. Bonus por captura
        """
        distance_score = -self._manhattan_distance(guard_room, player_room)
        capture_bonus = 20 if guard_room == player_room else 0
        return distance_score + capture_bonus

    def _manhattan_distance(self, room1, room2):
        """Distancia Manhattan entre habitaciones"""
        order = {'A': 0, 'B': 1, 'C': 2, 'D': 3}
        try:
            return abs(order[room1] - order[room2])
        except KeyError:
            return 0