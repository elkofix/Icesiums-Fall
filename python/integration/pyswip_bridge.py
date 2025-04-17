from pyswip import Prolog
import os
from pathlib import Path

class PrologBridge:
    def __init__(self, prolog_path=None):
        self.prolog = Prolog()
        self.prolog_path = self._resolve_prolog_path(prolog_path)
        self._load_prolog_files()
    
    def _resolve_prolog_path(self, prolog_path):
        """Resuelve la ruta absoluta a los archivos Prolog"""
        if prolog_path is None:
            # Busca automáticamente la carpeta prolog en el directorio padre
            base_dir = Path(__file__).parent.parent.parent
            return str(base_dir / "prolog")
        return str(Path(prolog_path).resolve())
    
    def _load_prolog_files(self):
        """Carga todos los archivos Prolog necesarios"""
        files = [
            "facts.pl",
            "rules.pl",
            "constraints.pl",
            "state.pl",
            "search.pl",
        ]
        
        for file in files:
            full_path = Path(self.prolog_path) / file
            if not full_path.exists():
                raise FileNotFoundError(f"Archivo Prolog no encontrado: {full_path}")
            
            # Convertir a formato compatible con Prolog
            prolog_path = str(full_path).replace("\\", "/")
            query = f"consult('{prolog_path}')"
            
            try:
                result = list(self.prolog.query(query))
                if not result:
                    print(f"Archivo cargado: {prolog_path}")
            except Exception as e:
                print(f"Error al cargar {prolog_path}: {e}")
                raise
    
    def validate_state(self, room, keys, moves, b_visits):
        """Consulta a Prolog si un estado es válido"""
        query = f"valid_state({room}, {list(keys)}, {moves}, {b_visits})"
        return bool(list(self.prolog.query(query)))
    
    def get_possible_moves(self, room, keys):
        """Versión que maneja tanto llaves como puzzles"""
        try:
            # Actualizar estado en Prolog
            self._update_prolog_state(keys)
            
            # Consultar movimientos posibles
            query = f"facts:door({room.lower()}, Next, State), (State = unlocked ; (State = locked, facts:door_requirements({room}, Next, Reqs), satisfy_requirements(Reqs)))."

            results = list(self.prolog.query(query))
            return [res['Next'].upper() for res in results]
            
        except Exception as e:
            print(f"Error en get_possible_moves: {e}")
            return self._basic_moves(room, keys)

    def _update_prolog_state(self, keys, puzzles=None):
        """Actualiza tanto llaves como puzzles resueltos en Prolog"""
        puzzles = puzzles or []
        
        # Sincronizar llaves
        list(self.prolog.query("retractall(state:has_key(_))"))
        for key in keys:
            list(self.prolog.query(f"assertz(state:has_key({key}))"))
        
        # Sincronizar puzzles
        list(self.prolog.query("retractall(state:puzzle_solved(_))"))
        for puzzle in puzzles:
            list(self.prolog.query(f"assertz(state:puzzle_solved({puzzle}))"))
        
    def _basic_moves(self, room, keys):
        """Fallback básico si falla la consulta a Prolog"""
        room_obj = self.graph.get_room(room)
        return [conn[0] for conn in room_obj.connections]
    
    def check_solution(self, path):
        """Verifica si el camino es solución válida en Prolog"""
        query = f"check_solution({path})"
        return bool(list(self.prolog.query(query)))

    def try_resolve_puzzle(self, puzzle, room):
        # 1. Recoger piezas visibles
        visible_pieces = self.query(f"piece_in_room({room}, Piece, {puzzle}).")
        for entry in visible_pieces:
            piece = entry["Piece"]
            if not self.query(f"picked_piece({piece})."):
                print(f"Recogiendo pieza visible {piece} en {room}")
                self.query(f"pick_piece({piece}).")

        # 2. Buscar objetos que esconden piezas
        hidden = self.query(f"hides_piece(Object, {puzzle}, Piece).")
        for entry in hidden:
            obj = entry["Object"]
            piece = entry["Piece"]

            # Mover el objeto si no se ha movido aún
            if not self.query(f"moved_object({obj})."):
                print(f"Moviendo objeto {obj} para revelar {piece}")
                self.query(f"move_object({obj}).")

            # Recoger la pieza si no se ha recogido
            if not self.query(f"picked_piece({piece})."):
                print(f"Recogiendo pieza oculta {piece} de {obj}")
                self.query(f"pick_piece({piece}).")

        # 3. Verificar si el puzzle está resuelto
        resolved = self.query(f"can_resolve_puzzle({puzzle}).")
        if resolved:
            print(f"Puzzle {puzzle} ahora está resuelto.")
            self.query(f"mark_puzzle_resolved({puzzle}).")
            return True
        return False


    def pick_piece(self, piece):
        query = f"pick_piece({piece})"
        return bool(list(self.prolog.query(query)))

    def try_move_object(self, object_):
        query = f"move_object({object_})"
        return bool(list(self.prolog.query(query)))


    def assert_piece_collected(self, piece):
        self.prolog.assertz(f"pieza_recolectada({piece})")

    def try_solve_puzzle(self, puzzle):
        query = f"solve_puzzle({puzzle})"
        return bool(list(self.prolog.query(query)))

    def get_visible_pieces(self, room):
        query = f"facts:piece_in_room({room}, Piece, Puzzle)"
        return [(str(result["Piece"]), str(result["Puzzle"])) for result in self.prolog.query(query)]

    def get_hidden_pieces(self, room):
        query = f"facts:object_in_room({room}, Object), facts:hides_piece(Object, Puzzle, Piece)"
        return [(str(result["Object"]), str(result["Piece"]), str(result["Puzzle"])) for result in self.prolog.query(query)]
    
    def update_guard_position(self, room):
        """Actualiza la posición del guardia en Prolog"""
        self.prolog.assertz(f"guard_position({room})")
        
    def get_guard_position(self):
        """Obtiene la posición actual del guardia"""
        try:
            return list(self.prolog.query("guard_position(X)"))[0]['X']
        except:
            return None
