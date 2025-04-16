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
            query = f"facts:door({room}, Next, State), (State = unlocked ; (State = locked, facts:door_requirements({room}, Next, Reqs), satisfy_requirements(Reqs)))."

            results = list(self.prolog.query(query))
            return [res['Next'] for res in results]
            
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