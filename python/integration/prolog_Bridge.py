from pyswip import Prolog
import os
from pathlib import Path
import re

class PrologBridge:
    def __init__(self):
        self.prolog = Prolog()
        self._configure_prolog()
        self._load_essential_files()
    
    def _configure_prolog(self):
        """Configuración inicial de Prolog para mejor rendimiento"""
        self.prolog.query("set_prolog_flag(stack_limit, 16_000_000)")
        self.prolog.query("set_prolog_flag(toplevel_print_options, [quoted(true), portray(true), max_depth(1000)])")
    
    def _load_essential_files(self):
        """Carga los archivos Prolog en el orden correcto"""
        base_dir = Path(__file__).parent.parent.parent
        prolog_dir = base_dir / "prolog"
        
        load_order = [
            "facts.pl",
            "state.pl", 
            "constraints.pl",
            "rules.pl",
            "search.pl"
        ]
        
        for file in load_order:
            file_path = prolog_dir / file
            query = f"consult('{file_path.as_posix()}')"
            list(self.prolog.query(query))

    def initialize_game(self, choice=1):
        """Inicializa el juego según la elección del usuario"""
        if choice == 1:
            query = "facts:load_predefined_game, constraints:load_default_constraints, rules:initialize_game"
        elif choice == 2:
            query = "facts:create_custom_game, constraints:create_custom_constraints, rules:initialize_game"
        else:
            raise ValueError("La elección debe ser 1 (predefinido) o 2 (personalizado)")
        
        return bool(list(self.prolog.query(query)))

    def find_escape_plan(self):
        """
        Ejecuta el BFS de Prolog y captura la solución completa
        """
        try:
            # Primero obtener la salida cruda del BFS
            output = list(self.prolog.query(
                "with_output_to(codes(Codes), search:find_escape_solution), "
                "atom_codes(Output, Codes)"
            ))[0]['Output']
            
            # Procesar la salida para extraer los pasos
            return self._parse_solution(output)
        except Exception as e:
            print(f"Error al buscar solución: {str(e)}")
            return None

    def _parse_solution(self, output):
        """
        Parsea la salida de Prolog para extraer los pasos de la solución
        """
        # Buscar líneas que comienzan con "- " (los pasos de la solución)
        steps = []
        solution_section = False
        
        for line in output.split('\n'):
            if "Steps to escape:" in line:
                solution_section = True
                continue
            elif "Total steps required:" in line:
                break
                
            if solution_section and line.strip().startswith("- "):
                steps.append(line.strip()[2:])
        
        return steps if steps else None

    def get_current_state(self):
        """Obtiene el estado actual del juego"""
        try:
            return {
                'room': self._query_single("state:player_location(Room)", "Room"),
                'inventory': self._query_list("state:inventory(Items)", "Items"),
                'solved_puzzles': self._query_list("state:puzzle_solved(Puzzle)", "Puzzle")
            }
        except:
            return None
    
    def _query_single(self, query, var_name):
        """Consulta que retorna un solo valor"""
        result = list(self.prolog.query(query))
        return result[0][var_name] if result else None
    
    def _query_list(self, query, var_name):
        """Consulta que retorna una lista de valores"""
        return [res[var_name] for res in self.prolog.query(query)]