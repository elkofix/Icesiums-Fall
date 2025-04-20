from pyswip import Prolog
import os
from pathlib import Path
import sys
import time

class PrologBridge:
    def __init__(self):
        self.prolog = Prolog()
        self._configure_prolog()
        self._load_essential_files()
    
    def _configure_prolog(self):
        """Configuración optimizada que no requiere cambios en los archivos .pl"""
        try:
            # Configuración agresiva de memoria
            self.prolog.query("set_prolog_flag(stack_limit, 256_000_000)")  # 256MB
            self.prolog.query("set_prolog_flag(trail_limit, 128_000_000)")  # 128MB trail
            self.prolog.query("set_prolog_flag(optimise, true)")
            self.prolog.query("set_prolog_flag(gc, true)")
        except Exception as e:
            print(f"Config warning: {str(e)}")

    def _load_essential_files(self):
        """Carga de archivos sin modificar los .pl existentes"""
        try:
            base_dir = Path(__file__).parent.parent.parent
            prolog_dir = base_dir / "prolog"
            
            if not prolog_dir.exists():
                raise FileNotFoundError(f"Carpeta 'prolog' no encontrada en: {prolog_dir}")

         
            files_to_load = [
                "facts.pl",
                "state.pl", 
                "constraints.pl",
                "rules.pl",
                "search.pl",
                "main.pl"
            ]
            
            for file in files_to_load:
                file_path = prolog_dir / file
                if file_path.exists():
                    query = f"consult('{file_path.resolve().as_posix()}')"
                    if not list(self.prolog.query(query)):
                        print(f"Warning: File {file} loaded with issues")
                else:
                    print(f"Warning: Missing {file}")

        except Exception as e:
            print(f"CRITICAL: {str(e)}")
            sys.exit(1)

    def find_escape_plan(self, time_limit=120):
        """Búsqueda con protección de recursos sin cambiar el código Prolog"""
        try:
            # Triple protección de recursos
            self.prolog.query("garbage_collect")
            self.prolog.query(f"set_prolog_flag(stack_limit, 256_000_000)")
            
            query = f"""
                call_with_time_limit({time_limit},
                    with_output_to(
                        codes(Codes),
                        find_escape_solution
                    )
                ),
                atom_codes(Output, Codes)
            """
            
            start_time = time.time()
            result = list(self.prolog.query(query))
            exec_time = time.time() - start_time
            
            if not result:
                print(f"Búsqueda terminada (límite: {time_limit}s)")
                return None
                
            solution = self._parse_solution(result[0]['Output'])
            print(f"Búsqueda completada en {exec_time:.2f}s")
            return solution
            
        except Exception as e:
            print(f"Error controlado: {str(e)}")
            return None

    def _parse_solution(self, output):
        """Parseo compatible con tu salida Prolog existente"""
        if not output:
            return None
            
        try:
            steps = []
            lines = output.split('\n')
            
            for line in lines:
                if line.strip().startswith("- "):
                    steps.append(line.strip()[2:])
                elif "Total steps required:" in line:
                    break
                    
            return steps if steps else None
            
        except Exception as e:
            print(f"Parse error: {str(e)}")
            return None

    def initialize_game(self, choice=1):
        """Inicialización que no modifica los .pl"""
        try:
            self.prolog.query("garbage_collect")
            
            if choice == 1:
                return bool(list(self.prolog.query("""
                    facts:load_predefined_game,
                    constraints:load_default_constraints,
                    rules:initialize_game
                """)))
            elif choice == 2:
                return bool(list(self.prolog.query("""
                    facts:create_custom_game,
                    constraints:create_custom_constraints,
                    rules:initialize_game
                """)))
            return False
            
        except Exception as e:
            print(f"Init error: {str(e)}")
            return False

    def get_current_state(self):
        """Consulta compatible con tu state.pl"""
        try:
            result = list(self.prolog.query("""
                state:player_location(Room),
                state:inventory(Inv),
                findall(P, state:puzzle_solved(P), Puzzles)
            """))
            
            if result:
                return {
                    'room': result[0]['Room'],
                    'inventory': result[0]['Inv'],
                    'solved_puzzles': result[0]['Puzzles']
                }
            return None
            
        except Exception as e:
            print(f"State error: {str(e)}")
            return None