class SearchNoConstraints:
    def __init__(self, prolog_bridge):
        self.prolog = prolog_bridge.prolog
        
    def find_escape_solution_no_constraints(self):
        """Finds an escape solution without inventory constraints"""
        print('Buscando solución de escape sin restricciones de inventario...')
        
        # Get start and end rooms
        start_room = list(self.prolog.query("state:player_location(Room)"))[0]['Room']
        final_room = list(self.prolog.query("facts:final_room(Room)"))[0]['Room']
        print(f'Planificando escape desde {start_room} hasta {final_room}')
        
        # Query the solution using BFS without constraints
        query = """
            with_output_to(
                codes(Codes),
                search_no_constraints:find_escape_solution_no_constraints
            ),
            atom_codes(Output, Codes)
        """
        
        try:
            result = list(self.prolog.query(query))
            if result:
                output = result[0]['Output']
                self._print_solution(output)
                # Count steps from output
                steps = [line for line in output.split('\n') if line.startswith('- ')]
                print(f'Total de pasos requeridos: {len(steps)}')
                return steps
            else:
                print('¡No se encontró solución de escape! La habitación podría ser irresoluble.')
                return None
                
        except Exception as e:
            print(f'Error en la búsqueda: {str(e)}')
            return None
            
    def _print_solution(self, output):
        """Prints the solution steps from Prolog output"""
        if not output:
            return
            
        for line in output.split('\n'):
            if line.startswith('¡Solución encontrada!'):
                print(line)
            elif line.startswith('- '):
                print(line)
            elif 'Total de pasos requeridos' in line:
                print(line)
                break