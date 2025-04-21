class Search:
    def __init__(self, prolog_bridge):
        self.prolog = prolog_bridge.prolog

    def find_escape_solution(self):
        """Python implementation of find_escape_solution/0 from search.pl"""
        print('Searching for escape solution...')
        
        # Get the starting room and final room
        start_room = next(self.prolog.query("state:player_location(Room)"))['Room']
        final_room = next(self.prolog.query("facts:final_room(Room)"))['Room']
        print(f'Planning escape from {start_room} to {final_room}')
        
        # Query the BFS solution
        solution = list(self.prolog.query("""
            state:player_location(StartRoom),
            InitialState = state(StartRoom, [], [], [], [], []),
            bfs(InitialState, _, Solution),
            length(Solution, Steps)
        """))
        
        if solution:
            sol = solution[0]
            print('Solution found! Steps to escape:')
            self._print_solution(sol['Solution'])
            print(f'Total steps required: {sol["Steps"]}')
            return sol['Solution']
        else:
            print('No escape solution found! The room might be unsolvable.')
            return None

    def _print_solution(self, solution):
        """Helper to print the solution steps"""
        for step in solution:
            print(f"- {step}")