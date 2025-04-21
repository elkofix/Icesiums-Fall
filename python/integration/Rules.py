class Rules:
    def __init__(self, prolog_bridge):
        self.prolog = prolog_bridge.prolog
    
    def can_move(self, from_room, to_room):
        """Check if player can move between rooms"""
        return bool(list(self.prolog.query(f"rules:can_move({from_room}, {to_room})")))
    
    def get_possible_moves(self, from_room):
        """Get a list of all rooms the player can move to from a specific room"""
        moves = list(self.prolog.query(f"rules:get_possible_moves({from_room}, Moves)"))
        if moves:
            return moves[0]['Moves']
        return []
    
    def satisfy_requirements(self, requirements):
        """Check if requirements are satisfied"""
        # Requirements should be passed as a list of strings representing Prolog terms
        reqs_str = ','.join(requirements)
        return bool(list(self.prolog.query(f"rules:satisfy_requirements([{reqs_str}])")))
    
    def initialize_game(self):
        """Initialize game state"""
        result = list(self.prolog.query("rules:initialize_game"))
        if result:
            return True
        return False
    
    def game_stats(self):
        """Display game stats including constraints"""
        stats = list(self.prolog.query("rules:game_stats"))
        # The actual printing is done by Prolog's writeln in the original code
        # So we just need to execute the query to trigger the prints
        return bool(stats)
    
    def puzzle_requirements_met(self, puzzle):
        """Check if all pieces for a puzzle have been collected"""
        return bool(list(self.prolog.query(f"rules:puzzle_requirements_met({puzzle})")))
    
    # Helper methods for specific requirements
    def has_key(self, key):
        """Check if player has a specific key"""
        return bool(list(self.prolog.query(f"state:has_key({key})")))
    
    def is_puzzle_solved(self, puzzle):
        """Check if a puzzle is solved"""
        return bool(list(self.prolog.query(f"state:puzzle_solved({puzzle})")))