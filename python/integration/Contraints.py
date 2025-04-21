class Constraints:
    def __init__(self, prolog_bridge):
        self.prolog = prolog_bridge.prolog

    def max_moves(self):
        """Get the current maximum moves limit"""
        result = list(self.prolog.query("constraints:max_moves(X)"))
        return result[0]['X'] if result else None

    def max_b_visits(self):
        """Get the current maximum visits to room B limit"""
        result = list(self.prolog.query("constraints:max_b_visits(X)"))
        return result[0]['X'] if result else None

    def valid_state(self, room, keys, moves, b_visits):
        """Check if the current state is valid"""
        return bool(list(self.prolog.query(
            f"constraints:valid_state({room}, {keys}, {moves}, {b_visits})"
        )))

    def can_carry(self, item_type):
        """Get the carry limit for an item type"""
        result = list(self.prolog.query(f"constraints:can_carry({item_type}, X)"))
        return result[0]['X'] if result else None

    def trap(self, room):
        """Check if a room has a trap"""
        result = list(self.prolog.query(f"constraints:trap({room}, X)"))
        return result[0]['X'] if result else None

    def turns_in_room(self, room):
        """Get the current turn count for a room"""
        result = list(self.prolog.query(f"constraints:turns_in_room({room}, X)"))
        return result[0]['X'] if result else None

    def move_count(self):
        """Get the current move count"""
        result = list(self.prolog.query("constraints:move_count(X)"))
        return result[0]['X'] if result else None

    def increment_move_count(self):
        """Increment the global move counter"""
        return bool(list(self.prolog.query("constraints:increment_move_count")))

    def increment_room_turn(self, room):
        """Increment the turn counter for a specific room"""
        return bool(list(self.prolog.query(f"constraints:increment_room_turn({room})")))

    def check_move_limit(self):
        """Check if move limit has been reached"""
        result = list(self.prolog.query("constraints:check_move_limit"))
        if not result:
            print("Move limit reached!")
        return bool(result)

    def check_trap(self, room):
        """Check if trap is activated in a room"""
        result = list(self.prolog.query(f"constraints:check_trap({room})"))
        print("tampa", result)
        if not result:
            print(f"Trap activated in room {room}!")
        return (not bool(result))

    def check_inventory_limit(self, item_type):
        """Check inventory limits before picking up items"""
        result = list(self.prolog.query(f"constraints:check_inventory_limit({item_type})"))
        if not result:
            print(f"Cannot carry more items of type {item_type}")
        return bool(result)

    def count_items_of_type(self, item_type):
        """Count items of a specific type in inventory"""
        result = list(self.prolog.query(f"constraints:count_items_of_type({item_type}, X)"))
        return result[0]['X'] if result else 0

    def initialize_constraints(self):
        """Initialize constraints for all rooms in the game"""
        return bool(list(self.prolog.query("constraints:initialize_constraints")))

    # Dynamic constraint modification functions
    def set_max_moves(self, limit):
        """Set the maximum moves limit"""
        return bool(list(self.prolog.query(f"constraints:set_max_moves({limit})")))

    def set_max_b_visits(self, limit):
        """Set the maximum visits to room B limit"""
        return bool(list(self.prolog.query(f"constraints:set_max_b_visits({limit})")))

    def set_carry_limit(self, item_type, limit):
        """Set the carry limit for an item type"""
        return bool(list(self.prolog.query(f"constraints:set_carry_limit({item_type}, {limit})")))

    def add_room_trap(self, room, turn_limit):
        """Add a trap to a room"""
        return bool(list(self.prolog.query(f"constraints:add_room_trap({room}, {turn_limit})")))

    def clear_constraints(self):
        """Clear all constraints"""
        return bool(list(self.prolog.query("constraints:clear_constraints")))

    def load_default_constraints(self):
        """Load default constraints"""
        return bool(list(self.prolog.query("constraints:load_default_constraints")))

    def create_custom_constraints(self, max_moves, max_b_visits, key_limit, piece_limit, traps=None):
        """
        Create custom constraints with provided parameters instead of reading from input
        
        Args:
            max_moves: Maximum moves allowed
            max_b_visits: Maximum visits to room B
            key_limit: Maximum keys player can carry
            piece_limit: Maximum puzzle pieces player can carry
            traps: List of tuples (room, turns) for room traps
        """
        # First clear existing constraints
        self.clear_constraints()
        
        # Set the basic constraints
        self.set_max_moves(max_moves)
        self.set_max_b_visits(max_b_visits)
        self.set_carry_limit('key', key_limit)
        self.set_carry_limit('piece', piece_limit)
        
        # Add any traps if provided
        if traps:
            for room, turns in traps:
                self.add_room_trap(room, turns)
        
        print('Custom constraints have been set successfully!')
        return True

    # Helper function for the original custom_room_traps functionality
    def _add_custom_room_traps(self, traps):
        """Helper to add multiple room traps from a list"""
        for room, turns in traps:
            self.add_room_trap(room, turns)
        return True