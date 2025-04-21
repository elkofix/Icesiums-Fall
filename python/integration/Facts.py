class Facts:
    def __init__(self, prolog_bridge):
        self.prolog = prolog_bridge.prolog
        
    # Dynamic definition functions
    def add_room(self, room):
        return bool(list(self.prolog.query(f"facts:add_room({room})")))
    
    def add_door(self, from_room, to_room, state):
        return bool(list(self.prolog.query(f"facts:add_door({from_room}, {to_room}, {state})")))
    
    def add_key(self, room, key):
        return bool(list(self.prolog.query(f"facts:add_key({room}, {key})")))
    
    def add_object(self, room, obj):
        return bool(list(self.prolog.query(f"facts:add_object({room}, {obj})")))
    
    def add_puzzle(self, puzzle):
        return bool(list(self.prolog.query(f"facts:add_puzzle({puzzle})")))
    
    def add_piece(self, puzzle, piece):
        return bool(list(self.prolog.query(f"facts:add_piece({puzzle}, {piece})")))
    
    def hide_piece(self, obj, puzzle, piece):
        return bool(list(self.prolog.query(f"facts:hide_piece({obj}, {puzzle}, {piece})")))
    
    def set_puzzle_room(self, puzzle, room):
        return bool(list(self.prolog.query(f"facts:set_puzzle_room({puzzle}, {room})")))
    
    def set_door_requirements(self, from_room, to_room, requirements):
        return bool(list(self.prolog.query(f"facts:set_door_requirements({from_room}, {to_room}, {requirements})")))
    
    def add_visible_piece(self, room, piece, puzzle):
        return bool(list(self.prolog.query(f"facts:add_visible_piece({room}, {piece}, {puzzle})")))
    
    def set_final_room(self, room):
        return bool(list(self.prolog.query(f"facts:set_final_room({room})")))
    
    # Game configuration functions
    def clear_game_data(self):
        return bool(list(self.prolog.query("facts:clear_game_data")))
    
    def load_predefined_game(self):
        result = bool(list(self.prolog.query("facts:load_predefined_game")))
        if result:
            print("Predefined game loaded successfully!")
        return result
    
    def set_game_mode(self, mode):
        return bool(list(self.prolog.query(f"facts:set_game_mode({mode})")))
    
    def choose_game_mode(self, mode_choice):
        """Replaces the interactive choose_game_mode with a parameterized version"""
        if mode_choice == 1:
            self.set_game_mode("standard")
            return True
        elif mode_choice == 2:
            self.set_game_mode("adversary")
            return True
        else:
            print("Invalid choice. Please enter 1 or 2.")
            return False
        
    def set_guard_movement_type(self, movement_choice):
        """Changes the guard movement mode either predictive or random"""
        if movement_choice == 1:
            self.prolog.query(f"adversary:set_guard_movement_type(predictive)")
            return True
        elif movement_choice == 2:
            self.prolog.query(f"adversary:set_guard_movement_type(random)")
            return True
        else:
            print("Invalid choice. Please enter 1 or 2.")
            return False

    # Custom game creation functions (parameterized versions)
    def create_custom_game(self, game_config):
        """Creates a custom game from a configuration dictionary"""
        self.clear_game_data()
        print("Creating a custom escape room game.")
        
        # Rooms
        print("Setting up rooms...")
        for room in game_config.get("rooms", []):
            self.add_room(room)
        
        # Doors
        print("Setting up doors...")
        for door in game_config.get("doors", []):
            self.add_door(door["from"], door["to"], door["state"])
        
        # Keys
        print("Placing keys...")
        for key in game_config.get("keys", []):
            self.add_key(key["room"], key["key_name"])
        
        # Objects
        print("Placing objects...")
        for obj in game_config.get("objects", []):
            self.add_object(obj["room"], obj["object_name"])
        
        # Puzzles
        print("Creating puzzles...")
        for puzzle in game_config.get("puzzles", []):
            self.add_puzzle(puzzle)
        
        # Pieces
        print("Assigning pieces to puzzles...")
        for piece in game_config.get("pieces", []):
            self.add_piece(piece["puzzle"], piece["piece_name"])
        
        # Visible pieces
        print("Placing visible pieces...")
        for v_piece in game_config.get("visible_pieces", []):
            self.add_visible_piece(v_piece["room"], v_piece["piece"], v_piece["puzzle"])
        
        # Hidden pieces
        print("Hiding pieces in objects...")
        for h_piece in game_config.get("hidden_pieces", []):
            self.hide_piece(h_piece["object"], h_piece["puzzle"], h_piece["piece"])
        
        # Puzzle rooms
        print("Setting puzzle rooms...")
        for p_room in game_config.get("puzzle_rooms", []):
            self.set_puzzle_room(p_room["puzzle"], p_room["room"])
        
        # Door requirements
        print("Setting door requirements...")
        for req in game_config.get("door_requirements", []):
            self.set_door_requirements(req["from"], req["to"], req["requirements"])
        
        # Final room
        final_room = game_config.get("final_room")
        if final_room:
            if self.set_final_room(final_room):
                print(f"Final room set to {final_room} successfully!")
            else:
                print(f"Failed to set final room {final_room}")
        
        # Adversary setup if in adversary mode
        if game_config.get("game_mode") == "adversary":
            guard_room = game_config.get("guard_location")
            if guard_room:
                self.prolog.query(f"adversary:set_initial_guard_location({guard_room})")
                print(f"Guard will start in room {guard_room}.")
            
            movement_type = game_config.get("guard_movement_type", "predictive")
            self.prolog.query(f"adversary:set_guard_movement_type({movement_type})")
            print(f"Guard will use {movement_type} movement.")
        
        print("Custom game created successfully!")
        return True
    
    # Query functions
    def get_rooms(self):
        return list(self.prolog.query("facts:room(Room)"))
    
    def get_doors(self):
        return list(self.prolog.query("facts:door(From, To, State)"))
    
    def get_keys(self):
        return list(self.prolog.query("facts:key_in_room(Room, Key)"))
    
    def get_objects(self):
        return list(self.prolog.query("facts:object_in_room(Room, Object)"))
    
    def get_puzzles(self):
        return list(self.prolog.query("facts:puzzle(Puzzle)"))
    
    def get_pieces(self):
        return list(self.prolog.query("facts:piece(Puzzle, Piece)"))
    
    def get_piece_locations(self):
        return list(self.prolog.query("facts:piece_location(Puzzle, Piece, Room)"))
    
    def get_puzzle_rooms(self):
        return list(self.prolog.query("facts:puzzle_room(Puzzle, Room)"))
    
    def get_door_requirements(self):
        return list(self.prolog.query("facts:door_requirements(From, To, Requirements)"))
    
    def get_final_room(self):
        result = list(self.prolog.query("facts:final_room(Room)"))
        return result[0]["Room"] if result else None
    
    def get_game_mode(self):
        result = list(self.prolog.query("facts:game_mode(Mode)"))
        return result[0]["Mode"] if result else "standard"