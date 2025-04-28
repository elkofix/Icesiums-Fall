class StateManager:
    def __init__(self, prolog_bridge):
        self.prolog = prolog_bridge.prolog
        self.collected_items = {
            'keys': set(),
            'pieces': set()
        }

    def player_location(self):
        """Get current player location"""
        result = list(self.prolog.query("state:player_location(Room)"))
        return result[0]['Room'] if result else None
    
    def guard_location(self):
        """Get current guard location"""
        result = list(self.prolog.query("adversary:guard_location(GuardRoom)"))
        return result[0]['GuardRoom'] if result else None

    def inventory(self):
        """Get current inventory"""
        
        result = list(self.prolog.query("state:inventory(Inv)"))
        print("invez", result)
        return result[0]['Inv'] if result else []

    def door_state(self, room1, room2):
        """Get door state between two rooms"""
        result = list(self.prolog.query(f"state:door_state({room1}, {room2}, State)"))
        if result:
            return result[0]['State']
        # Check reverse direction if not found
        result = list(self.prolog.query(f"state:door_state({room2}, {room1}, State)"))
        return result[0]['State'] if result else None

    def has_piece(self, puzzle, piece):
        """Check if player has a puzzle piece"""
        return bool(list(self.prolog.query(f"state:has_piece({puzzle}, {piece})")))

    def object_moved(self, obj):
        """Check if an object has been moved"""
        return bool(list(self.prolog.query(f"state:object_moved({obj})")))

    def puzzle_solved(self, puzzle):
        """Check if a puzzle is solved"""
        return bool(list(self.prolog.query(f"state:puzzle_solved({puzzle})")))

    def move_player(self, new_room):
        """Move player to new room"""
        return bool(list(self.prolog.query(f"state:move_player({new_room})")))

    def pick_key(self, key):
        """Pick up a key"""
        query = f"""
            with_output_to(
                codes(Codes),
                state:pick_key({key})
            ),
            atom_codes(Output, Codes)
        """
        result = list(self.prolog.query(query))[0]['Output']
        result = result.strip()  # Elimina espacios y saltos de línea
        
        expected_message = f"You have picked up key: {key}"
        if result == expected_message:
            self.collected_items['keys'].add(key)
            print("temps agre", self.collected_items)
            return True
        return False


    def pick_piece(self, piece):
        """Pick up a puzzle piece"""
        query = f"""
                with_output_to(
                    codes(Codes),
                    state:pick_piece({piece})
                ),
                atom_codes(Output, Codes)
            """
        result = list(self.prolog.query(query))[0]['Output']
        result = result.strip()  # Elimina espacios y saltos de línea
        if "You have picked up" in result:
            self.collected_items['pieces'].add(piece)
            return True
        else:
            return False

    def move_object(self, obj):
        """Move an object in the room"""
        return bool(list(self.prolog.query(f"state:move_object({obj})")))

    def solve_puzzle(self, puzzle):
        """Solve a puzzle"""
        query = f"""
                with_output_to(
                    codes(Codes),
                    state:solve_puzzle({puzzle})
                ),
                atom_codes(Output, Codes)
            """
        result = list(self.prolog.query(query))[0]['Output']
        result = result.strip()  # Elimina espacios y saltos de línea    
        print("puzz", result)    
        if "Congratulations" in result or "already" in result:
            return True
        else:
            return False

    def unlock_door(self, from_room, to_room):
        """Unlock a door between rooms"""
        query = f"""
                with_output_to(
                    codes(Codes),
                    state:unlock_door({from_room}, {to_room})
                ),
                atom_codes(Output, Codes)
            """
        result = list(self.prolog.query(query))[0]['Output']
        result = result.strip()  # Elimina espacios y saltos de línea        
        # Verificamos si el resultado contiene "has been unlocked"
        print("resultaopuer",result)
        if "has been unlocked" in result:
            return True
        else:
            return False


    def has_key(self, key):
        """Check if player has a key"""
        return bool(list(self.prolog.query(f"state:has_key({key})")))

    def remove_key(self, key):
        """Remove a key from inventory"""
        return bool(list(self.prolog.query(f"state:remove_key({key})")))

    def drop_key(self, key):
        """Drop a key in current room"""
        query = f"""
                with_output_to(
                    codes(Codes),
                    state:drop_key({key})
                ),
                atom_codes(Output, Codes)
            """
        result = list(self.prolog.query(query))[0]['Output']
        result = result.strip()  # Elimina espacios y saltos de línea        
        if "No tienes" in result:
            return False
        else:
            self.collected_items['keys'].discard(key)
            return True


    def drop_piece(self, piece):
        """Drop a puzzle piece in current room"""
        query = f"""
                with_output_to(
                    codes(Codes),
                    state:drop_piece({piece})
                ),
                atom_codes(Output, Codes)
            """
        result = list(self.prolog.query(query))[0]['Output']
        result = result.strip()  # Elimina espacios y saltos de línea 
        if "No tienes" in result:
            return False
            
        else:
            self.collected_items['pieces'].discard(piece)
            return True

    def key_dropped(self, key, room):
        """Check if a key was dropped in a room"""
        return bool(list(self.prolog.query(f"state:key_dropped({key}, {room})")))

    def piece_dropped(self, piece, puzzle, room):
        """Check if a puzzle piece was dropped in a room"""
        return bool(list(self.prolog.query(f"state:piece_dropped({piece}, {puzzle}, {room})")))

    def get_current_state(self):
        """Get comprehensive game state"""
        result = list(self.prolog.query("""
            state:player_location(Room),
            state:inventory(Inv),
            findall(P, state:puzzle_solved(P), Puzzles),
            findall(key_dropped(Key, R), state:key_dropped(Key, R), DroppedKeys),
            findall(piece_dropped(Piece, Puzzle, R), state:piece_dropped(Piece, Puzzle, R), DroppedPieces)
        """))
        
        if not result:
            return None
            
        state = {
            'room': result[0]['Room'],
            'inventory': result[0]['Inv'],
            'solved_puzzles': result[0]['Puzzles'],
            'dropped_keys': result[0]['DroppedKeys'],
            'dropped_pieces': result[0]['DroppedPieces']
        }
        
        # Get all door states
        door_states = []
        door_query = list(self.prolog.query("facts:door(From, To, _), state:door_state(From, To, State)"))
        for door in door_query:
            door_states.append({
                'from': door['From'],
                'to': door['To'],
                'state': door['State']
            })
        state['doors'] = door_states
        
        return state

    def initialize_game(self, choice=1):
        """Initialize game state"""
        if choice == 1:
            print("Loading predefined game...")
            return bool(list(self.prolog.query("""
                facts:load_predefined_game,
                constraints:load_default_constraints,
                state:player_location(a),
                state:retractall(inventory(_)),
                state:assertz(inventory([]))
            """)))
        elif choice == 2:
            print("Creating custom game...")
            return bool(list(self.prolog.query("""
                facts:create_custom_game,
                constraints:create_custom_constraints,
                state:player_location(a),
                state:retractall(inventory(_)),
                state:assertz(inventory([]))
            """)))
        return False

    def check_win_condition(self):
        """Check if player has reached the final room"""
        result = list(self.prolog.query("facts:final_room(Room), state:player_location(Room)"))
        return bool(result)

    def get_available_actions(self):
        """Get available actions in current room"""
        actions = []
        current_room = self.player_location()
        
        # Check for keys in room
        keys = list(self.prolog.query(f"facts:key_in_room({current_room}, Key)"))
        for key in keys:
            actions.append(f"pick_key:{key['Key']}")
        
        # Check for dropped keys in room
        dropped_keys = list(self.prolog.query(f"state:key_dropped(Key, {current_room})"))
        for key in dropped_keys:
            actions.append(f"pick_key:{key['Key']}")
        
        # Check for puzzle pieces in room
        pieces = list(self.prolog.query(f"facts:piece_in_room({current_room}, Piece, Puzzle)"))
        for piece in pieces:
            actions.append(f"pick_piece:{piece['Piece']}")
        
        # Check for dropped pieces in room
        dropped_pieces = list(self.prolog.query(f"state:piece_dropped(Piece, Puzzle, {current_room})"))
        for piece in dropped_pieces:
            actions.append(f"pick_piece:{piece['Piece']}")
        
        # Check for movable objects
        objects = list(self.prolog.query(f"facts:object_in_room({current_room}, Object)"))
        for obj in objects:
            if not self.object_moved(obj['Object']):
                actions.append(f"move_object:{obj['Object']}")
        
        # Check for puzzles that can be solved
        puzzles = list(self.prolog.query(f"facts:puzzle_room(Puzzle, {current_room}), \+ state:puzzle_solved(Puzzle)"))
        for puzzle in puzzles:
            actions.append(f"solve_puzzle:{puzzle['Puzzle']}")
        
        # Check for doors that can be unlocked
        doors = list(self.prolog.query(f"facts:door({current_room}, OtherRoom, _), state:door_state({current_room}, OtherRoom, locked)"))
        for door in doors:
            actions.append(f"unlock_door:{door['OtherRoom']}")
        
        # Check for adjacent rooms
        adjacent = list(self.prolog.query(f"facts:door({current_room}, OtherRoom, _)"))
        for room in adjacent:
            door_state = self.door_state(current_room, room['OtherRoom'])
            if door_state == 'unlocked':
                actions.append(f"move_to:{room['OtherRoom']}")
        
        return actions
    
    def get_collected_items(self):
        """Return all collected keys and pieces"""
        print("entrego",self.collected_items)
        return {
            'keys': list(self.collected_items['keys']),
            'pieces': list(self.collected_items['pieces'])
        }

    def has_collected_key(self, key):
        """Check if a specific key has been collected"""
        return key in self.collected_items['keys']

    def has_collected_piece(self, piece):
        """Check if a specific puzzle piece has been collected"""
        return piece in self.collected_items['pieces']