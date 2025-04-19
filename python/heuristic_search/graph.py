
class Room:
    def __init__(self, name, coordinates=None):
        self.name = name
        self.coordinates = coordinates or (0, 0)
        self.connections = []  # (neighbor_name, cost, is_locked, key_required, puzzle_required)
        self.has_keys = []
        self.has_pieces = []

    def add_connection(self, neighbor, cost=1, is_locked=False, key_required=None, puzzle_required=None):
        self.connections.append((neighbor, cost, is_locked, key_required, puzzle_required))

    def add_item(self, item, item_type):
        if item_type == 'key':
            self.has_keys.append(item)
        elif item_type == 'piece':
            self.has_pieces.append(item)


class EscapeRoomGraph:
    def __init__(self):
        self.rooms = {}
        self.trap_room = 'B'
        self.trap_threshold = 3
        self.max_moves = 30
        self.inventory_limits = {
            'key': 2,
            'piece': 4
        }
        self.puzzle_requirements = {}  # puzzle_name -> set of pieces required

    def add_room(self, name, coordinates=None):
        self.rooms[name] = Room(name, coordinates)

    def get_room(self, name):
        return self.rooms.get(name)

    def add_connection(self, room1, room2, cost=1, is_locked=False, key_required=None, puzzle_required=None):
        if room1 in self.rooms and room2 in self.rooms:
            self.rooms[room1].add_connection(room2, cost, is_locked, key_required, puzzle_required)
            self.rooms[room2].add_connection(room1, cost, is_locked, key_required, puzzle_required)

    def lock_room(self, from_room, to_room, key=None, puzzle=None):
        # Filtra las conexiones para from_room
        from_conns = [conn for conn in self.rooms[from_room].connections if conn[0] != to_room]
        self.rooms[from_room].connections = from_conns

        # Filtra las conexiones para to_room (usa lista separada)
        to_conns = [conn for conn in self.rooms[to_room].connections if conn[0] != from_room]
        self.rooms[to_room].connections = to_conns

        # Añade la nueva conexión bloqueada
        self.add_connection(from_room, to_room, cost=1, is_locked=True, key_required=key, puzzle_required=puzzle)


    def add_item_to_room(self, room, item, item_type):
        if room in self.rooms:
            self.rooms[room].add_item(item, item_type)

    def set_puzzle_requirement(self, puzzle_name, pieces):
        self.puzzle_requirements[puzzle_name] = set(pieces)

    def set_trap_room(self, room, threshold):
        self.trap_room = room
        self.trap_threshold = threshold

    def set_max_moves(self, limit):
        self.max_moves = limit

    def set_inventory_limit(self, item_type, limit):
        self.inventory_limits[item_type] = limit

    def initialize_simple_graph(self):
        self.add_room('A', (0, 0))
        self.add_room('B', (0, 1))
        self.add_room('C', (1, 0))
        self.add_room('D', (1, 1))

        # Initial connections (lineal structure)
        self.add_connection('A', 'B')
        self.add_connection('B', 'C')
        self.add_connection('C', 'D')

        # Add items
        self.add_item_to_room('A', 'key1', 'key')
        self.add_item_to_room('A', 'p3', 'piece')
        self.add_item_to_room('A', 'puzzle2_piece1', 'piece')
        self.add_item_to_room('A', 'puzzle2_piece2', 'piece')

        self.add_item_to_room('B', 'key2', 'key')
        self.add_item_to_room('B', 'p1', 'piece')
        self.add_item_to_room('B', 'p2', 'piece')

        # Puzzle requirements
        self.set_puzzle_requirement('puzzle1', ['p1', 'p2', 'p3'])
        self.set_puzzle_requirement('puzzle2', ['puzzle2_piece1', 'puzzle2_piece2'])

        # Locks
        self.lock_room('A', 'B', key='key1')
        self.lock_room('B', 'C', puzzle='puzzle1')
        self.lock_room('C', 'D', key='key2', puzzle='puzzle2')

        # Constraints
        self.set_trap_room('B', 3)
        self.set_max_moves(30)
        self.set_inventory_limit('key', 2)
        self.set_inventory_limit('piece', 4)

    def visualize(self):
        print("Rooms and connections:")
        for room_name, room in self.rooms.items():
            print(f"Room {room_name} at {room.coordinates}")
            for conn in room.connections:
                dest, cost, is_locked, key_req, puzzle_req = conn
                lock_str = ""
                if is_locked:
                    lock_str = f"(Locked"
                    if key_req:
                        lock_str += f", Key: {key_req}"
                    if puzzle_req:
                        lock_str += f", Puzzle: {puzzle_req}"
                    lock_str += ")"
                print(f"  -> {dest}, cost: {cost} {lock_str}")
            for key in room.has_keys:
                print(f"  Contains key: {key}")
            for piece in room.has_pieces:
                print(f"  Contains piece: {piece}")
        print("\nConstraints:")
        print(f"  Trap room: {self.trap_room}, max visits: {self.trap_threshold}")
        print(f"  Max moves allowed: {self.max_moves}")
        print(f"  Inventory limits: {self.inventory_limits}")
