class Room:
    def __init__(self, name):
        self.name = name
        self.connections = []  
        self.has_key = None     
        self.is_trap = False

class EscapeRoomGraph:
    def __init__(self):
        self.rooms = {}
        self.max_moves = 30
        self.trap_room = 'B'
        self.trap_threshold = 3
        
    def add_room(self, name, has_key=None, is_trap=False):
        """Añade una nueva habitación al grafo"""
        self.rooms[name] = Room(name)
        self.rooms[name].has_key = has_key
        self.rooms[name].is_trap = is_trap
        
    def add_connection(self, room1, room2, cost=1, is_locked=False, key_needed=None):
        """Conecta dos habitaciones"""
        self.rooms[room1].connections.append((room2, cost, is_locked, key_needed))
        self.rooms[room2].connections.append((room1, cost, is_locked, key_needed))
        
    def initialize_simple_graph(self):
        """Configuración inicial del grafo lineal A-B-C-D"""
        self.add_room('A', has_key='key_B') 
        self.add_room('B', is_trap=True, has_key='key_C')
        self.add_room('C', has_key='key_D')
        self.add_room('D')
        
        # Conexiones (todas bloqueadas inicialmente)
        self.add_connection('A', 'B', is_locked=True, key_needed='key_B')
        self.add_connection('B', 'C', is_locked=True, key_needed='key_C')
        self.add_connection('C', 'D', is_locked=True, key_needed='key_D')
    
    def get_room(self, name):
        """Obtiene una habitación por su nombre"""
        return self.rooms.get(name)

        