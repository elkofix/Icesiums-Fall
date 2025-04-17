class Room:
    def __init__(self, name):
        self.name = name
        # Cada conexión es una tupla: (room_name, cost, is_locked, key_needed, puzzle_needed)
        self.connections = []  
        self.has_key = None
        self.is_trap = False
        self.puzzle_piece = None # Pieza de puzzle en la habitación (si existe)
        self.hidden_pieces = []       # ← Lista de piezas ocultas
        self.hides_piece_with = {} 

class EscapeRoomGraph:
    """
    Grafo que representa todo el escape room con sus habitaciones y conexiones.
    
    Atributos:
        rooms (dict): Diccionario de habitaciones {nombre: objeto Room}
        max_moves (int): Límite máximo de movimientos permitidos
        trap_room (str): Nombre de la habitación con trampa
        trap_threshold (int): Límite de visitas a la habitación trampa
    """
    def __init__(self):
        self.rooms = {}
        self.max_moves = 30
        self.trap_room = 'B'
        self.trap_threshold = 3
        
    def add_room(self, name, has_key=None, is_trap=False):
        """
        Añade una nueva habitación al grafo.
        
        Args:
            name (str): Nombre de la habitación
            has_key (str, optional): Llave que contiene. Defaults to None.
            is_trap (bool, optional): Si es una trampa. Defaults to False.
        """
        self.rooms[name] = Room(name)
        self.rooms[name].has_key = has_key
        self.rooms[name].is_trap = is_trap
        
    def add_connection(self, room1, room2, cost=1, is_locked=False, key_needed=None, puzzle_needed=None):
        """
        Conecta dos habitaciones bidireccionalmente con opción de puzzle.
        
        Args:
            room1 (str): Nombre de la primera habitación
            room2 (str): Nombre de la segunda habitación
            cost (int, optional): Coste de moverse entre ellas. Defaults to 1.
            is_locked (bool, optional): Si la conexión está bloqueada. Defaults to False.
            key_needed (str, optional): Llave requerida para desbloquear. Defaults to None.
            puzzle_needed (str, optional): Puzzle requerido para desbloquear. Defaults to None.
        """
        self.rooms[room1].connections.append((room2, cost, is_locked, key_needed, puzzle_needed))
        self.rooms[room2].connections.append((room1, cost, is_locked, key_needed, puzzle_needed))
        
    def initialize_simple_graph(self):
        """Configuración alineada con facts.pl"""
        # Habitaciones
        self.add_room('A', has_key='key1')  # Llave para A-B
        self.add_room('B', is_trap=True)    # B es trampa, no tiene llave (tiene puzzle)
        self.add_room('C', has_key='key2')  # Llave para C-D
        self.add_room('D')                  # Salida
        
        # Conexiones (según facts.pl)
        self.add_connection('A', 'B', is_locked=True, key_needed='key1')  # Requiere key1
        self.add_connection('B', 'C', is_locked=True, puzzle_needed='puzzle1')  # Requiere puzzle1
        self.add_connection('C', 'D', is_locked=True, key_needed='key2')  # Requiere key2  
    
    def get_room(self, name):
        """
        Obtiene un objeto Room por su nombre.
        
        Args:
            name (str): Nombre de la habitación
            
        Returns:
            Room: Objeto de la habitación o None si no existe
        """
        return self.rooms.get(name)
    
    def visualize(self):
        """
        Muestra una representación visual simple del grafo.
        """
        print("\nRepresentación del Escape Room:")
        for room_name, room in self.rooms.items():
            connections = []
            for conn in room.connections:
                status = " (bloqueada)" if conn[2] else ""
                key_info = f" necesita {conn[3]}" if conn[2] else ""
                connections.append(f"{conn[0]}{status}{key_info}")
            
            key_info = f" | Contiene llave: {room.has_key}" if room.has_key else ""
            trap_info = " | ¡TRAMPA!" if room.is_trap else ""
            print(f"- {room_name}{key_info}{trap_info}")
            for conn in connections:
                print(f"  → {conn}")
        print(f"\nRestricciones: Máx {self.max_moves} movimientos, Máx {self.trap_threshold} visitas a {self.trap_room}")