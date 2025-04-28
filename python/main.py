import pygame
import sys
import os
from pygame.locals import *
import re
import tkinter as tk
from tkinter import filedialog
from integration.prolog_Bridge import PrologBridge
from components.Cinematic import Cinematic
from components.Button import Button

from components.Solver import Solver
from integration.Rules import Rules
from integration.Search import Search
from integration.SearchNoConstraints import SearchNoConstraints
from integration.State import StateManager
from integration.Contraints import Constraints
from integration.Facts import Facts
import json
import networkx as nx
import matplotlib.pyplot as plt
from matplotlib.backends.backend_agg import FigureCanvasAgg
import pygame
import io
import time  # Para controlar el tiempo de las imágenes
from assets.texts.constants import SCREEN_WIDTH, SCREEN_HEIGHT, BUTTON_WIDTH, BUTTON_HEIGHT, MARGIN, FONT_SIZE, SCROLL_SPEED
from assets.texts.colors import WHITE, BLACK, GRAY, LIGHT_GRAY, DARK_GRAY, RED, GREEN, BLUE, YELLOW, PURPLE

# Initialize Pygame
pygame.init()
pygame.mixer.init()

class EscapeRoomGUI:
    def __init__(self, bridge):
        self.cinematic = Cinematic(self.start_screen)
        self.bridge = bridge
        self.last_click_time = 0    # Para controlar dobles clics
        self.clear_all = False
        self.solver =  Solver(self.bridge, self.add_output)
        self.solver.set_help_callback(self.add_help_output)
        self.screen = pygame.display.set_mode((SCREEN_WIDTH, SCREEN_HEIGHT))
        pygame.display.set_caption("Dynamic Escape Room Game")
        self.show_map = False
        self.map = {}
        self.object_positions = {}  # Diccionario para guardar posiciones de objetos
        try:
            self.retro_font = pygame.font.Font(
                "retro_font.ttf", 24
            )  # Cambia "retro_font.ttf" por tu fuente
            self.retro_font_small = pygame.font.Font("retro_font.ttf", 18)
        except:
            print("No se pudo cargar la fuente retro, usando fuente por defecto")
            self.retro_font = pygame.font.SysFont("Arial", 24)
            self.retro_font_small = pygame.font.SysFont("Arial", 18)
        self.font = self.retro_font
        self.small_font = self.retro_font_small

        # Game state
        self.current_room = None
        self.inventory = []
        self.solved_puzzles = []
        self.doors = []
        self.objects = []
        self.keys = []
        self.pieces = []
        self.available_actions = []

        self.logo = None
        self.load_logo()
    
        # UI state
        self.output_text = []
        self.scroll_position = 0
        self.mode = "standard"
        self.game_initialized = False
        self.show_help = False
        self.background = None
        self.load_background()
        self.object_images = {}  # Diccionario para almacenar imágenes de objetos
        self.load_object_images()  # Cargar imágenes de objetos al inicializar
        self.door_images = {
            "locked": None,
            "unlocked": None
        }
        self.load_door_images()
        
    def load_door_images(self):
        """Cargar imágenes para puertas bloqueadas y desbloqueadas"""
        try:
            locked_path = os.path.join("assets", "images", "locked.png")
            unlocked_path = os.path.join("assets", "images", "unlocked.png")
            
            if os.path.exists(locked_path):
                img = pygame.image.load(locked_path).convert_alpha()
                self.door_images["locked"] = pygame.transform.scale(img, (60, 100))
            
            if os.path.exists(unlocked_path):
                img = pygame.image.load(unlocked_path).convert_alpha()
                self.door_images["unlocked"] = pygame.transform.scale(img, (60, 100))
        except Exception as e:
            print(f"Error cargando imágenes de puertas: {e}")
            # Crear placeholders si falla la carga
            self.door_images["locked"] = self.create_door_placeholder("BLOQUEADA", RED)
            self.door_images["unlocked"] = self.create_door_placeholder("ABIERTA", GREEN)
    
    def create_door_placeholder(self, text, color):
        """Crear una imagen de placeholder para puertas"""
        surface = pygame.Surface((60, 100), pygame.SRCALPHA)
        pygame.draw.rect(surface, color, (0, 0, 60, 100))
        pygame.draw.rect(surface, BLACK, (0, 0, 60, 100), 2)
        font = pygame.font.SysFont("Arial", 12)
        text_surf = font.render(text, True, BLACK)
        surface.blit(text_surf, (30 - text_surf.get_width()//2, 50 - text_surf.get_height()//2))
        return surface

    def load_object_images(self):
        """Cargar imágenes para los objetos disponibles"""
        object_image_files = {
            "painting": "painting.png",
            "cabinet": "cabinet.png",
            "box": "box.png",
            "bookshelf": "bookshelf.png",
            "rug": "rug.png",
            "puzzle": "puzzles.png"
        }
        
        for obj_name, filename in object_image_files.items():
            try:
                image_path = os.path.join("assets", "images", filename)
                if os.path.exists(image_path):
                    img = pygame.image.load(image_path).convert_alpha()
                    # Escalar imágenes a un tamaño consistente
                    self.object_images[obj_name] = pygame.transform.scale(img, (80, 80))
                else:
                    print(f"Imagen no encontrada: {image_path}")
            except Exception as e:
                print(f"Error cargando imagen {filename}: {e}")
                # Crear una imagen de placeholder si falla la carga
                placeholder = pygame.Surface((80, 80), pygame.SRCALPHA)
                pygame.draw.rect(placeholder, (255, 0, 255), (0, 0, 80, 80))
                pygame.draw.rect(placeholder, (0, 0, 0), (0, 0, 80, 80), 2)
                text = pygame.font.SysFont("Arial", 12).render(obj_name, True, (0, 0, 0))
                placeholder.blit(text, (40 - text.get_width() // 2, 40 - text.get_height() // 2))
                self.object_images[obj_name] = placeholder

    def get_object_image(self, obj_name):
        """Obtener la imagen para un objeto, o una aleatoria si no existe"""
        # Primero buscar coincidencia exacta
        if obj_name in self.object_images:
            return self.object_images[obj_name]
        
        # Buscar coincidencia parcial (por si el nombre tiene prefijo/sufijo)
        for key in self.object_images:
            if key in obj_name.lower() or obj_name.lower() in key:
                return self.object_images[key]
        
        # Si no se encuentra, devolver una imagen aleatoria
        import random
        if self.object_images:
            return random.choice(list(self.object_images.values()))
        
        # Si no hay imágenes cargadas, crear un placeholder
        placeholder = pygame.Surface((80, 80), pygame.SRCALPHA)
        pygame.draw.rect(placeholder, (255, 255, 0), (0, 0, 80, 80))
        pygame.draw.rect(placeholder, (0, 0, 0), (0, 0, 80, 80), 2)
        text = pygame.font.SysFont("Arial", 12).render(obj_name, True, (0, 0, 0))
        placeholder.blit(text, (40 - text.get_width() // 2, 40 - text.get_height() // 2))
        return placeholder
    
    def load_background(self):
        """Carga la imagen de fondo con manejo de errores"""
        try:
            background_path = "assets/images/background.png"
            if os.path.exists(background_path):
                self.background = pygame.image.load(background_path).convert()
                # Escalar la imagen al tamaño de la pantalla
                self.background = pygame.transform.scale(self.background, (SCREEN_WIDTH, SCREEN_HEIGHT))
            else:
                print(f"Fondo no encontrado en {background_path}")
        except Exception as e:
            print(f"Error cargando fondo: {e}")
            self.background = None


    def load_logo(self):
        """Carga el logo con manejo de errores"""
        try:
            logo_path = "assets/images/logo.png"  # Asegúrate de que la ruta es correcta
            if os.path.exists(logo_path):
                original_logo = pygame.image.load(logo_path)
                # Convertir para mejor rendimiento y soporte de transparencia
                self.logo = original_logo.convert_alpha()
                # Escalar manteniendo relación de aspecto
                logo_width = min(300, SCREEN_WIDTH - 400)  # Máximo 400px o el ancho de pantalla -100
                aspect_ratio = self.logo.get_width() / self.logo.get_height()
                logo_height = int(logo_width / aspect_ratio)
                self.logo = pygame.transform.scale(self.logo, (logo_width, logo_height))
            else:
                print(f"Logo no encontrado en {logo_path}")
        except Exception as e:
            print(f"Error cargando logo: {e}")
            self.logo = None


    def start_screen(self):
        """Show the initial game start screen"""
        self.clear_all = False
        self.game_initialized = False
        self.show_map = False
        self.clear_outputs()

        # Create buttons for game mode selection (sin fondo ni borde)
        self.buttons = [
            Button(
                SCREEN_WIDTH // 2 - BUTTON_WIDTH // 2,
                300,
                BUTTON_WIDTH,
                BUTTON_HEIGHT,
                "Modo estandar",
                self.start_standard_game,
                bg_color=None,  # Sin fondo
                text_color=WHITE,  # Texto blanco
                border_color=None,  # Sin borde
                font=self.retro_font
            ),
            Button(
                SCREEN_WIDTH // 2 - BUTTON_WIDTH // 2,
                360,
                BUTTON_WIDTH,
                BUTTON_HEIGHT,
                "Modo adversario",
                self.start_adversary_game,
                bg_color=None,
                text_color=WHITE,
                border_color=None,
                font=self.retro_font
            )
        ]

    def toggle_help(self):
        """Toggle help display and clear outputs when opening"""
        self.show_help = not self.show_help
        if self.show_help:
            self.clear_outputs()

    def start_standard_game(self):
        """Start game in standard mode"""
        self.mode = "standard"
        self.state = StateManager(self.bridge)
        self.facts = Facts(self.bridge)
        self.facts.set_game_mode("standard")
        self.initialize_game()

    def start_adversary_game(self):
        """Start game in adversary mode"""
        self.mode = "adversary"
        self.state = StateManager(self.bridge)
        self.facts = Facts(self.bridge)
        self.facts.set_game_mode("adversary")
        self.movement_mode_screen()

    def initialize_game(self):
        """Initialize the game"""
        self.clear_outputs()
        self.add_output("\nEscoger modo de juego:")
        self.buttons = [
            Button(
                SCREEN_WIDTH // 2 - BUTTON_WIDTH - MARGIN,
                200,
                BUTTON_WIDTH,
                BUTTON_HEIGHT,
                "Juego precargado",
                self.load_predefined_game,
            ),
            Button(
                SCREEN_WIDTH // 2 + MARGIN,
                200,
                BUTTON_WIDTH,
                BUTTON_HEIGHT,
                "Cargar juego",
                self.create_custom_game,
            ),
            Button(
                SCREEN_WIDTH // 2 - BUTTON_WIDTH // 2,
                300,
                BUTTON_WIDTH,
                BUTTON_HEIGHT,
                "Volver",
                self.start_screen,
            ),
        ]

    def load_predefined_game(self):
        """Load the predefined game"""
        if self.facts.load_predefined_game():
            self.constraints = Constraints(self.bridge)
            self.constraints.load_default_constraints()
            self.rules = Rules(self.bridge)
            if self.rules.initialize_game():
                self.game_initialized = True
                self.update_game_state()
                self.add_output("Predefined game loaded successfully!")
                self.map = self.facts.get_json_map()
                self.main_game_screen()
            else:
                self.add_output("Failed to initialize game rules.")
        else:
            self.add_output("Failed to load predefined game.")
            

    def create_custom_game(self):
        """Open a file dialog to load a custom game config JSON (for Pygame)"""
        # Usamos Tkinter solo para el diálogo de archivos (no se muestra ventana)
        root = tk.Tk()
        root.withdraw()

        self.add_output("Abriendo selector de archivo JSON...")

        file_path = filedialog.askopenfilename(
            title="Seleccionar configuración del juego",
            filetypes=[("Archivos JSON", "*.json")],
        )

        if not file_path:
            self.add_output("Carga cancelada por el usuario.")
            return

        try:
            with open(file_path, "r") as f:
                game_config = json.load(f)

            # Asegurar que el modo esté presente
            game_config["game_mode"] = self.mode

            if self.mode == "adversary":
                game_config.setdefault("guard_location", "b")
                game_config.setdefault("guard_movement_type", "predictive")

            if self.facts.create_custom_game(game_config):
                self.constraints = Constraints(self.bridge)
                self.constraints.create_custom_constraints(
                    max_moves=50,
                    max_b_visits=3,
                    key_limit=2,
                    piece_limit=5,
                    traps=[("b", 3)] if self.mode == "adversary" else None,
                )
                self.rules = Rules(self.bridge)
                if self.rules.initialize_game():
                    self.game_initialized = True
                    self.update_game_state()
                    self.add_output("Juego personalizado cargado exitosamente.")
                    self.map = self.facts.get_json_map()
                    self.main_game_screen()
                else:
                    self.add_output("Error al inicializar las reglas del juego.")
            else:
                self.add_output("Error al crear el juego personalizado.")
        except Exception as e:
            self.add_output(f"Error al cargar el archivo JSON: {e}")

    def set_guard_movement(self, option):
        self.facts.set_guard_movement_type(option)
        self.initialize_game()

    def movement_mode_screen(self):
        """Guard movement screen"""
        self.clear_outputs()
        self.add_output("Movimiento del guardia:")
        self.buttons = [
            Button(
                SCREEN_WIDTH // 2 - BUTTON_WIDTH // 2,
                200,
                BUTTON_WIDTH,
                BUTTON_HEIGHT,
                "Predictivo",
                lambda: self.set_guard_movement(1),
            ),
            Button(
                SCREEN_WIDTH // 2 - BUTTON_WIDTH // 2,
                260,
                BUTTON_WIDTH,
                BUTTON_HEIGHT,
                "Aleatorio",
                lambda: self.set_guard_movement(2),
            ),
        ]

    def main_game_screen(self):
        """Main game screen with all actions"""
        self.clear_outputs()
        self.buttons = [
            Button(
                SCREEN_WIDTH - BUTTON_WIDTH - MARGIN,
                SCREEN_HEIGHT - BUTTON_HEIGHT - MARGIN,
                BUTTON_WIDTH,
                BUTTON_HEIGHT,
                "New Game",
                self.start_screen,
            ),
        ]

        # Add room-specific buttons
        self.update_action_buttons()

    def draw_map(self):
        """Dibuja el mapa pequeño en la esquina superior derecha con fondo negro transparente"""
        if not hasattr(self, "map") or not self.map:
            return

        try:
            import networkx as nx
            import matplotlib.pyplot as plt
            import io
            from PIL import Image
        except ImportError:
            print("Advertencia: NetworkX/Matplotlib no instalados. No se dibujará el mapa.")
            return

        # Obtener la habitación final (salida)
        final_room = self.facts.get_final_room()
        
        # Marcar la habitación actual y la final
        for node in self.map["nodes"]:
            node["is_current"] = node["id"] == self.state.player_location()
            node["is_final"] = node["id"] == final_room

        # Crear el grafo
        G = nx.DiGraph()

        # Añadir nodos y aristas
        for node in self.map["nodes"]:
            G.add_node(node["id"], **node)
        for link in self.map["links"]:
            G.add_edge(link["source"], link["target"], **link)

        # Configurar la figura con fondo transparente
        fig, ax = plt.subplots(figsize=(4, 3), facecolor='none')
        pos = nx.spring_layout(G, seed=42)

        # Personalizar nodos
        node_colors = []
        for node in G.nodes():
            if G.nodes[node].get("is_current", False):
                node_colors.append("#4cc9f0")  # Habitación actual - azul claro
            elif G.nodes[node].get("is_final", False):
                node_colors.append("#ff0000")  # Habitación final - rojo brillante
            elif G.nodes[node].get("has_guard", False):
                node_colors.append("#f72585")  # Con guardia - rosa
            else:
                node_colors.append("#4361ee")  # Normal - azul

        # Dibujar el grafo
        nx.draw_networkx_nodes(
            G, pos, 
            node_color=node_colors, 
            node_size=400,
            ax=ax,
            edgecolors='white',
            linewidths=1
        )
        
        # Etiquetas con estilo especial para la habitación final
        node_labels = {}
        for node in G.nodes():
            if G.nodes[node].get("is_final", False):
                node_labels[node] = f"{node}\n(SALIDA)"
            else:
                node_labels[node] = node
        
        nx.draw_networkx_labels(
            G, pos, 
            labels=node_labels,
            font_color='white', 
            font_size=8,
            ax=ax
        )

        # Personalizar aristas
        edge_colors = []
        for u, v in G.edges():
            if G.edges[u, v].get("state", "") == "locked":
                edge_colors.append("#ff9e00")  # Cerrada - naranja
            else:
                edge_colors.append("#4ad66d")  # Abierta - verde

        nx.draw_networkx_edges(
            G, pos, 
            edge_color=edge_colors, 
            width=1.5,
            arrows=True, 
            arrowsize=8,
            ax=ax
        )

        # Configurar el fondo del gráfico como transparente
        ax.set_facecolor('none')
        fig.patch.set_alpha(0)
        ax.axis('off')
        plt.tight_layout()

        # Convertir a imagen compatible con Pygame
        buf = io.BytesIO()
        plt.savefig(
            buf, 
            format="png", 
            bbox_inches='tight', 
            pad_inches=0, 
            dpi=100,
            transparent=True
        )
        buf.seek(0)

        # Usar PIL para cargar la imagen y luego convertir a Pygame
        img = Image.open(buf)
        img = img.resize((250, 200))
        
        # Crear superficie con alpha para el fondo oscuro semitransparente
        map_surface = pygame.Surface((250, 200), pygame.SRCALPHA)
        map_surface.fill((0, 0, 0, 180))  # Negro con 70% de opacidad
        
        # Pegar el mapa en la superficie
        map_data = img.tobytes()
        pygame_img = pygame.image.fromstring(map_data, img.size, img.mode)
        map_surface.blit(pygame_img, (0, 0))

        # Dibujar leyenda simple
        font = pygame.font.SysFont("Arial", 12)

        # Dibujar en la esquina superior derecha de la pantalla
        self.screen.blit(map_surface, (SCREEN_WIDTH - 260, 20))

        # Limpiar
        plt.close(fig)
        buf.close()

    def safe_pick_key(self, key):
        """Wrapper para pick_key que evita doble llamada"""
        current_time = time.time()
        if current_time - self.last_click_time > 0.3:  # 300ms de debounce
            self.last_click_time = current_time
            self.pick_key(key)
           


    def safe_pick_piece(self, piece):
        """Wrapper para pick_piece que evita doble llamada"""
        current_time = time.time()
        if current_time - self.last_click_time > 0.3:  # 300ms de debounce
            self.last_click_time = current_time
            self.pick_piece(piece)


    def load_image(self, filename, size):
        """Load and scale an image"""
        try:
            image_path = os.path.join("assets", "images", filename)
            if os.path.exists(image_path):
                img = pygame.image.load(image_path).convert_alpha()
                return pygame.transform.scale(img, size)
        except Exception as e:
            print(f"Error loading image {filename}: {e}")
        # Return a placeholder surface if image can't be loaded
        surface = pygame.Surface(size, pygame.SRCALPHA)
        pygame.draw.rect(surface, (255, 0, 0), (0, 0, size[0], size[1]))
        return surface
    
    def update_action_buttons(self):
        """Update buttons for current room actions"""
        # Limpiar botones existentes (excepto los principales como "New Game")
        if self.clear_all:
            self.buttons = [            
                Button(
                SCREEN_WIDTH - BUTTON_WIDTH - MARGIN,
                SCREEN_HEIGHT - BUTTON_HEIGHT - MARGIN,
                BUTTON_WIDTH,
                BUTTON_HEIGHT,
                "New Game",
                self.start_screen,
            ),]
            return
        
        self.buttons = [
            btn for btn in self.buttons if not isinstance(btn, ImageButton) 
        ]
        self.buttons = [
            btn for btn in self.buttons if isinstance(btn, Button) if btn.text == "New Game"
        ]  

        # Cargar imágenes
        key_img = self.load_image("key.png", (40, 40))
        piece_img = self.load_image("puzzle_piece.png", (40, 40))
        puzzle_img = self.load_image("puzzles.png", (60, 60))  # Imagen para resolver puzzles

        # Obtener acciones disponibles e items recolectados
        self.available_actions = self.state.get_available_actions()
        collected_items = self.state.get_collected_items()

        # Filtrar objetos disponibles - solo mostrar los que no están recolectados
        available_pick_keys = set()
        available_pick_pieces = set()
        available_objects = set()
        available_solve_puzzles = set()


        for action in self.available_actions:
            action_type, action_target = action.split(":")
            if action_type == "pick_key" and action_target not in collected_items["keys"]:
                available_pick_keys.add(action_target)
            elif action_type == "pick_piece" and action_target not in collected_items["pieces"]:
                available_pick_pieces.add(action_target)
            elif action_type == "move_object":
                available_objects.add(action_target)
            elif action_type == "solve_puzzle":
                available_solve_puzzles.add(action_target)

        # Filtrar llaves, piezas y objetos disponibles
        filtered_keys = [k for k in self.keys if k in available_pick_keys]
        filtered_pieces = [(p, puz) for (p, puz) in self.pieces if p in available_pick_pieces]
        filtered_objects = [obj for obj in self.objects if obj in available_objects]
        filtered_puzzles = [puz for puz in available_solve_puzzles]

        # Ca    lcular posiciones para todos los elementos
        all_items = filtered_keys + [p[0] for p in filtered_pieces] + filtered_objects + filtered_puzzles
        perimeter_positions = self.calculate_perimeter_positions(len(all_items))

        # Asignar posiciones
        self.update_object_positions(filtered_keys, filtered_pieces, filtered_objects, filtered_puzzles)

        # Agregar botones de llaves (solo las no recolectadas)
        for key in filtered_keys:
            pos = self.object_positions.get(key, (0, 0))
            if not any(isinstance(btn, ImageButton) and getattr(btn, 'tooltip', '').endswith(key) for btn in self.buttons):
                self.buttons.append(ImageButton(
                    pos[0], pos[1], key_img, 
                    lambda k=key: self.safe_pick_key(k),
                    tooltip=f"Recoger llave {key}"
                ))


        # Agregar botones de piezas
        offset = len(filtered_keys)
        for i, (piece, puzzle) in enumerate(filtered_pieces):
            pos = self.object_positions.get(piece, perimeter_positions[offset + i] if offset + i < len(perimeter_positions) else (0, 0))
            self.buttons.append(ImageButton(
                pos[0], pos[1], piece_img,
                lambda p=piece: self.safe_pick_piece(p),
                tooltip=f"Recoger pieza {piece} (Puzzle {puzzle})"
            ))

        # Agregar botones de objetos
        offset += len(filtered_pieces)
        for i, obj in enumerate(filtered_objects):
            pos = self.object_positions.get(obj, perimeter_positions[offset + i] if offset + i < len(perimeter_positions) else (0, 0))
            obj_img = self.get_object_image(obj)
            self.buttons.append(ImageButton(
                pos[0], pos[1], obj_img,
                lambda o=obj: self.move_object(o),
                tooltip=f"Interactuar con {obj}"
            ))

        # Agregar botones de puzzles
        offset += len(filtered_objects)
        for i, puzzle in enumerate(filtered_puzzles):
            pos = self.object_positions.get(puzzle, perimeter_positions[offset + i] if offset + i < len(perimeter_positions) else (0, 0))
            self.buttons.append(ImageButton(
                pos[0], pos[1], puzzle_img,
                lambda p=puzzle: self.solve_puzzle(p),
                tooltip=f"Resolver puzzle {puzzle}"
            ))

        # Obtener puertas disponibles
        available_doors = []
        for action in self.available_actions:
            action_type, action_target = action.split(":")
            if action_type == "unlock_door":
                from_room = self.state.player_location()
                to_room = action_target
                available_doors.append((from_room, to_room))
            elif action_type == "move_to":
                # También considerar puertas ya desbloqueadas como movimientos
                for door in self.doors:
                    if (door["from"] == self.current_room and door["to"] == action_target) or \
                       (door["from"] == action_target and door["to"] == self.current_room):
                        if door["state"] == "unlocked":
                            available_doors.append((self.current_room, action_target))

        # Calcular posiciones de puertas
        self.update_door_positions(available_doors)
        
        # Añadir botones de puertas
        for from_room, to_room in available_doors:
            pos = self.door_positions.get((from_room, to_room), (0, 0))
            
            # Determinar estado de la puerta
            door_state = "locked"
            for door in self.doors:
                if (door["from"] == from_room and door["to"] == to_room) or \
                   (door["from"] == to_room and door["to"] == from_room):
                    door_state = door["state"]
                    break
            
            # Determinar acción según estado
            if door_state == "locked":
                action = lambda f=from_room, t=to_room: self.handle_door_action(f, t)
                tooltip = f"Puerta {from_room}-{to_room} (Bloqueada - Click para desbloquear)"
            else:
                action = lambda f=from_room, t=to_room: self.move_player(t)
                tooltip = f"Puerta {from_room}-{to_room} (Abierta - Click para pasar)"
            
            door_img = self.door_images[door_state]
            
            self.buttons.append(ImageButton(
                pos[0], pos[1], door_img,
                action,
                tooltip=tooltip
            ))


    def update_object_positions(self, keys, pieces, objects, puzzles=None):
        """Actualiza o asigna posiciones a los objetos manteniendo las existentes"""
        puzzles = puzzles or []
        all_objects = keys + [p[0] for p in pieces] + objects + puzzles
        
        # Limpiar posiciones de objetos que ya no están
        for obj in list(self.object_positions.keys()):
            if obj not in all_objects:
                del self.object_positions[obj]
        
        # Asignar nuevas posiciones solo a objetos no recolectados
        perimeter_positions = self.calculate_perimeter_positions(len(all_objects))
        
        # Asignar posiciones en orden
        for i, obj in enumerate(all_objects):
            if obj not in self.object_positions:
                self.object_positions[obj] = perimeter_positions[i]

    def handle_door_action(self, from_room, to_room):
        """Manejar acción de puerta (desbloquear o pasar)"""
        # Primero intentar desbloquear si está bloqueada
        if not self.unlock_door(from_room, to_room):
            # Si no se pudo desbloquear, intentar mover si ya está desbloqueada
            for door in self.doors:
                if (door["from"] == from_room and door["to"] == to_room) or \
                   (door["from"] == to_room and door["to"] == from_room):
                    if door["state"] == "unlocked":
                        self.move_player(to_room)
                    break

    def update_door_positions(self, doors):
        """Coloca todas las puertas en una sola fila horizontal"""
        if not hasattr(self, 'door_positions'):
            self.door_positions = {}
            
        new_positions = {}
        
        if not doors:
            self.door_positions = new_positions
            return

        # Definir el área donde se pueden poner las puertas
        margin_x = 100  # espacio a los lados
        y_position = 450  # altura fija donde estarán todas las puertas
        available_width = SCREEN_WIDTH - 2 * margin_x

        step = available_width // (len(doors) + 1)  # espacio entre puertas

        for i, door in enumerate(doors):
            x = margin_x + (i + 1) * step
            new_positions[door] = (x, y_position)

        # Mantener posiciones anteriores si existe la puerta
        for door, pos in self.door_positions.items():
            if door in doors:
                new_positions[door] = pos

        self.door_positions = new_positions


    def unlock_door(self, from_room, to_room):
        """Desbloquear puerta de forma segura (con debounce de 300ms)"""
        current_time = time.time()
        if current_time - getattr(self, 'last_unlock_time', 0) > 0.3:  # 300ms de espera
            self.last_unlock_time = current_time
            if self.state.unlock_door(from_room, to_room):
                self.add_output(f"\n¡Puerta entre {from_room} y {to_room} desbloqueada!")
                self.update_game_state()
                return True
        return False


    def calculate_perimeter_positions(self, num_items):
        """Calculate non-overlapping positions around the room perimeter with configurable corners"""
        positions = []
        margin = 170  # Distance from screen edges
        spacing = 60  # Minimum space between items

        # Define the four edges with their movement logic
        edges = [
            {
                "name": "top",
                "start": (margin, 500),
                "direction": (1, 0),  # Move right
                "length": SCREEN_WIDTH - 2 * margin
            },
            {
                "name": "right",
                "start": (SCREEN_WIDTH - margin, margin),
                "direction": (0, 1),  # Move down
                "length": SCREEN_HEIGHT - 2 * margin
            },
            {
                "name": "bottom",
                "start": (SCREEN_WIDTH - margin, SCREEN_HEIGHT - margin),
                "direction": (-1, 0),  # Move left
                "length": SCREEN_WIDTH - 2 * margin
            },
            {
                "name": "left",
                "start": (margin, SCREEN_HEIGHT - margin),
                "direction": (0, -1),  # Move up
                "length": SCREEN_HEIGHT - 2 * margin
            }
        ]

        for edge in edges:
            if num_items <= 0:
                break

            max_items = max(0, min(num_items, edge["length"] // spacing))
            if max_items == 0:
                continue

            dx, dy = edge["direction"]
            step = edge["length"] // max(1, max_items - 1)

            for i in range(max_items):
                x = edge["start"][0] + i * step * dx
                y = edge["start"][1] + i * step * dy
                positions.append((x, y))

            num_items -= max_items

        return positions


    def handle_action(self, action_str):
        """Handle a game action"""
        action_type, action_target = action_str.split(":")
        if action_type == "move_to":
            self.move_player(action_target)
        elif action_type == "pick_key":
            self.pick_key(action_target)
        elif action_type == "pick_piece":
            self.pick_piece(action_target)
        elif action_type == "move_object":
            self.move_object(action_target)
        elif action_type == "solve_puzzle":
            self.solve_puzzle(action_target)
        elif action_type == "unlock_door":
            # For unlock, we need from_room and to_room
            current_room = self.state.player_location()
            self.unlock_door(current_room, action_target)
        self.map = self.facts.get_json_map()

        # Update game state after action
        self.update_game_state()

    def update_game_state(self):
        """Update the current game state"""
        state = self.state.get_current_state()

        if state:
            self.current_room = state["room"]
            self.inventory = state["inventory"]
            self.solved_puzzles = state["solved_puzzles"]
            self.doors = state["doors"]

            print(self.facts.get_objects())
            # Get objects in current room
            self.objects = [
                obj["Object"]
                for obj in self.facts.get_objects()
                if obj["Room"] == self.current_room
            ]

            # Get keys in current room (both original and dropped)
            original_keys = [
                key["Key"]
                for key in self.facts.get_keys()
                if key["Room"] == self.current_room
            ]
            dropped_keys = [
                key["Key"]
                for key in state["dropped_keys"]
                if key["R"] == self.current_room
            ]
            self.keys = list(set(original_keys + dropped_keys))

            # Get pieces in current room (both original and dropped)
            original_pieces = [
                (piece["Piece"], piece["Puzzle"])
                for piece in self.facts.get_piece_locations()
                if piece["Room"] == self.current_room
            ]
            dropped_pieces = [
                (piece["Piece"], piece["Puzzle"])
                for piece in state["dropped_pieces"]
                if piece["R"] == self.current_room
            ]
            self.pieces = list(set(original_pieces + dropped_pieces))

        # Check win condition
        if self.state.check_win_condition():
            self.game_over_screen(True)

        # Update action buttons
        self.update_action_buttons()

    def clear_outputs(self):
        """Clear all text from the output display"""
        self.output_text = []
        self.scroll_position = 0

    def add_output(self, text, center=False, indent=0):
        """Add text to the output display, optionally centered or indented"""
        self.clear_outputs()
        max_chars = (SCREEN_WIDTH - 2 * MARGIN) // (FONT_SIZE // 2)
        lines = []

        if len(text) > max_chars:
            words = text.split()
            current_line = []
            current_length = 0

            for word in words:
                if current_length + len(word) + 1 <= max_chars:
                    current_line.append(word)
                    current_length += len(word) + 1
                else:
                    line = " ".join(current_line)
                    if center:
                        line = line.center(max_chars)
                    elif indent > 0:
                        line = " " * indent + line
                    lines.append(line)
                    current_line = [word]
                    current_length = len(word)

            if current_line:
                line = " ".join(current_line)
                if center:
                    line = line.center(max_chars)
                elif indent > 0:
                    line = " " * indent + line
                lines.append(line)
        else:
            line = text
            if center:
                line = line.center(max_chars)
            elif indent > 0:
                line = " " * indent + line
            lines.append(line)

        self.output_text.extend(lines)

        # Auto-scroll to bottom
        self.scroll_position = max(
            0,
            len(self.output_text)
            - (SCREEN_HEIGHT // (FONT_SIZE + 4)) * (FONT_SIZE + 4),
        )

    def show_inventory(self):
        """Show player inventory"""
        self.add_output("\nYour inventory:")
        if not self.inventory:
            self.add_output("  Empty")
        else:
            for item in self.inventory:
                self.add_output(f"- {item}")

        # Show key count
        key_limit = self.constraints.can_carry("key")
        key_count = self.constraints.count_items_of_type("key")
        self.add_output(f"\nKeys: {key_count}/{key_limit}")

        # Show puzzle pieces
        self.add_output("\nCollected puzzle pieces:")
        puzzles = self.facts.get_puzzles()
        if not puzzles:
            self.add_output("  None")
        else:
            for puzzle in puzzles:
                puzzle = puzzle["Puzzle"]
                pieces = list(
                    self.bridge.prolog.query(f"state:has_piece({puzzle}, Piece)")
                )
                piece_names = [p["Piece"] for p in pieces]
                self.add_output(f"Puzzle {puzzle}: {piece_names}")

        # Show piece count
        piece_limit = self.constraints.can_carry("piece")
        piece_count = self.constraints.count_items_of_type("piece")
        self.add_output(f"\nPuzzle pieces: {piece_count}/{piece_limit}")

        # Show solved puzzles
        self.add_output("\nSolved puzzles:")
        if not self.solved_puzzles:
            self.add_output("  None")
        else:
            for puzzle in self.solved_puzzles:
                self.add_output(f"- {puzzle}")

    def show_game_stats(self):
        """Show game statistics"""
        self.add_output("\nGame Statistics:")

        # Constraints
        max_moves = self.constraints.max_moves()
        moves_used = self.constraints.move_count()
        remaining_moves = max_moves - moves_used
        self.add_output(
            f"Moves: {moves_used}/{max_moves} ({remaining_moves} remaining)"
        )

        max_b_visits = self.constraints.max_b_visits()
        if max_b_visits:
            b_visits = list(self.bridge.prolog.query("constraints:b_visits(X)"))
            b_visits = b_visits[0]["X"] if b_visits else 0
            remaining_b_visits = max_b_visits - b_visits
            self.add_output(
                f"Room B visits: {b_visits}/{max_b_visits} ({remaining_b_visits} remaining)"
            )

        # Inventory limits
        key_limit = self.constraints.can_carry("key")
        piece_limit = self.constraints.can_carry("piece")
        self.add_output(f"\nInventory Limits:")
        self.add_output(f"- Keys: {key_limit}")
        self.add_output(f"- Puzzle pieces: {piece_limit}")

        # Room traps
        traps = list(self.bridge.prolog.query("constraints:trap(Room, Turns)"))
        if traps:
            self.add_output("\nRoom Traps:")
            for trap in traps:
                room = trap["Room"]
                turns = trap["Turns"]
                current_turns = self.constraints.turns_in_room(room)
                remaining = turns - current_turns
                self.add_output(
                    f"- Room {room}: activates after {turns} turns ({remaining} remaining)"
                )

    def move_player(self, new_room):
        """Mover jugador de forma segura (con debounce de 300ms)"""
        current_time = time.time()
        if current_time - getattr(self, 'last_move_time', 0) > 0.3:  # 300ms de espera
            self.last_move_time = current_time

            if self.state.move_player(new_room):
                # Check if new room has trap
                if not self.constraints.check_trap(new_room):
                    self.game_over_screen(False, f"Activaste una trampa en el piso {new_room}")
                    return

                # Check move limit
                if not self.constraints.check_move_limit():
                    self.game_over_screen(False, "Excediste el límite de movimientos")
                    return

                if self.facts.get_game_mode() == "adversary":
                    if self.state.player_location() == self.state.guard_location():
                        self.game_over_screen(False, "El guardia esta contigo")
                        return

                self.add_output(f"\nMoved to room {new_room}.")
                self.update_game_state()
            else:
                self.add_output(f"\nCannot move to room {new_room}.")
        self.map = self.facts.get_json_map()



    def pick_key(self, key):
        """Pick up a key"""
        if self.constraints.check_inventory_limit("key"):
            if self.state.pick_key(key):
                self.add_output(f"\nPicked up key: {key}")
                # Actualizar botones inmediatamente
                self.update_action_buttons()
                # Forzar redibujado
                pygame.display.flip()
            else:
                self.add_output(f"\nCannot pick up key: {key}")
        else:
            self.add_output(
                f"\nCannot carry more keys (limit: {self.constraints.can_carry('key')})"
            )

    def pick_piece(self, piece):
        """Pick up a puzzle piece"""
        if self.constraints.check_inventory_limit("piece"):
            if self.state.pick_piece(piece):
                self.add_output(f"\nPicked up puzzle piece: {piece}")
                # Actualizar botones inmediatamente
                self.update_action_buttons()
                # Forzar redibujado
                pygame.display.flip()
            else:
                self.add_output(f"\nCannot pick up piece: {piece}")
        else:
            self.add_output(
                f"\nCannot carry more puzzle pieces (limit: {self.constraints.can_carry('piece')})"
            )

    def move_object(self, obj):
        """Examine/move an object"""
        if self.state.move_object(obj):
            self.add_output(f"\nExamined object: {obj}")
            # Check if object hides a puzzle piece
            hidden_piece = list(
                self.bridge.prolog.query(f"facts:hide_piece({obj}, Puzzle, Piece)")
            )
            print("hidden", hidden_piece)
            if hidden_piece:
                piece = hidden_piece[0]["Piece"]
                puzzle = hidden_piece[0]["Puzzle"]
                self.add_output(
                    f"Found hidden puzzle piece: {piece} (of puzzle {puzzle})"
                )
                self.update_action_buttons()
                # Forzar redibujado
                pygame.display.flip()
        else:
            self.add_output(f"\nCannot examine object: {obj}")

    def solve_puzzle(self, puzzle):
        """Solve a puzzle"""
        if self.state.solve_puzzle(puzzle):
            self.add_output(f"\nSolved puzzle: {puzzle}")
        else:
            self.add_output(f"\nCannot solve puzzle: {puzzle}")

    def unlock_door(self, from_room, to_room):
        """Unlock a door"""
        print("fron", from_room)
        print("tu", to_room)
        if self.state.unlock_door(from_room, to_room):
            self.add_output(f"\nUnlocked door between {from_room} and {to_room}")
        else:
            self.add_output(f"\nCannot unlock door between {from_room} and {to_room}")
        self.update_game_state()


    def game_over_screen(self, won, reason=None):
        """Muestra la pantalla de fin de juego"""
        self.available_actions = []
        self.clear_outputs()
        self.clear_all = True
        self.update_action_buttons()
        self.game_initialized = False
        
        if won:
            self.add_output("¡FELICIDADES!", center=True)
            self.add_output("Has escapado exitosamente", center=True)
            self.add_output("¡GANASTE EL JUEGO!", center=True)
        else:
            self.add_output("GAME OVER", center=True)
            self.add_output("No lograste escapar", center=True)
            if reason:
                self.add_output(f"{reason}", center=True)
        
        # Botones

        
        # Reproducir sonido de victoria/derrota
        try:
            if won:
                pygame.mixer.Sound("win_sound.wav").play()
            else:
                pygame.mixer.Sound("lose_sound.wav").play()
        except:
            pass  # Si no hay sonidos, continuar sin ellos

    def run(self):
        """Main game loop"""
        clock = pygame.time.Clock()
        running = True

        while running:
            for event in pygame.event.get():
                if event.type == QUIT:
                    running = False
                elif event.type == KEYDOWN:
                    if not self.cinematic.intro_completed and event.key == K_SPACE:
                        self.cinematic.skip_intro()
                    elif event.key == K_UP:
                        self.scroll_position = max(
                            0, self.scroll_position - SCROLL_SPEED
                        )
                    elif event.key == K_DOWN:
                        max_scroll = max(
                            0,
                            len(self.output_text) * (FONT_SIZE + 4)
                            - SCREEN_HEIGHT
                            + 100,
                        )
                        self.scroll_position = min(
                            max_scroll, self.scroll_position + SCROLL_SPEED
                        )
                    elif event.key == K_h:
                        self.toggle_help()
                    elif event.key == K_m:
                        self.show_map = not self.show_map
                elif event.type == MOUSEBUTTONDOWN:
                    if not self.cinematic.intro_completed:
                        self.cinematic.skip_intro()
                    elif event.button == 4:
                        self.scroll_position = max(
                            0, self.scroll_position - SCROLL_SPEED
                        )
                    elif event.button == 5:
                        max_scroll = max(
                            0,
                            len(self.output_text) * (FONT_SIZE + 4)
                            - SCREEN_HEIGHT
                            + 100,
                        )
                        self.scroll_position = min(
                            max_scroll, self.scroll_position + SCROLL_SPEED
                        )
                    else:
                        if not self.cinematic.intro_completed:
                            self.cinematic.skip_intro()
                        else:
                            for button in self.buttons:
                                if button.is_clicked(event.pos):
                                    button.action()

            # Actualizar lógica del juego o cinemática
            if not self.cinematic.intro_completed:
                self.cinematic.update_intro()

            # Dibujar todo
            self.screen.fill(BLACK)

            if not self.cinematic.intro_completed:
                self.cinematic.draw_intro()
            else:
                if self.game_initialized and self.background:
                    self.screen.blit(self.background, (0, 0))
                # Dibujar texto del juego
                if not self.game_initialized and len(self.output_text) == 0:
                    # Dibujar logo o título alternativo
                    if self.logo:
                        logo_rect = self.logo.get_rect(center=(SCREEN_WIDTH//2, 150))
                        self.screen.blit(self.logo, logo_rect)
                    else:
                        title = self.large_font.render("ESCAPE ROOM GAME", True, WHITE)
                        title_rect = title.get_rect(center=(SCREEN_WIDTH//2, 150))
                        self.screen.blit(title, title_rect)
     
                else:
                    # Dibujar texto del juego normal
                    y_pos = MARGIN - self.scroll_position
                    for i, line in enumerate(self.output_text):
                        if MARGIN <= y_pos <= SCREEN_HEIGHT - MARGIN:
                            text_surface = self.font.render(line, True, WHITE)
                            self.screen.blit(text_surface, (MARGIN, y_pos))
                        y_pos += FONT_SIZE + 4

                # Dibujar mapa si está activado
                if hasattr(self, "show_map") and self.show_map:
                    self.draw_map()

                # Dibujar botones
                for button in self.buttons:
                    button.draw(self.screen)

                # Dibujar ayuda si se muestra
                if self.show_help:
                    self.draw_help()

            pygame.display.flip()
            clock.tick(30)

        pygame.quit()
        sys.exit()

    def draw_help(self):
        """Draw help overlay with solution buttons and results, organizing text in columns if needed"""
        # Semi-transparent overlay
        s = pygame.Surface((SCREEN_WIDTH, SCREEN_HEIGHT), pygame.SRCALPHA)
        s.fill((0, 0, 0, 180))
        self.screen.blit(s, (0, 0))

        # Help box dimensions
        help_width = SCREEN_WIDTH - 200
        help_height = SCREEN_HEIGHT - 200
        pygame.draw.rect(self.screen, WHITE, (100, 100, help_width, help_height))
        pygame.draw.rect(self.screen, BLUE, (100, 100, help_width, help_height), 3)

        # Title
        title = self.font.render("HELP - SOLUTION FINDERS", True, BLACK)
        self.screen.blit(title, (help_width//2 - title.get_width()//2 + 100, 120))

        # Solution buttons in a row
        solution_buttons = [
            Button(
                help_width//4 + 80,
                180,
                BUTTON_WIDTH - 40,
                BUTTON_HEIGHT - 10,
                "BFS Solution",
                lambda: self.solver.find_solution(show_in_help=True),
                bg_color=GREEN,
                text_color=BLACK
            ),
            Button(
                help_width//2 + 80,
                180,
                BUTTON_WIDTH - 40,
                BUTTON_HEIGHT - 10,
                "BFS No Cons",
                lambda: self.solver.find_solution_no(show_in_help=True),
                bg_color=YELLOW,
                text_color=BLACK
            ),
            Button(
                3*help_width//4 + 80,
                180,
                BUTTON_WIDTH - 40,
                BUTTON_HEIGHT - 10,
                "A* Solution",
                lambda: self.solver.find_solution_start(show_in_help=True),
                bg_color=RED,
                text_color=WHITE
            )
        ]

        # Draw buttons
        mouse_pos = pygame.mouse.get_pos()
        mouse_clicked = pygame.mouse.get_pressed()[0]
        
        for button in solution_buttons:
            button.draw(self.screen)
            if mouse_clicked and button.is_clicked(mouse_pos):
                button.action()

        # Solution display area
        solution_box_y = 230
        solution_box_height = help_height - 130
        pygame.draw.rect(
            self.screen, 
            LIGHT_GRAY, 
            (120, solution_box_y, help_width-40, solution_box_height)
        )
        pygame.draw.rect(
            self.screen, 
            DARK_GRAY, 
            (120, solution_box_y, help_width-40, solution_box_height), 
            2
        )

        # Draw solution text if available
        if hasattr(self, 'help_solution_text') and self.help_solution_text:
            # Calculate how many lines fit in the box
            line_height = FONT_SIZE - 2
            max_lines = solution_box_height // line_height
            
            # Calculate how many columns we need
            total_lines = len(self.help_solution_text)
            columns_needed = (total_lines + max_lines - 1) // max_lines
            
            # Calculate column width (with minimum width)
            column_width = max(200, (help_width - 60) // columns_needed)
            
            # Draw text in columns
            current_column = 0
            current_line = 0
            
            for i, line in enumerate(self.help_solution_text):
                if i >= (current_column + 1) * max_lines:
                    current_column += 1
                    current_line = 0
                
                x_pos = 130 + (current_column * column_width)
                y_pos = solution_box_y + 10 + (current_line * line_height)
                
                if y_pos + line_height <= solution_box_y + solution_box_height:
                    text_surface = self.small_font.render(line, True, BLACK)
                    self.screen.blit(text_surface, (x_pos, y_pos))
                
                current_line += 1

    def add_help_output(self, text):
        """Add text to the help modal output"""
        if not hasattr(self, 'help_solution_text'):
            self.help_solution_text = []
        
        # Split long text into multiple lines
        max_chars = (SCREEN_WIDTH - 250) // (FONT_SIZE // 2)
        if len(text) > max_chars:
            words = text.split()
            current_line = ""
            for word in words:
                if len(current_line) + len(word) + 1 <= max_chars:
                    current_line += " " + word
                else:
                    self.help_solution_text.append(current_line.strip())
                    current_line = word
            if current_line:
                self.help_solution_text.append(current_line.strip())
        else:
            self.help_solution_text.append(text)
    def show_solution_in_help(self, algorithm_type):
        """Calculate and display solution within the help modal"""
        # Clear previous solution
        self.help_solution_text = ["Calculating solution..."]
        
        # Force immediate redraw
        pygame.display.flip()
        
        # Calculate solution based on algorithm type
        if algorithm_type == "BFS":
            solution = self.solver.find_solution()
        elif algorithm_type == "BFS No Constraints":
            solution = self.solver.find_solution_no()
        elif algorithm_type == "A*":
            solution = self.solver.find_solution_start()
        
        # Process solution text
        if solution:
            self.help_solution_text = [
                f"{algorithm_type} Solution Found:",
                "-----------------------------"
            ]
            if isinstance(solution, list):
                self.help_solution_text.extend(solution)
            else:
                self.help_solution_text.append(str(solution))
        else:
            self.help_solution_text = ["No solution found with current constraints."]

class ImageButton:
    def __init__(self, x, y, image, action, tooltip=""):
        self.image = image
        self.rect = self.image.get_rect(center=(x, y))
        self.action = action
        self.tooltip = tooltip
        self.show_tooltip = False
        
        # Cargar sonido de clic
        try:
            self.click_sound = pygame.mixer.Sound("assets/sounds/click.mp3")
        except:
            self.click_sound = None

    def draw(self, surface):
        """Draw the image button"""
        surface.blit(self.image, self.rect)
        
        # Mostrar tooltip si está hover
        mouse_pos = pygame.mouse.get_pos()
        self.show_tooltip = self.rect.collidepoint(mouse_pos)
        
        if self.show_tooltip and self.tooltip:
            # Crear superficie para el tooltip
            font = pygame.font.SysFont("Arial", 16)
            text_surface = font.render(self.tooltip, True, WHITE)
            
            # Fondo del tooltip
            tooltip_rect = text_surface.get_rect()
            tooltip_rect.centerx = self.rect.centerx
            tooltip_rect.bottom = self.rect.top - 5
            pygame.draw.rect(surface, (50, 50, 50), tooltip_rect.inflate(10, 5))
            pygame.draw.rect(surface, WHITE, tooltip_rect.inflate(10, 5), 1)
            
            # Dibujar texto
            surface.blit(text_surface, tooltip_rect)

    def is_clicked(self, pos, event=None):
        """Check if image was clicked and play sound"""
        clicked = self.rect.collidepoint(pos)
        if clicked:
            if self.click_sound:
                self.click_sound.play()
            if callable(self.action):
                self.action()
        return clicked

# Main entry point
if __name__ == "__main__":
    bridge = PrologBridge()
    game = EscapeRoomGUI(bridge)
    game.run()


