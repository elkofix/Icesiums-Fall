import pygame
import sys
import os
from pygame.locals import *
import re
import tkinter as tk
from tkinter import filedialog
from integration.prolog_Bridge import PrologBridge
from components.Cinematic import Cinematic
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
        self.solver =  Solver(self.bridge, self.add_output)
        self.screen = pygame.display.set_mode((SCREEN_WIDTH, SCREEN_HEIGHT))
        pygame.display.set_caption("Dynamic Escape Room Game")
        self.show_map = False
        self.map = {}
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

        # UI state
        self.output_text = []
        self.scroll_position = 0
        self.mode = "standard"
        self.game_initialized = False
        self.show_help = False

    def start_screen(self):
        """Show the initial game start screen"""
        self.show_map = False
        self.output_text = ["Welcome to the Dynamic Escape Room Game!"]
        self.add_output("Choose game mode:")

        # Create buttons for game mode selection
        self.buttons = [
            Button(
                SCREEN_WIDTH // 2 - BUTTON_WIDTH // 2,
                200,
                BUTTON_WIDTH,
                BUTTON_HEIGHT,
                "Standard Mode",
                self.start_standard_game,
            ),
            Button(
                SCREEN_WIDTH // 2 - BUTTON_WIDTH // 2,
                260,
                BUTTON_WIDTH,
                BUTTON_HEIGHT,
                "Adversary Mode",
                self.start_adversary_game,
            ),
            Button(
                SCREEN_WIDTH // 2 - BUTTON_WIDTH // 2,
                400,
                BUTTON_WIDTH,
                BUTTON_HEIGHT,
                "Help",
                self.toggle_help,
            ),
        ]

    def toggle_help(self):
        """Toggle help display"""
        self.show_help = not self.show_help

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
        self.add_output("\nChoose game type:")
        self.buttons = [
            Button(
                SCREEN_WIDTH // 2 - BUTTON_WIDTH - MARGIN,
                200,
                BUTTON_WIDTH,
                BUTTON_HEIGHT,
                "Predefined Game",
                self.load_predefined_game,
            ),
            Button(
                SCREEN_WIDTH // 2 + MARGIN,
                200,
                BUTTON_WIDTH,
                BUTTON_HEIGHT,
                "Custom Game",
                self.create_custom_game,
            ),
            Button(
                SCREEN_WIDTH // 2 - BUTTON_WIDTH // 2,
                300,
                BUTTON_WIDTH,
                BUTTON_HEIGHT,
                "Back",
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
        self.add_output("How should the guard move?")
        self.buttons = [
            Button(
                SCREEN_WIDTH // 2 - BUTTON_WIDTH // 2,
                200,
                BUTTON_WIDTH,
                BUTTON_HEIGHT,
                "Predictive",
                lambda: self.set_guard_movement(1),
            ),
            Button(
                SCREEN_WIDTH // 2 - BUTTON_WIDTH // 2,
                260,
                BUTTON_WIDTH,
                BUTTON_HEIGHT,
                "Random",
                lambda: self.set_guard_movement(2),
            ),
        ]

    def main_game_screen(self):
        """Main game screen with all actions"""
        self.clear_outputs()
        self.buttons = [
            Button(
                MARGIN,
                SCREEN_HEIGHT - 1 * (BUTTON_HEIGHT + MARGIN),
                BUTTON_WIDTH,
                BUTTON_HEIGHT,
                "Inventory",
                self.show_inventory,
            ),
            Button(
                MARGIN,
                SCREEN_HEIGHT - 3 * (BUTTON_HEIGHT + MARGIN),
                BUTTON_WIDTH,
                BUTTON_HEIGHT,
                "Find Solution BFS",
                self.solver.find_solution,
            ),
            Button(
                MARGIN,
                SCREEN_HEIGHT - 4 * (BUTTON_HEIGHT + MARGIN),
                BUTTON_WIDTH,
                BUTTON_HEIGHT,
                "Find Solution BFS no Cons",
                self.solver.find_solution_no,
            ),
            Button(
                MARGIN,
                SCREEN_HEIGHT - 5 * (BUTTON_HEIGHT + MARGIN),
                BUTTON_WIDTH,
                BUTTON_HEIGHT,
                "Find Solution A*",
                self.solver.find_solution_start,
            ),
            Button(
                MARGIN,
                SCREEN_HEIGHT - 6 * (BUTTON_HEIGHT + MARGIN),
                BUTTON_WIDTH,
                BUTTON_HEIGHT,
                "Clear",
                self.clear_outputs,
            ),
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
        """Dibuja el mapa usando NetworkX con una solución robusta para Pygame"""
        if not hasattr(self, "map") or not self.map:
            return

        try:
            import networkx as nx
            import matplotlib.pyplot as plt
            import io
            from PIL import Image
        except ImportError:
            print(
                "Advertencia: NetworkX/Matplotlib no instalados. No se dibujará el mapa."
            )
            return

        for node in self.map["nodes"]:
            node["is_current"] = node["id"] == self.state.player_location()
        # Crear el grafo
        G = nx.DiGraph()

        # Añadir nodos y aristas
        for node in self.map["nodes"]:
            G.add_node(node["id"], **node)
        for link in self.map["links"]:
            G.add_edge(link["source"], link["target"], **link)

        # Configurar la figura
        fig, ax = plt.subplots(figsize=(10, 6), facecolor="#1a1a2e")
        pos = nx.spring_layout(G, seed=42)

        # Personalizar nodos
        node_colors = []
        for node in G.nodes():
            if G.nodes[node].get("is_current", False):
                node_colors.append("#4cc9f0")  # Habitación actual
            elif G.nodes[node].get("has_guard", False):
                node_colors.append("#f72585")  # Con guardia
            else:
                node_colors.append("#4361ee")  # Normal

        # Dibujar el grafo
        nx.draw_networkx_nodes(G, pos, node_color=node_colors, node_size=1500, ax=ax)
        nx.draw_networkx_labels(G, pos, font_color="white", ax=ax)

        # Personalizar aristas
        edge_colors = []
        for u, v in G.edges():
            if G.edges[u, v].get("state", "") == "locked":
                edge_colors.append("#ff9e00")  # Cerrada
            else:
                edge_colors.append("#4ad66d")  # Abierta

        nx.draw_networkx_edges(
            G, pos, edge_color=edge_colors, width=2, arrows=True, ax=ax
        )

        # Convertir a imagen compatible con Pygame
        buf = io.BytesIO()
        plt.savefig(buf, format="png", bbox_inches="tight", pad_inches=0, dpi=100)
        buf.seek(0)

        # Usar PIL para cargar la imagen y luego convertir a Pygame
        img = Image.open(buf)
        img = img.resize((int(SCREEN_WIDTH * 0.8), int(SCREEN_HEIGHT * 0.7)))
        mode = img.mode
        size = img.size
        data = img.tobytes()

        pygame_img = pygame.image.fromstring(data, size, mode)

        # Dibujar en la pantalla
        self.screen.blit(pygame_img, (SCREEN_WIDTH * 0.1, SCREEN_HEIGHT * 0.15))

        # Limpiar
        plt.close(fig)
        buf.close()

    def update_action_buttons(self):
        """Update buttons for current room actions"""
        # Remove old action buttons (keep the first 6 buttons)
        if len(self.buttons) > 6:
            self.buttons = self.buttons[:6]

        # Get available actions from state manager
        self.available_actions = self.state.get_available_actions()
        collected_items = self.state.get_collected_items()
        print("actions", self.available_actions)
        # Add action buttons
        print(collected_items)
        y_pos = MARGIN
        for action in self.available_actions:
            action_type, action_target = action.split(":")
            print("accion", action_type)
            print("object", action_target)
            if action_type == "pick_key" and action_target in collected_items["keys"]:
                continue
            if (
                action_type == "pick_piece"
                and action_target in collected_items["pieces"]
            ):
                continue
            btn_text = f"{action_type.replace('_', ' ').title()}: {action_target}"

            # Shorten long button text
            if len(btn_text) > 20:
                btn_text = f"{action_type.replace('_', ' ').title()}"

            self.buttons.append(
                Button(
                    SCREEN_WIDTH - BUTTON_WIDTH - MARGIN,
                    y_pos,
                    BUTTON_WIDTH,
                    BUTTON_HEIGHT,
                    btn_text,
                    lambda a=action: self.handle_action(a),
                )
            )
            y_pos += BUTTON_HEIGHT + MARGIN

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
            self.add_output("\nCONGRATULATIONS! You've escaped the room!")
            self.add_output("Game Over - You Win!")
            self.game_initialized = False

        # Update action buttons
        self.update_action_buttons()

    def clear_outputs(self):
        """Clear all text from the output display"""
        self.output_text = []
        self.scroll_position = 0

    def add_output(self, text, center=False, indent=0):
        """Add text to the output display, optionally centered or indented"""
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
        """Move player to a new room"""
        if self.state.move_player(new_room):
            # Check if new room has trap
            if not self.constraints.check_trap(new_room):
                self.add_output(f"\nTRAP ACTIVATED in room {new_room}! Game Over!")
                self.game_initialized = False
                return

            # Check move limit
            if not self.constraints.check_move_limit():
                self.add_output("\nMOVE LIMIT REACHED! Game Over!")
                self.game_initialized = False
                return

            # In adversary mode, move the guard

            self.add_output(f"\nMoved to room {new_room}.")
        else:
            self.add_output(f"\nCannot move to room {new_room}.")

    def pick_key(self, key):
        """Pick up a key"""
        if self.constraints.check_inventory_limit("key"):
            if self.state.pick_key(key):
                self.add_output(f"\nPicked up key: {key}")
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
        if self.state.unlock_door(from_room, to_room):
            self.add_output(f"\nUnlocked door between {from_room} and {to_room}")
        else:
            self.add_output(f"\nCannot unlock door between {from_room} and {to_room}")

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
            self.screen.fill(WHITE)

            if not self.cinematic.intro_completed:
                self.cinematic.draw_intro()
            else:
                # Dibujar texto del juego
                y_pos = MARGIN - self.scroll_position
                for i, line in enumerate(self.output_text):
                    if MARGIN <= y_pos <= SCREEN_HEIGHT - MARGIN:
                        text_surface = self.font.render(line, True, BLACK)
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
        """Draw help overlay"""
        # Semi-transparent overlay
        s = pygame.Surface((SCREEN_WIDTH, SCREEN_HEIGHT), pygame.SRCALPHA)
        s.fill((0, 0, 0, 180))
        self.screen.blit(s, (0, 0))

        # Help text
        help_text = [
            "Available commands:",
            "- Inventory: View your collected items",
            "- Game Stats: View game constraints and limits",
            "- Find Solution: Calculate escape path (may take time)",
            "",
            "Room Actions:",
            "- Move To: Move to another room",
            "- Pick Key: Collect a key",
            "- Pick Piece: Collect a puzzle piece",
            "- Move Object: Examine an object",
            "- Solve Puzzle: Solve a puzzle with collected pieces",
            "- Unlock Door: Unlock a door with the right key",
            "",
            "Press H to close help",
        ]

        # Draw help box
        help_width = SCREEN_WIDTH - 200
        help_height = SCREEN_HEIGHT - 200
        pygame.draw.rect(self.screen, WHITE, (100, 100, help_width, help_height))
        pygame.draw.rect(self.screen, BLUE, (100, 100, help_width, help_height), 3)

        # Draw help text
        y_pos = 120
        for line in help_text:
            text_surface = self.font.render(line, True, BLACK)
            self.screen.blit(text_surface, (120, y_pos))
            y_pos += FONT_SIZE + 4

class Button:
    def __init__(self, x, y, width, height, text, action):
        self.rect = pygame.Rect(x, y, width, height)
        self.text = text
        self.action = action
        self.font = pygame.font.SysFont("Arial", 20)
        self.normal_color = BLUE
        self.hover_color = GREEN
        self.text_color = WHITE

    def draw(self, surface):
        """Draw the button"""
        mouse_pos = pygame.mouse.get_pos()
        is_hovered = self.rect.collidepoint(mouse_pos)

        color = self.hover_color if is_hovered else self.normal_color
        pygame.draw.rect(surface, color, self.rect)
        pygame.draw.rect(surface, BLACK, self.rect, 2)  # Border

        text_surface = self.font.render(self.text, True, self.text_color)
        text_rect = text_surface.get_rect(center=self.rect.center)
        surface.blit(text_surface, text_rect)

    def is_clicked(self, pos):
        """Check if button was clicked"""
        return self.rect.collidepoint(pos)

# Main entry point
if __name__ == "__main__":
    bridge = PrologBridge()
    game = EscapeRoomGUI(bridge)
    game.run()
