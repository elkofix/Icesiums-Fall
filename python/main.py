import pygame
import sys
from pygame.locals import *
import re
import tkinter as tk
from tkinter import filedialog
from integration.prolog_Bridge import PrologBridge
from integration.Rules import Rules
from integration.Search import Search
from integration.SearchNoConstraints import SearchNoConstraints
from integration.State import StateManager
from integration.Contraints import Constraints
from integration.Facts import Facts
import json

# Initialize Pygame
pygame.init()

# Constants
SCREEN_WIDTH = 1024
SCREEN_HEIGHT = 650
BUTTON_WIDTH = 200
BUTTON_HEIGHT = 40
MARGIN = 20
FONT_SIZE = 24
SCROLL_SPEED = 20

# Colors
WHITE = (255, 255, 255)
BLACK = (0, 0, 0)
GRAY = (200, 200, 200)
LIGHT_GRAY = (230, 230, 230)
DARK_GRAY = (100, 100, 100)
RED = (255, 0, 0)
GREEN = (0, 255, 0)
BLUE = (0, 0, 255)
YELLOW = (255, 255, 0)
PURPLE = (128, 0, 128)



class EscapeRoomGUI:
    def __init__(self, bridge):
        self.bridge = bridge
        self.screen = pygame.display.set_mode((SCREEN_WIDTH, SCREEN_HEIGHT))
        pygame.display.set_caption("Dynamic Escape Room Game")
        
        self.font = pygame.font.SysFont('Arial', FONT_SIZE)
        self.small_font = pygame.font.SysFont('Arial', FONT_SIZE - 4)
        
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
        self.mode = "standard"  # or "adversary"
        self.game_initialized = False
        self.show_help = False
        
        # Initialize game
        self.start_screen()
    
    def start_screen(self):
        """Show the initial game start screen"""
        self.output_text = ["Welcome to the Dynamic Escape Room Game!"]
        self.add_output("Choose game mode:")
        
        # Create buttons for game mode selection
        self.buttons = [
            Button(SCREEN_WIDTH//2 - BUTTON_WIDTH//2, 200, BUTTON_WIDTH, BUTTON_HEIGHT, 
                  "Standard Mode", self.start_standard_game),
            Button(SCREEN_WIDTH//2 - BUTTON_WIDTH//2, 260, BUTTON_WIDTH, BUTTON_HEIGHT, 
                  "Adversary Mode", self.start_adversary_game),
            Button(SCREEN_WIDTH//2 - BUTTON_WIDTH//2, 400, BUTTON_WIDTH, BUTTON_HEIGHT, 
                  "Help", self.toggle_help)
        ]
    
    def toggle_help(self):
        """Toggle help display"""
        self.show_help = not self.show_help
    
    def start_standard_game(self):
        """Start game in standard mode"""
        self.mode = "standard"
        self.facts = Facts(self.bridge)
        self.facts.set_game_mode("standard")
        self.initialize_game()
    
    def start_adversary_game(self):
        """Start game in adversary mode"""
        self.mode = "adversary"
        self.facts = Facts(self.bridge)
        self.facts.set_game_mode("adversary")
        self.movement_mode_screen()
            
    def initialize_game(self):
        """Initialize the game"""
        self.add_output("\nChoose game type:")
        self.buttons = [
            Button(SCREEN_WIDTH//2 - BUTTON_WIDTH - MARGIN, 200, BUTTON_WIDTH, BUTTON_HEIGHT, 
                  "Predefined Game", self.load_predefined_game),
            Button(SCREEN_WIDTH//2 + MARGIN, 200, BUTTON_WIDTH, BUTTON_HEIGHT, 
                  "Custom Game", self.create_custom_game),
            Button(SCREEN_WIDTH//2 - BUTTON_WIDTH//2, 300, BUTTON_WIDTH, BUTTON_HEIGHT, 
                  "Back", self.start_screen)
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
            filetypes=[("Archivos JSON", "*.json")]
        )

        if not file_path:
            self.add_output("Carga cancelada por el usuario.")
            return

        try:
            with open(file_path, 'r') as f:
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
                    traps=[("b", 3)] if self.mode == "adversary" else None
                )
                self.rules = Rules(self.bridge)
                if self.rules.initialize_game():
                    self.game_initialized = True
                    self.update_game_state()
                    self.add_output("Juego personalizado cargado exitosamente.")
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
            Button(SCREEN_WIDTH//2 - BUTTON_WIDTH//2, 200, BUTTON_WIDTH, BUTTON_HEIGHT, 
                  "Predictive", lambda: self.set_guard_movement(1)),
            Button(SCREEN_WIDTH//2 - BUTTON_WIDTH//2, 260, BUTTON_WIDTH, BUTTON_HEIGHT, 
                  "Random", lambda: self.set_guard_movement(2)),
        ]


    def main_game_screen(self):
        """Main game screen with all actions"""
        self.clear_outputs()
        self.buttons = [
            Button(MARGIN, SCREEN_HEIGHT - 1*(BUTTON_HEIGHT + MARGIN), BUTTON_WIDTH, BUTTON_HEIGHT, 
                  "Inventory", self.show_inventory),
            Button(MARGIN, SCREEN_HEIGHT - 3*(BUTTON_HEIGHT + MARGIN), BUTTON_WIDTH, BUTTON_HEIGHT, 
                  "Find Solution BFS", self.find_solution),
                              Button(MARGIN, SCREEN_HEIGHT - 4*(BUTTON_HEIGHT + MARGIN), BUTTON_WIDTH, BUTTON_HEIGHT, 
                  "Find Solution BFS no Cons", self.find_solution_no),
                              Button(MARGIN, SCREEN_HEIGHT - 5*(BUTTON_HEIGHT + MARGIN), BUTTON_WIDTH, BUTTON_HEIGHT, 
                  "Find Solution A*", self.find_solution_start),
                                  Button(MARGIN, SCREEN_HEIGHT - 6*(BUTTON_HEIGHT + MARGIN), BUTTON_WIDTH, BUTTON_HEIGHT, 
                  "Clear", self.clear_outputs),
            Button(SCREEN_WIDTH - BUTTON_WIDTH - MARGIN, SCREEN_HEIGHT - BUTTON_HEIGHT - MARGIN, 
                  BUTTON_WIDTH, BUTTON_HEIGHT, "New Game", self.start_screen)
        ]
        
        # Add room-specific buttons
        self.update_action_buttons()

    
    def update_action_buttons(self):
        """Update buttons for current room actions"""
        # Remove old action buttons (keep the first 6 buttons)
        if len(self.buttons) > 6:
            self.buttons = self.buttons[:6]
        
        # Get available actions from state manager
        self.state = StateManager(self.bridge)
        self.available_actions = self.state.get_available_actions()
        print("actions", self.available_actions)
        # Add action buttons
        y_pos = MARGIN
        for action in self.available_actions:
            action_type, action_target = action.split(":")
            btn_text = f"{action_type.replace('_', ' ').title()}: {action_target}"
            
            # Shorten long button text
            if len(btn_text) > 20:
                btn_text = f"{action_type.replace('_', ' ').title()}"
            
            self.buttons.append(
                Button(SCREEN_WIDTH - BUTTON_WIDTH - MARGIN, y_pos, BUTTON_WIDTH, BUTTON_HEIGHT, 
                      btn_text, lambda a=action: self.handle_action(a))
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
        
        # Update game state after action
        self.update_game_state()
    
    def update_game_state(self):
        """Update the current game state"""
        self.state = StateManager(self.bridge)
        state = self.state.get_current_state()
        
        if state:
            self.current_room = state['room']
            self.inventory = state['inventory']
            self.solved_puzzles = state['solved_puzzles']
            self.doors = state['doors']
            
            print(self.facts.get_objects())
            # Get objects in current room
            self.objects = [obj['Object'] for obj in 
                            self.facts.get_objects() if obj['Room'] == self.current_room]

            # Get keys in current room (both original and dropped)
            original_keys = [key['Key'] for key in 
                           self.facts.get_keys() if key['Room'] == self.current_room]
            dropped_keys = [key['Key'] for key in state['dropped_keys'] 
                          if key['R'] == self.current_room]
            self.keys = list(set(original_keys + dropped_keys))
            
            # Get pieces in current room (both original and dropped)
            original_pieces = [(piece['Piece'], piece['Puzzle']) for piece in 
                             self.facts.get_piece_locations() if piece['Room'] == self.current_room]
            dropped_pieces = [(piece['Piece'], piece['Puzzle']) for piece in 
                            state['dropped_pieces'] if piece['R'] == self.current_room]
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
        self.scroll_position = max(0, len(self.output_text) - (SCREEN_HEIGHT // (FONT_SIZE + 4)) * (FONT_SIZE + 4))


    def show_inventory(self):
        """Show player inventory"""
        self.add_output("\nYour inventory:")
        if not self.inventory:
            self.add_output("  Empty")
        else:
            for item in self.inventory:
                self.add_output(f"- {item}")
        
        # Show key count
        key_limit = self.constraints.can_carry('key')
        key_count = self.constraints.count_items_of_type('key')
        self.add_output(f"\nKeys: {key_count}/{key_limit}")
        
        # Show puzzle pieces
        self.add_output("\nCollected puzzle pieces:")
        puzzles = self.facts.get_puzzles()
        if not puzzles:
            self.add_output("  None")
        else:
            for puzzle in puzzles:
                puzzle = puzzle['Puzzle']
                pieces = list(self.bridge.prolog.query(f"state:has_piece({puzzle}, Piece)"))
                piece_names = [p['Piece'] for p in pieces]
                self.add_output(f"Puzzle {puzzle}: {piece_names}")
        
        # Show piece count
        piece_limit = self.constraints.can_carry('piece')
        piece_count = self.constraints.count_items_of_type('piece')
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
        self.add_output(f"Moves: {moves_used}/{max_moves} ({remaining_moves} remaining)")
        
        max_b_visits = self.constraints.max_b_visits()
        if max_b_visits:
            b_visits = list(self.bridge.prolog.query("constraints:b_visits(X)"))
            b_visits = b_visits[0]['X'] if b_visits else 0
            remaining_b_visits = max_b_visits - b_visits
            self.add_output(f"Room B visits: {b_visits}/{max_b_visits} ({remaining_b_visits} remaining)")
        
        # Inventory limits
        key_limit = self.constraints.can_carry('key')
        piece_limit = self.constraints.can_carry('piece')
        self.add_output(f"\nInventory Limits:")
        self.add_output(f"- Keys: {key_limit}")
        self.add_output(f"- Puzzle pieces: {piece_limit}")
        
        # Room traps
        traps = list(self.bridge.prolog.query("constraints:trap(Room, Turns)"))
        if traps:
            self.add_output("\nRoom Traps:")
            for trap in traps:
                room = trap['Room']
                turns = trap['Turns']
                current_turns = self.constraints.turns_in_room(room)
                remaining = turns - current_turns
                self.add_output(f"- Room {room}: activates after {turns} turns ({remaining} remaining)")
    
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
            if self.mode == "adversary":
                query = """
                    with_output_to(
                        codes(Codes),
                        adversary:move_guard
                    ),
                    atom_codes(Output, Codes)
                    """
                result = list(self.bridge.prolog.query(query))
                if result:
                    output = result[0]["Output"]
                    self.add_output(f"\n{output}")
                else:
                    self.add_output("\nNo guard movement output.")
                
            
            self.add_output(f"\nMoved to room {new_room}.")
        else:
            self.add_output(f"\nCannot move to room {new_room}.")
    
    def pick_key(self, key):
        """Pick up a key"""
        if self.constraints.check_inventory_limit('key'):
            if self.state.pick_key(key):
                self.add_output(f"\nPicked up key: {key}")
            else:
                self.add_output(f"\nCannot pick up key: {key}")
        else:
            self.add_output(f"\nCannot carry more keys (limit: {self.constraints.can_carry('key')})")
    
    def pick_piece(self, piece):
        """Pick up a puzzle piece"""
        if self.constraints.check_inventory_limit('piece'):
            if self.state.pick_piece(piece):
                self.add_output(f"\nPicked up puzzle piece: {piece}")
            else:
                self.add_output(f"\nCannot pick up piece: {piece}")
        else:
            self.add_output(f"\nCannot carry more puzzle pieces (limit: {self.constraints.can_carry('piece')})")
    
    def move_object(self, obj):
        """Examine/move an object"""
        if self.state.move_object(obj):
            self.add_output(f"\nExamined object: {obj}")
            # Check if object hides a puzzle piece
            hidden_piece = list(self.bridge.prolog.query(f"facts:hide_piece({obj}, Puzzle, Piece)"))
            print("hidden", hidden_piece)
            if hidden_piece:
                piece = hidden_piece[0]['Piece']
                puzzle = hidden_piece[0]['Puzzle']
                self.add_output(f"Found hidden puzzle piece: {piece} (of puzzle {puzzle})")
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
    
    def find_solution(self):
        """Find escape solution"""
        self.add_output("\nSearching for escape solution with bfs...")
        solution = self.bridge.find_escape_plan()
        
        if solution:
            self.add_output("\nSolution found:", False, 80)
            for step in solution:
                self.add_output(f"- {step}",False, 80)
            self.add_output(f"Total steps required: {len(solution)}" , False, 80)
        else:
            self.add_output("\nNo escape solution found! The room might be unsolvable.")
    
    def find_solution_no(self):
        """Find escape solution"""
        self.add_output("\nSearching for escape solution with no constraints...")
        solution = self.bridge.find_escape_plan_no()
        
        if solution:
            self.add_output("\nSolution found:", False, 80)
            for step in solution:
                self.add_output(f"- {step}",False, 80)
            self.add_output(f"Total steps required: {len(solution)}" , False, 80)
        else:
            self.add_output("\nNo escape solution found! The room might be unsolvable.")
    
    def find_solution_start(self):
        """Find escape solution"""
        self.add_output("\nSearching for escape solution with A*...")
        solution = self.bridge.find_escape_plan_star()
        
        if solution:
            self.add_output("\nSolution found:", False, 80)
            for step in solution:
                self.add_output(f"- {step}",False, 80)
            self.add_output(f"Total steps required: {len(solution)}" , False, 80)
        else:
            self.add_output("\nNo escape solution found! The room might be unsolvable.")
    
    def run(self):
        """Main game loop"""
        clock = pygame.time.Clock()
        running = True
        
        while running:
            for event in pygame.event.get():
                if event.type == QUIT:
                    running = False
                elif event.type == KEYDOWN:
                    if event.key == K_UP:
                        self.scroll_position = max(0, self.scroll_position - SCROLL_SPEED)
                    elif event.key == K_DOWN:
                        max_scroll = max(0, len(self.output_text) * (FONT_SIZE + 4) - SCREEN_HEIGHT + 100)
                        self.scroll_position = min(max_scroll, self.scroll_position + SCROLL_SPEED)
                    elif event.key == K_h:
                        self.toggle_help()
                elif event.type == MOUSEBUTTONDOWN:
                    if event.button == 4:  # Scroll up
                        self.scroll_position = max(0, self.scroll_position - SCROLL_SPEED)
                    elif event.button == 5:  # Scroll down
                        max_scroll = max(0, len(self.output_text) * (FONT_SIZE + 4) - SCREEN_HEIGHT + 100)
                        self.scroll_position = min(max_scroll, self.scroll_position + SCROLL_SPEED)
                    else:
                        # Check button clicks
                        for button in self.buttons:
                            if button.is_clicked(event.pos):
                                button.action()
            
            # Draw everything
            self.screen.fill(WHITE)
            
            # Draw output text
            y_pos = MARGIN - self.scroll_position
            for i, line in enumerate(self.output_text):
                if MARGIN <= y_pos <= SCREEN_HEIGHT - MARGIN:
                    text_surface = self.font.render(line, True, BLACK)
                    self.screen.blit(text_surface, (MARGIN, y_pos))
                y_pos += FONT_SIZE + 4
            
            # Draw buttons
            for button in self.buttons:
                button.draw(self.screen)
            
            # Draw help if shown
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
            "Press H to close help"
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
        self.font = pygame.font.SysFont('Arial', 20)
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