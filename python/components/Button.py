from assets.texts.colors import WHITE, BLACK
import pygame

class Button:
    def __init__(self, x, y, width, height, text, action, 
                 bg_color=None, text_color=WHITE, border_color=None, 
                 font=None, hover_text_color=None):
        self.rect = pygame.Rect(x, y, width, height)
        self.text = text
        self.action = action
        self.bg_color = bg_color  # None para transparente
        self.text_color = text_color
        self.border_color = border_color  # None para sin borde
        self.hover_text_color = hover_text_color if hover_text_color else text_color
        
        # Configuración de fuente
        if font:
            self.font = font
        else:
            try:
                self.font = pygame.font.Font("retro_font.ttf", 20)
            except:
                self.font = pygame.font.SysFont("Arial", 20)
        
        # Cargar sonido de clic
        try:
            self.click_sound = pygame.mixer.Sound("assets/sounds/click.mp3")
        except:
            print("Warning: No se pudo cargar el sonido de clic")
            self.click_sound = None

    def draw(self, surface):
        """Draw the button with no background or border, just text"""
        mouse_pos = pygame.mouse.get_pos()
        is_hovered = self.rect.collidepoint(mouse_pos)
        
        # Color del texto (cambia si está hover y hay color definido)
        current_text_color = self.hover_text_color if is_hovered else self.text_color
        
        # Dibujar fondo solo si hay color definido
        if self.bg_color:
            pygame.draw.rect(surface, self.bg_color, self.rect)
        
        # Dibujar borde solo si hay color definido
        if self.border_color:
            pygame.draw.rect(surface, self.border_color, self.rect, 2)
        
        # Renderizar texto
        text_surface = self.font.render(self.text, True, current_text_color)
        text_rect = text_surface.get_rect(center=self.rect.center)
        surface.blit(text_surface, text_rect)

    def is_clicked(self, pos, event=None):
        """Check if button was clicked and play sound"""
        clicked = self.rect.collidepoint(pos)
        if clicked:
            if self.click_sound:
                self.click_sound.play()
            if callable(self.action):
                self.action()
        return clicked