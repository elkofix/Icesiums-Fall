from assets.texts.colors import WHITE, BLACK, GRAY, LIGHT_GRAY, DARK_GRAY, RED, GREEN, BLUE, YELLOW, PURPLE
import pygame

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