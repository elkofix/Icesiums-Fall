import os
import pygame
from assets.texts.colors import BLACK
from assets.texts.constants import SCREEN_WIDTH, SCREEN_HEIGHT
import time  # Para controlar el tiempo de las imágenes
from assets.texts.dialogs import DIALOGS

class Cinematic:

    def __init__(self, start_screen_callback):
        self.screen = pygame.display.set_mode((SCREEN_WIDTH, SCREEN_HEIGHT))
        self.start_screen = start_screen_callback
        self.intro_image_margin_top = 100  # Margen superior de 100px
        self.intro_image_margin_sides = 200  # Margen horizontal de 200px
        self.intro_texts = DIALOGS
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

        # Cinemática introductoria
        self.intro_completed = False
        self.intro_images = []
        self.current_intro_image = 0
        self.intro_start_time = 0
        self.intro_music = None

        # Cargar recursos de la cinemática
        self.load_intro_resources()

        # Iniciar con la cinemática
        self.play_intro()

    def load_intro_resources(self):
        """Cargar imágenes y música para la cinemática introductoria"""
        try:
            # Cargar imágenes (asegúrate de que estos archivos existan en tu directorio)
            for i in range(1, 34):
                img_path = f"assets/images/intro_image{i}.png"
                if os.path.exists(img_path):
                    # Cargar la imagen manteniendo su relación de aspecto
                    original_image = pygame.image.load(img_path)
                    original_width, original_height = original_image.get_size()

                    # Calcular dimensiones máximas disponibles
                    max_width = SCREEN_WIDTH - 2 * self.intro_image_margin_sides
                    max_height = (
                        SCREEN_HEIGHT - self.intro_image_margin_top - 300
                    )  # 50px para el texto de "saltar"

                    # Calcular nuevas dimensiones manteniendo relación de aspecto
                    ratio = min(
                        max_width / original_width, max_height / original_height
                    )
                    new_width = int(original_width * ratio)
                    new_height = int(original_height * ratio)

                    # Escalar la imagen
                    image = pygame.transform.scale(
                        original_image, (new_width, new_height)
                    )
                    self.intro_images.append(
                        {"surface": image, "width": new_width, "height": new_height}
                    )
                else:
                    print(f"Advertencia: No se encontró {img_path}")
                    # Crear una imagen de relleno con las dimensiones máximas
                    placeholder = pygame.Surface((max_width, max_height))
                    placeholder.fill((50, 50, 50))  # Gris oscuro
                    font = pygame.font.SysFont("Arial", 30)
                    text = font.render(f"Intro Image {i}", True, (200, 200, 200))
                    text_rect = text.get_rect(center=(max_width // 2, max_height // 2))
                    placeholder.blit(text, text_rect)

                    self.intro_images.append(
                        {
                            "surface": placeholder,
                            "width": max_width,
                            "height": max_height,
                        }
                    )
            while len(self.intro_texts) < len(self.intro_images):
                self.intro_texts.append("Continúa la historia...")

            # Cargar música (igual que antes)
            if os.path.exists("assets/sounds/intro_music.mp3"):
                self.intro_music = pygame.mixer.Sound("assets/sounds/intro_music.mp3")
            else:
                print("Advertencia: No se encontró assets/sounds/intro_music.mp3")

        except Exception as e:
            print(f"Error al cargar recursos de la cinemática: {e}")
            # Si hay error, saltar la cinemática
            self.intro_completed = True
            self.start_screen()


    def draw_intro(self):
        """Dibujar la imagen actual de la cinemática con los nuevos márgenes"""
        if not self.intro_completed and self.intro_images:
            # Dibujar fondo negro
            self.screen.fill(BLACK)

            # Obtener la imagen actual
            current_img = self.intro_images[self.current_intro_image]
            img_surface = current_img["surface"]
            img_width = current_img["width"]
            img_height = current_img["height"]

            # Calcular posición centrada
            x_pos = (SCREEN_WIDTH - img_width) // 2
            y_pos = self.intro_image_margin_top

            # Dibujar la imagen
            self.screen.blit(img_surface, (x_pos, y_pos))

            # Dibujar el texto correspondiente debajo de la imagen
            if self.current_intro_image < len(self.intro_texts):
                text_lines = self.wrap_text(
                    self.intro_texts[self.current_intro_image],
                    self.retro_font_small,
                    SCREEN_WIDTH - 2 * self.intro_image_margin_sides,
                )

                text_y = y_pos + img_height + 20  # 20px debajo de la imagen

                for line in text_lines:
                    text_surface = self.retro_font_small.render(
                        line, True, (255, 255, 0)
                    )  # Texto amarillo
                    text_rect = text_surface.get_rect(
                        center=(SCREEN_WIDTH // 2, text_y)
                    )
                    self.screen.blit(text_surface, text_rect)
                    text_y += 30  # Espacio entre líneas
            # Dibujar un texto indicando que se puede saltar
            skip_text = self.retro_font_small.render(
                "Presiona ESPACIO para saltar la intro", True, (255, 255, 255)
            )
            text_rect = skip_text.get_rect(
                center=(SCREEN_WIDTH // 2, SCREEN_HEIGHT - 30)
            )
            self.screen.blit(skip_text, text_rect)

    def wrap_text(self, text, font, max_width):
        """Envuelve el texto en múltiples líneas para que quepa en el ancho especificado"""
        words = text.split(" ")
        lines = []
        current_line = []

        for word in words:
            test_line = " ".join(current_line + [word])
            test_width = font.size(test_line)[0]

            if test_width <= max_width:
                current_line.append(word)
            else:
                lines.append(" ".join(current_line))
                current_line = [word]

        if current_line:
            lines.append(" ".join(current_line))

        return lines

    def play_intro(self):
        """Reproducir la cinemática introductoria"""
        if not self.intro_images:
            self.intro_completed = True
            self.start_screen()
            return

        # Reproducir música si está disponible
        if self.intro_music:
            self.intro_music.play()

        # Configurar temporizador para la primera imagen
        self.intro_start_time = time.time()
        self.current_intro_image = 0

    def update_intro(self):
        """Actualizar la cinemática introductoria"""
        if self.intro_completed or not self.intro_images:
            return

        current_time = time.time()
        # Cambiar de imagen cada 3 segundos
        if current_time - self.intro_start_time >= 2.15:
            self.current_intro_image += 1
            self.intro_start_time = current_time

            # Si hemos mostrado todas las imágenes, terminar la cinemática
            if self.current_intro_image >= len(self.intro_images):
                self.intro_completed = True
                if self.intro_music:
                    self.intro_music.stop()
                self.start_screen()

    def skip_intro(self):
        """Saltar la cinemática introductoria"""
        if not self.intro_completed:
            self.intro_completed = True
            if self.intro_music:
                self.intro_music.stop()
            self.start_screen()