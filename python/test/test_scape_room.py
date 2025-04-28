import pytest
import pygame
import sys
import os
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

from main import EscapeRoomGUI, ImageButton
from integration.prolog_Bridge import PrologBridge

# Fixtures remain the same
@pytest.fixture(scope="module")
def pygame_init():
    pygame.init()
    pygame.display.set_mode((800, 600))
    yield
    pygame.quit()

@pytest.fixture
def game_instance(pygame_init, monkeypatch):
    bridge = PrologBridge()
    instance = EscapeRoomGUI(bridge)
    monkeypatch.setattr(instance, 'start_standard_game', lambda: None)
    monkeypatch.setattr(instance, 'load_predefined_game', lambda: None)
    monkeypatch.setattr(instance, 'move_player', lambda x: True)
    monkeypatch.setattr(instance, 'pick_key', lambda x: True)
    monkeypatch.setattr(instance, 'unlock_door', lambda x, y: True)
    monkeypatch.setattr(instance, 'update_output_text', lambda x: None)
    instance.current_room = "a"
    instance.inventory = []
    instance.doors = [{"from": "a", "to": "b", "state": "locked"}]
    instance.output_text = ["Started"]
    instance.game_initialized = True
    return instance

## Initialization Tests (Expanded)
def test_image_button_initialization_minimal():
    """Most basic initialization test"""
    button = ImageButton(0, 0, pygame.Surface((1, 1)), lambda: None)
    assert button is not None

def test_image_button_initialization_size():
    """Test button size matches image size"""
    test_img = pygame.Surface((75, 25))
    button = ImageButton(0, 0, test_img, lambda: None)
    assert button.rect.width == 75
    assert button.rect.height == 25

def test_image_button_initialization_with_tooltip():
    """Test initialization with tooltip"""
    button = ImageButton(0, 0, pygame.Surface((10, 10)), lambda: None, "Help text")
    assert button.tooltip == "Help text"

## Click Action Tests (Expanded)
def test_image_button_click_with_counter():
    """Test click using a counter"""
    counter = 0
    def increment():
        nonlocal counter
        counter += 1
    button = ImageButton(0, 0, pygame.Surface((10, 10)), increment)
    
    button.action()
    button.action()
    assert counter == 2

def test_image_button_click_with_side_effect():
    """Test click using side effects"""
    side_effect = []
    def action():
        side_effect.append("clicked")
    button = ImageButton(0, 0, pygame.Surface((10, 10)), action)
    
    button.action()
    assert side_effect == ["clicked"]

def test_image_button_click_with_return_value():
    """Test click that returns a value"""
    def action():
        return 42
    button = ImageButton(0, 0, pygame.Surface((10, 10)), action)
    
    result = button.action()
    assert result == 42

def test_image_button_click_with_exception():
    """Test click that raises an exception"""
    def action():
        raise ValueError("Test error")
    button = ImageButton(0, 0, pygame.Surface((10, 10)), action)
    
    with pytest.raises(ValueError, match="Test error"):
        button.action()

def test_image_button_click_with_multiple_actions():
    """Test multiple actions"""
    results = []
    def action1():
        results.append(1)
    def action2():
        results.append(2)
    
    button1 = ImageButton(0, 0, pygame.Surface((10, 10)), action1)
    button2 = ImageButton(0, 0, pygame.Surface((10, 10)), action2)
    
    button1.action()
    button2.action()
    assert results == [1, 2]

def test_image_button_click_with_lambda():
    """Test click with lambda function"""
    x = 5
    button = ImageButton(0, 0, pygame.Surface((10, 10)), lambda: x * 2)
    assert button.action() == 10

def test_image_button_click_with_args():
    """Test click with function that takes args (should fail)"""
    def action(x):
        return x * 2
    button = ImageButton(0, 0, pygame.Surface((10, 10)), action)
    with pytest.raises(TypeError):
        button.action()

## Position and Collision Tests (Expanded)
def test_image_button_contains_point_inside():
    """Test point inside button"""
    button = ImageButton(100, 100, pygame.Surface((50, 50)), lambda: None)
    assert button.rect.collidepoint(110, 110) is True

def test_image_button_contains_point_outside():
    """Test point outside button"""
    button = ImageButton(100, 100, pygame.Surface((50, 50)), lambda: None)
    assert button.rect.collidepoint(200, 200) is False

def test_image_button_position_after_move():
    """Test button position after moving"""
    button = ImageButton(100, 100, pygame.Surface((50, 50)), lambda: None)
    button.rect.x = 200
    button.rect.y = 300
    assert button.rect.topleft == (200, 300)

def test_image_button_collision_with_other_rect():
    """Test collision with another rect"""
    button = ImageButton(100, 100, pygame.Surface((50, 50)), lambda: None)
    other_rect = pygame.Rect(120, 120, 30, 30)
    assert button.rect.colliderect(other_rect) is True

def test_image_button_no_collision_when_moved():
    """Test no collision after moving away"""
    button = ImageButton(100, 100, pygame.Surface((50, 50)), lambda: None)
    other_rect = pygame.Rect(200, 200, 30, 30)
    button.rect.x = 300
    assert button.rect.colliderect(other_rect) is False

## Tooltip Tests (Expanded)
def test_image_button_tooltip_setting():
    """Test setting tooltip after creation"""
    button = ImageButton(0, 0, pygame.Surface((10, 10)), lambda: None)
    button.tooltip = "New tooltip"
    assert button.tooltip == "New tooltip"

def test_image_button_tooltip_empty_string():
    """Test empty string tooltip"""
    button = ImageButton(0, 0, pygame.Surface((10, 10)), lambda: None, "")
    assert button.tooltip == ""

def test_image_button_tooltip_long_text():
    """Test long tooltip text"""
    long_text = "This is a very long tooltip text that might wrap in the UI"
    button = ImageButton(0, 0, pygame.Surface((10, 10)), lambda: None, long_text)
    assert button.tooltip == long_text

def test_image_button_tooltip_special_chars():
    """Test tooltip with special characters"""
    special_text = "Tooltip with spéciål chàracters!@#$%^&*()"
    button = ImageButton(0, 0, pygame.Surface((10, 10)), lambda: None, special_text)
    assert button.tooltip == special_text

def test_image_button_tooltip_none():
    """Test None tooltip"""
    button = ImageButton(0, 0, pygame.Surface((10, 10)), lambda: None, None)
    assert button.tooltip is None

def test_image_button_tooltip_unicode():
    """Test unicode tooltip"""
    unicode_text = "工具提示"
    button = ImageButton(0, 0, pygame.Surface((10, 10)), lambda: None, unicode_text)
    assert button.tooltip == unicode_text

def test_image_button_tooltip_multiline():
    """Test multiline tooltip"""
    multiline_text = "Line 1\nLine 2\nLine 3"
    button = ImageButton(0, 0, pygame.Surface((10, 10)), lambda: None, multiline_text)
    assert button.tooltip == multiline_text

## Surface Tests (Expanded)
def test_image_button_with_different_surface_formats():
    """Test with different surface formats"""
    for depth in [8, 16, 24, 32]:
        surf = pygame.Surface((10, 10), depth=depth)
        button = ImageButton(0, 0, surf, lambda: None)
        assert button.image.get_bitsize() == depth

def test_image_button_with_alpha_channel():
    """Test with alpha channel surface"""
    surf = pygame.Surface((10, 10), pygame.SRCALPHA)
    button = ImageButton(0, 0, surf, lambda: None)
    assert button.image.get_flags() & pygame.SRCALPHA

def test_image_button_with_scaled_surface():
    """Test with scaled surface"""
    surf = pygame.Surface((100, 100))
    pygame.transform.scale(surf, (50, 50))
    button = ImageButton(0, 0, surf, lambda: None)
    assert button.image.get_size() == (100, 100)  # Should keep original size

def test_image_button_with_colored_surface():
    """Test with colored surface"""
    surf = pygame.Surface((10, 10))
    surf.fill((255, 0, 0))  # Red
    button = ImageButton(0, 0, surf, lambda: None)
    # Verify color at center pixel
    assert button.image.get_at((5, 5)) == (255, 0, 0, 255)

def test_image_button_with_empty_surface():
    """Test with empty surface (1x1 pixel)"""
    surf = pygame.Surface((1, 1))
    button = ImageButton(0, 0, surf, lambda: None)
    assert button.image.get_size() == (1, 1)

def test_image_button_with_transparent_surface():
    """Test with fully transparent surface"""
    surf = pygame.Surface((10, 10), pygame.SRCALPHA)
    surf.fill((0, 0, 0, 0))  # Fully transparent
    button = ImageButton(0, 0, surf, lambda: None)
    assert button.image.get_at((5, 5))[3] == 0  # Alpha channel

def test_image_button_with_sub_surface():
    """Test with subsurface"""
    parent = pygame.Surface((20, 20))
    subsurf = parent.subsurface((5, 5, 10, 10))
    button = ImageButton(0, 0, subsurf, lambda: None)
    assert button.image.get_size() == (10, 10)

## Additional Edge Cases
def test_image_button_with_very_large_surface():
    """Test with very large surface"""
    surf = pygame.Surface((1000, 1000))
    button = ImageButton(0, 0, surf, lambda: None)
    assert button.image.get_size() == (1000, 1000)

def test_image_button_with_very_small_surface():
    """Test with very small surface"""
    surf = pygame.Surface((1, 1))
    button = ImageButton(0, 0, surf, lambda: None)
    assert button.image.get_size() == (1, 1)

def test_image_button_action_with_closure():
    """Test action with closure variable"""
    value = 10
    def action():
        return value + 5
    button = ImageButton(0, 0, pygame.Surface((10, 10)), action)
    assert button.action() == 15

def test_image_button_with_pixel_alpha():
    """Test with per-pixel alpha surface"""
    surf = pygame.Surface((10, 10), pygame.SRCALPHA)
    for x in range(10):
        for y in range(10):
            surf.set_at((x, y), (x*25, y*25, 0, 255))
    button = ImageButton(0, 0, surf, lambda: None)
    assert button.image.get_at((5, 5)) == (125, 125, 0, 255)


def test_image_button_with_double_click():
    """Test double click action"""
    counter = 0
    def action():
        nonlocal counter
        counter += 1
    button = ImageButton(0, 0, pygame.Surface((10, 10)), action)
    button.action()
    button.action()
    assert counter == 2

def test_image_button_with_position_update():
    """Test updating position after creation"""
    button = ImageButton(0, 0, pygame.Surface((10, 10)), lambda: None)
    button.rect.topleft = (50, 60)
    assert button.rect.topleft == (50, 60)
