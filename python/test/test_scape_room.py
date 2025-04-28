import pytest
import pygame
from main import EscapeRoomGUI, ImageButton
from integration.prolog_Bridge import PrologBridge

# Fixture para inicializar pygame una sola vez
@pytest.fixture(scope="module")
def pygame_init():
    pygame.init()
    pygame.display.set_mode((800, 600))
    yield
    pygame.quit()

# Fixture para la instancia del juego
@pytest.fixture
def game_instance(pygame_init):
    bridge = PrologBridge()
    return EscapeRoomGUI(bridge)

def test_game_flow(game_instance, mocker):
    # Configurar mocks
    mocker.patch.object(game_instance.state, 'move_player', return_value=True)
    mocker.patch.object(game_instance.state, 'pick_key', return_value=True)
    mocker.patch.object(game_instance.state, 'unlock_door', return_value=True)
    
    # Iniciar juego
    game_instance.start_standard_game()
    game_instance.load_predefined_game()
    
    # Simular flujo del juego
    initial_room = game_instance.current_room
    game_instance.move_player("b")
    assert game_instance.current_room == "b"
    
    game_instance.pick_key("key1")
    assert "key1" in game_instance.inventory
    
    game_instance.unlock_door("b", "c")
    assert any(door["state"] == "unlocked" for door in game_instance.doors)

def test_image_button_initialization():
    test_image = pygame.Surface((50, 50))
    button = ImageButton(100, 100, test_image, lambda: None, "Test")
    assert button.rect.center == (100, 100)
    assert button.tooltip == "Test"

def test_image_button_click(mocker):
    mock_action = mocker.Mock()
    test_image = pygame.Surface((50, 50))
    button = ImageButton(100, 100, test_image, mock_action)
    
    # Simular click
    button.is_clicked((100, 100))
    mock_action.assert_called_once()

def test_error_handling(game_instance, mocker):
    # Mockear para simular error al cargar juego
    mocker.patch.object(game_instance.facts, 'load_predefined_game', return_value=False)
    
    game_instance.start_standard_game()
    game_instance.load_predefined_game()
    assert game_instance.game_initialized is False
    assert "Failed" in game_instance.output_text[0]