import os
from integration.prolog_Bridge import PrologBridge
from A_star.a_star import a_star_escape
from A_star.load_map import load_map_from_json
from A_star import map as game_map
import tkinter as tk
from tkinter import filedialog

default_map = {
    "doors": game_map.doors,
    "keys_in_rooms": game_map.keys_in_rooms,
    "room_coords": game_map.room_coords,
    "trap_room": game_map.trap_room,
    "trap_limit": game_map.trap_limit,
    "inventory_limit": game_map.inventory_limit,
    "max_moves": game_map.max_moves,
    "room_costs": game_map.room_costs,
}

def display_main_menu():
    print("\n===== Menú Inicial =====")
    print("1. BFS (Prolog)")
    print("2. A* (Python)")
    print("0. Salir")
    return input("Seleccione una opción: ")

# ================== PROLOG MENU ==================

def display_prolog_menu():
    print("\n=== Escape Room Solver (Prolog - BFS) ===")
    print("1. Cargar juego predefinido")
    print("2. Crear juego personalizado")
    print("3. Buscar solución de escape")
    print("4. Mostrar estado actual")
    print("0. Volver al menú inicial")
    return input("Seleccione una opción: ")

def display_solution(steps, cost=0):
    if steps:
        print("\n=== Plan de Escape ===")
        for i, step in enumerate(steps, 1):
            print(f"{i}. {step}")
        print(f"\nTotal de pasos requeridos: {len(steps)}")
        print(f"Costo total: {cost}")
    else:
        print("\nNo se encontró una solución válida para escapar")


def prolog_mode():
    solver = PrologBridge()
    current_state = None

    while True:
        choice = display_prolog_menu()

        if choice == '1':
            if solver.initialize_game(1):
                current_state = solver.get_current_state()
                print("\nJuego predefinido cargado correctamente!")
                print(f"Ubicación actual: {current_state['room']}")
            else:
                print("Error al cargar el juego predefinido")

        elif choice == '2':
            if solver.initialize_game(2):
                current_state = solver.get_current_state()
                print("\nJuego personalizado creado correctamente!")
                print(f"Ubicación actual: {current_state['room']}")
            else:
                print("Error al crear el juego personalizado")

        elif choice == '3':
            if not current_state:
                print("Primero debe cargar o crear un juego!")
                continue

            print("\nBuscando solución óptima (BFS - Prolog)...")
            solution = solver.find_escape_plan()
            display_solution(solution)

        elif choice == '4':
            if current_state:
                print("\n=== Estado Actual ===")
                print(f"Habitación: {current_state['room']}")
                print(f"Inventario: {current_state['inventory']}")
                print(f"Puzzles resueltos: {current_state['solved_puzzles']}")
            else:
                print("Primero debe cargar o crear un juego!")

        elif choice == '0':
            print("Volviendo al menú inicial...")
            break

        else:
            print("Opción no válida. Intente nuevamente.")

# ================== A* MENU ==================

def a_star_menu():
    print("\n=== Escape Room Solver (Python - A*) ===")
    print("1. Ejecutar A* desde A hasta H")
    print("2. Ejecutar A* desde cualquier nodo")
    print("3. Cargar mapa personalizado")
    print("0. Volver al menú inicial")
    return input("Seleccione una opción: ")

def a_star_mode():
    while True:
        choice = a_star_menu()

        if choice == '1':
            path, cost = a_star_escape("A", "H", default_map)
            display_solution(path, cost)

        elif choice == '2':
            start = input("Ingrese el nodo de inicio: ").strip().upper()
            goal = input("Ingrese el nodo destino: ").strip().upper()
            path, cost = a_star_escape(start, goal, default_map)
            display_solution(path, cost)
        elif choice == '3':
            print("Seleccione el archivo JSON del mapa...")
            root = tk.Tk()
            root.withdraw()  # Oculta la ventana principal

            file_path = filedialog.askopenfilename(
                title="Seleccionar archivo de mapa",
                filetypes=[("Archivos JSON", "*.json")]
            )

            if not file_path:
                print("No se seleccionó ningún archivo.")
                continue

            try:
                custom_map = load_map_from_json(file_path)

                # Reemplazar valores en el módulo map
                game_map.doors = custom_map["doors"]
                game_map.keys_in_rooms = custom_map["keys_in_rooms"]
                game_map.room_coords = custom_map["room_coords"]
                game_map.trap_room = custom_map["trap_room"]
                game_map.trap_limit = custom_map["trap_limit"]
                game_map.inventory_limit = custom_map["inventory_limit"]
                game_map.max_moves = custom_map["max_moves"]
                game_map.room_costs = custom_map["room_costs"]

                print("Mapa cargado correctamente.")
                start = input("Ingrese el nodo de inicio: ").strip().upper()
                goal = input("Ingrese el nodo destino: ").strip().upper()
                path, cost = a_star_escape(start, goal, custom_map)
                display_solution(path, cost)

            except Exception as e:
                print(f" Error cargando el archivo: {e}")

        elif choice == '0':
            print("Volviendo al menú inicial...")
            break

        else:
            print("Opción no válida. Intente nuevamente.")

# ================== MAIN ==================

def main():
    while True:
        choice = display_main_menu()

        if choice == '1':
            prolog_mode()

        elif choice == '2':
            a_star_mode()

        elif choice == '0':
            print("Saliendo del programa...")
            break

        else:
            print("Opción no válida. Intente nuevamente.")

if __name__ == "__main__":
    main()
