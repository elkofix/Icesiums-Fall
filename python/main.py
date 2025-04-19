from integration.prolog_Bridge import PrologBridge

def display_menu():
    print("\n=== Escape Room Solver ===")
    print("1. Cargar juego predefinido")
    print("2. Crear juego personalizado")
    print("3. Buscar solución de escape")
    print("4. Mostrar estado actual")
    print("5. Salir")
    return input("Seleccione una opción: ")

def display_solution(steps):
    if steps:
        print("\n=== Plan de Escape ===")
        for i, step in enumerate(steps, 1):
            print(f"{i}. {step}")
        print(f"\nTotal de pasos requeridos: {len(steps)}")
    else:
        print("\nNo se encontró una solución válida para escapar")

def main():
    solver = PrologBridge()
    current_state = None
    
    while True:
        choice = display_menu()
        
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
                
            print("\nBuscando solución óptima...")
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
        
        elif choice == '5':
            print("Saliendo del programa...")
            break
        
        else:
            print("Opción no válida. Por favor intente nuevamente.")

if __name__ == "__main__":
    main()