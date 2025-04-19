import sys
import os
from pathlib import Path

# Añade el directorio raíz al path de Python
sys.path.append(str(Path(__file__).parent.parent))

from python.heuristic_search.graph import EscapeRoomGraph
from python.heuristic_search.heuristic import manhattan_distance
from python.integration.hybrid_solver import HybridSolver

def main():
    graph = EscapeRoomGraph()
    graph.initialize_simple_graph()
    
    # El solver ahora encontrará automáticamente la carpeta prolog
    solver = HybridSolver(graph, manhattan_distance)
    path, moves = solver.solve('A', 'D', verbose=True)
    
    if path:
        print("\nSOLUCIÓN ENCONTRADA:")
        print(f"Camino: {' -> '.join(path)}")
        print(f"Movimientos: {moves}")
    else:
        print("No se encontró solución")

if __name__ == "__main__":
    main()