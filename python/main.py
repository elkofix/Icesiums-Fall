from heuristic_search.graph import EscapeRoomGraph
from heuristic_search.solver import a_star_escape

if __name__ == "__main__":
    g = EscapeRoomGraph()
    g.initialize_simple_graph()
    g.visualize()

    path, cost = a_star_escape(g, 'A', 'D')
    print("Camino óptimo:", path)
    print("Costo:", cost)
