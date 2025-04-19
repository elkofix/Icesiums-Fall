def manhattan_distance(graph, room1, room2):
    x1, y1 = graph.get_room(room1).coordinates
    x2, y2 = graph.get_room(room2).coordinates
    return abs(x1 - x2) + abs(y1 - y2)
