# Escape Room Solver Report

## 📌 Introduction

This project solves an escape room-like maze using both logical and heuristic search algorithms. Implementations in **Prolog** and **Python** allow comparison between exhaustive (BFS) and heuristic-driven (A*) search strategies. The environment includes doors, keys, traps, coordinates, and constraints that increase the agent's challenge in navigating from a start to a goal node.

---

## ⚙️ Methodology

We followed an iterative methodology:

1. The maze was represented as a graph with logical and physical constraints.
2. A **BFS** solution was implemented in Prolog for completeness.
3. A **heuristic A\*** algorithm was implemented in Python for efficiency.
4. We created a **Prolog-Python bridge** using `pyswip`, allowing Python to call Prolog predicates directly.
5. The system supports dynamic map loading using `.json` files.

---

## 🧩 Implementation

### 📁 Project Structure

![image](https://github.com/user-attachments/assets/af736137-34ac-4db2-b05a-06365f5714da)

## 🔗 Prolog-Python Bridge

Implemented via the [`pyswip`](https://github.com/yuce/pyswip) library, allowing Python scripts to load `.pl` files and execute Prolog queries programmatically. This enables a hybrid approach where logic reasoning and UI/heuristic computation coexist.

---

## ⚙️ Algorithms

### 🔍 BFS (Breadth-First Search in Prolog)

- Fully exhaustive and complete.
- Navigates while respecting constraints: locked doors, needed keys, and traps.
- Can be **very memory intensive**, recommended to run with increased stack limit:

```bash
swipl --stack-limit=8g
```

### 🔸 A* (A-Star Search in Python)
Uses Manhattan Distance as a heuristic.
```python
def manhattan_heuristic(current, goal):
    x1, y1 = room_coords.get(current, (0, 0))
    x2, y2 = room_coords.get(goal, (0, 0))
    return abs(x1 - x2) + abs(y1 - y2)
```


Considers movement cost, inventory limits, key-door dependencies, and traps.

Significantly faster for large maps or constraint-heavy environments.


#### 🧪 Results
|Algorithm | Language | Completeness | Optimality | Performance|
|----------|----------|------------|------------|-------------|
|BFS | Prolog | ✅ Yes | ✅ Yes | ❌ Slow on large maps|
|A* | Python | ✅ Yes | ✅ Yes | ✅ Fast and memory-efficient|

----

### 🤖 [Placeholder: Adversarial Search]
This section is reserved for future implementation using Adversarial Search with Minimax or Alpha-Beta Pruning techniques to simulate competitive escape scenarios or dynamic threats.

-----

## ▶️ Running the Game

### From prolog

```bash
swipl --stack-limit=8g

?- [prolog/facts].
?- [prolog/rules].
?- [prolog/search].

% Sample execution
?- take_key(a).
?- unlock_door(a, b).
?- bfs(a, e, Path)
```
> ⚠️ Warning: BFS requires a lot of memory due to path branching and constraint checking. Always use the **--stack-limit=8g** flag.

### From python 🐍

```bash
python main.py
```

You'll see the main menu:

```bash
===== Initial Menu =====
1. BFS (Prolog)
2. A* (Python)
0. Exit
Select an option: 
```
> It's very intuitive!

Depending on your selection:
- Option 1 will run the BFS search using Prolog via the bridge.
- Option 2 runs the A* algorithm with optional path visualization, map loading, or goal selection.

✅ Conclusions
- Successfully implemented a solver using both logical and heuristic search.

- The agent can navigate a maze with complex real-world constraints.

- The bridge between Prolog and Python offers flexibility and a great learning opportunity.

- A* shows better performance in constrained environments but requires a well-designed heuristic.

- The architecture allows easy extension, testing, and future AI integrations.
