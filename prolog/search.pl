% DFS y BFS sobre el estado actual del mundo (sólo puertas desbloqueadas)

% --- Movimiento permitido según puertas desbloqueadas ---
connected(X, Y) :- door(X, Y, unlocked).
connected(X, Y) :- door(Y, X, unlocked).

% --- DFS (Depth-First Search) ---
dfs(Start, Goal, Path) :-
    dfs_helper(Start, Goal, [Start], RevPath),
    reverse(RevPath, Path).

dfs_helper(Goal, Goal, Path, Path).
dfs_helper(Current, Goal, Visited, Path) :-
    connected(Current, Next),
    \+ member(Next, Visited),
    dfs_helper(Next, Goal, [Next|Visited], Path).

% --- BFS (Breadth-First Search) ---
bfs(Start, Goal, Path) :-
    bfs_helper([[Start]], Goal, RevPath),
    reverse(RevPath, Path).

bfs_helper([[Goal|Rest]|_], Goal, [Goal|Rest]).
bfs_helper([CurrentPath|OtherPaths], Goal, FinalPath) :-
    CurrentPath = [CurrentNode|_],
    findall([Next,CurrentNode|Rest],
        (connected(CurrentNode, Next), \+ member(Next, CurrentPath)),
        NewPaths),
    append(OtherPaths, NewPaths, UpdatedPaths),
    bfs_helper(UpdatedPaths, Goal, FinalPath).
