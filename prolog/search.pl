% --- DFS ---
dfs(Start, Goal, Path) :-
    dfs_helper(Start, Goal, [Start], RevPath),
    reverse(RevPath, Path).

dfs_helper(Goal, Goal, Path, Path).
dfs_helper(Current, Goal, Visited, Path) :-
    can_move(Current, Next),
    \+ member(Next, Visited),
    dfs_helper(Next, Goal, [Next|Visited], Path).

% --- BFS ---
bfs(Start, Goal, Path) :-
    bfs_helper([[Start]], Goal, RevPath),
    reverse(RevPath, Path).

bfs_helper([[Goal|Rest]|_], Goal, [Goal|Rest]).
bfs_helper([CurrentPath|OtherPaths], Goal, FinalPath) :-
    CurrentPath = [CurrentNode|_],
    findall([Next,CurrentNode|Rest],
        (can_move(CurrentNode, Next), \+ member(Next, CurrentPath)),
        NewPaths),
    append(OtherPaths, NewPaths, UpdatedPaths),
    bfs_helper(UpdatedPaths, Goal, FinalPath).
