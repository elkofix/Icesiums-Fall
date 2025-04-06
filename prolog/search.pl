:- module(search, [
    dfs/3,
    bfs/3
]).

:- use_module(rules).

% DFS
dfs(Start, Goal, Path) :-
    dfs_helper(Start, Goal, [Start], Path).

dfs_helper(Goal, Goal, Path, Path).
dfs_helper(Current, Goal, Visited, Path) :-
    rules:can_move(Current, Next),
    \+ member(Next, Visited),
    dfs_helper(Next, Goal, [Next | Visited], Path).

% BFS
bfs(Start, Goal, Path) :-
    bfs_helper([[Start]], Goal, RevPath),
    reverse(RevPath, Path).

bfs_helper([[Goal | Rest] | _], Goal, [Goal | Rest]).
bfs_helper([CurrentPath | Paths], Goal, FinalPath) :-
    CurrentPath = [CurrentRoom | _],
    findall([Next, CurrentRoom | Rest],
        (rules:can_move(CurrentRoom, Next), \+ member(Next, CurrentPath)),
        NewPaths),
    append(Paths, NewPaths, Queue),
    bfs_helper(Queue, Goal, FinalPath).
