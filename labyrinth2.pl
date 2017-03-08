member(X,[X|_]).
member(X,[_|T]) :- member(X,T).

notInVisited([], _, R, R).
notInVisited([X|L], Visited, R, Res) :- 
    (\+member(X, Visited), notInVisited(L, Visited, [X|R], Res)); 
    notInVisited(L, Visited, R, Res).

nextConnected(Block, Visited, Result) :- 
    findall(X, connected(Block, X), LR), 
    findall(X, connected(X, Block), LF), append(LR, LF, R),
    notInVisited(R, Visited,[], Result). 

%Fake stop condition
nextPath([], Next, R, [Next|R]).
nextPath([X|CurrentPath], Next, R, Result) :-
    ((connected(X, Next); connected(Next, X)), nextPath([], Next, [X|CurrentPath], Result)); 
    nextPath(CurrentPath, Next, R, Result).

solution(CurrentBlock, Goal, PathAlreadyPassed, [Goal|Solution]) :- 
    solutionImpl(CurrentBlock, Goal, PathAlreadyPassed, [], [], Solution), write([Goal|Solution]).

    solutionImpl(Goal, Goal, _, Path, _, Path).
    solutionImpl(CurrentBlock, Goal, PathAlreadyPassed, Path, Queue, Solution) :- 
        nextConnected(CurrentBlock, PathAlreadyPassed, RNext), 
        append(RNext, Queue, [Y|NewQueue]),
        nextPath(Path, CurrentBlock, [], NewPath),
        solutionImpl(Y, Goal, [CurrentBlock|PathAlreadyPassed], NewPath, NewQueue, Solution).
  

