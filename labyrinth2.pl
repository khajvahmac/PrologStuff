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

solution(CurrentBlock, Goal, PathAlreadyPassed, Solution) :- 
    solutionImpl(CurrentBlock, Goal, PathAlreadyPassed, [], [], Solution).

    solutionImpl(Goal, Goal, _, Path, _, Solution) :- nextPath(Path, Goal, [], Solution).
    solutionImpl(CurrentBlock, Goal, PathAlreadyPassed, Path, Queue, Solution) :- 
        nextConnected(CurrentBlock, PathAlreadyPassed, RNext), 
        append(RNext, Queue, [Y|NewQueue]),
        nextPath(Path, CurrentBlock, [], NewPath),
        solutionImpl(Y, Goal, [CurrentBlock|PathAlreadyPassed], NewPath, NewQueue, Solution).
  
iterWrite(E, E, _, _).
iterWrite(Block, Exit, S, S) :- write('\n|'), iterWrite(Block, Exit, S, 0).
iterWrite(Block, Exit, Size, Index) :- 
    (((X is Block + Size, connected(Block, X); X is Block + Size, connected(X, Block)), write(' ');
    write('_')), 
    ((Y is Block + 1, connected(Block, Y); Y is Block + 1, connected(Y, Block)), write(' ');
    write('|'))), F is Block + 1, Z is Index + 1, iterWrite(F, Exit, Size, Z).

repeat(Str,1,Str).
repeat(Str,Num,Res):-
    Num1 is Num-1,
    repeat(Str,Num1,Res1),
    string_concat(Str, Res1, Res).

draw() :- 
    entrance(X),
    exit(Y), Z is Y + 1,
    size(S), repeat(' _', S, Res), write(Res), write('\n '),
    iterWrite(X, Z, S, 0).
