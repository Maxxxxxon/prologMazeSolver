:-ensure_loaded(utils).
:-ensure_loaded(input).

adjacent_cells(Cell1, Cell2) :-
    Cell2 = [X2, Y2],
    Cell1 = [X1, Y1],
    (X2 is X1 + 1; X2 is X1; X2 is X1 - 1),
    (Y2 is Y1 + 1; Y2 is Y1; Y2 is Y1 - 1),
    restrictions(L),
    X2 > 0, X2 =< L,
    Y2 > 0, Y2 =< L, 
    Cell1 \= Cell2.

covidAdjacentD1(X) :-
    covid(Y),
    adjacent_cells(Y, X).

is_covid_safe(Cell, CompletedPath):-
    (   
      (covid(Cell); covidAdjacentD1(Cell)),
      (doctor(X), member(X, CompletedPath); mask(Y), member(Y, CompletedPath))
    );
    (
        not(covid(Cell)), 
        not(covidAdjacentD1(Cell))
    ).

path(X, X, _, []) :- true.
path(From, To, CompletedPath, FuturePath):-
    restrictions(L),
    length(CompletedPath, Len),
    Len =< L * 2,
    adjacent_cells(From, Next),
    is_covid_safe(Next, CompletedPath),
    not(member(Next, CompletedPath)),
    concatenate(CompletedPath, [Next], NewCompletedPath),
    path(Next, To, NewCompletedPath, FuturePath2),
    FuturePath = [Next|FuturePath2].
    

find_some_way(Path) :-
    home(Home),
    path([1, 1], Home, [[1, 1]], Path).

find_shortest_way(Path) :-
    home(Home),
    bagof(PathP, path([1, 1], Home, [[1, 1]], PathP), L),
    find_shortest_list(L, Path2),
    concatenate([[1, 1]], Path2, Path).

backtracking_main :-
    get_time(T1),    
    find_shortest_way(Path),
    get_time(T2),
    write("Path: "),
    write(Path),
    write("\nPath length: "),
    length(Path, N),
    write(N),
    write("\nTime spend: "),
    T3 is T2 - T1,
    write(T3).