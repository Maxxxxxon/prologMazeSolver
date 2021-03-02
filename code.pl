covid([2, 6]).
covid([4, 2]).

doctor([5, 1]).
home([9, 9]).
mask([1, 4]).

adjacent_cells(Cell1, Cell2) :-
    Cell2 = [X2, Y2],
    Cell1 = [X1, Y1],
    (X2 is X1 + 1; X2 is X1; X2 is X1 - 1),
    (Y2 is Y1 + 1; Y2 is Y1; Y2 is Y1 - 1),
    X2 > 0, X2 < 10,
    Y2 > 0, Y2 < 10, 
    Cell1 \= Cell2.

covidAdjacentD1(X) :-
    covid(Y),
    adjacent_cells(Y, X).

concatenate([], L, L).
concatenate([X|L1], L2, [X|L3]) :-   %%% rewrite
  concatenate(L1, L2, L3).

covid_safe(Cell, CompletedPath):-
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
    adjacent_cells(From, Next),
    covid_safe(Next, CompletedPath),
    not(member(Next, CompletedPath)),
    concatenate(CompletedPath, [Next], NewCompletedPath),
    path(Next, To, NewCompletedPath, FuturePath2),
    FuturePath = [Next|FuturePath2].
    

main(Path) :-
    path([1, 1], [9, 1], [[1, 1]], Path).
