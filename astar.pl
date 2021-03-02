 :- ensure_loaded(utils).

covid([2, 6]).
covid([4, 2]).

doctor([5, 1]).
home([9, 9]).
mask([1, 4]).

size([9, 9]).

initialize_values(FirstValue, Value_list) :-
    size([X, Y]),
    FirstValue =:= X * Y,
    Value_list = [],
    !.

initialize_values(FirstValue, Value_list) :-
    size([XMax, YMax]),
    X is FirstValue mod XMax + 1,
    Y is FirstValue // XMax + 1,
    NextValue is FirstValue + 1,
    initialize_values(NextValue, New_Value_list),
    Value_list = [[[X, Y], 999999] | New_Value_list].

heuristic(Cell, Ans) :- 
    Cell = [X, Y],
    home(Home),
    Home = [HomeX, HomeY],
    abs(HomeX - X, Xdif), abs(HomeY - Y, Ydif),
    max(Xdif, Ydif, Ans).

get_heuristic_value(Cell, Value_list, Value) :-
    nth0(_, Value_list, [Cell, Value]).
    

find_next(Cell, Next) :- 


astar(X, X, _, []) :- true.
astar(From, To, CompletedPath, FuturePath):-
    find_next(From, Next),
    covid_safe(Next, CompletedPath),
    not(member(Next, CompletedPath)),
    concatenate(CompletedPath, [Next], NewCompletedPath),
    astar(Next, To, NewCompletedPath, FuturePath2),
    FuturePath = [Next|FuturePath2].

main(Path) :-
    initialize_values(Value_list)
    astar([1, 1], [9, 1], [[1, 1]], Path).
