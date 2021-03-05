:- ensure_loaded(utils).

covid([2, 6]).
covid([4, 2]).

doctor([5, 1]).
home([9, 9]).
mask([1, 4]).

size([5, 4]).

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


initialize_cost(FirstValue, Cost) :-
    size([X, Y]),
    FirstValue =:= X * Y,
    Cost = [],
    !.

initialize_cost(FirstValue, Cost) :-
    size([XMax, YMax]),
    X is FirstValue mod XMax + 1,
    Y is FirstValue // XMax + 1,
    NextValue is FirstValue + 1,
    initialize_cost(NextValue, CostNew),
    Cost = [[[X, Y], 999999] | CostNew].

heuristic(Cell, Ans) :- 
    Cell = [X, Y],
    home(Home),
    Home = [HomeX, HomeY],
    abs(HomeX - X, Xdif), abs(HomeY - Y, Ydif),
    max(Xdif, Ydif, Ans).

distance(Cell, Cost, Ans) :-
    get_value_from_cell(Cell, Cost, CellValue),
    heuristic(Cell, HeuristicValue),
    Ans is CellValue - HeuristicValue.

get_value_from_cell(Cell, Cost, Value) :-
    nth0(_, Cost, [Cell, Value]).
    
% get_minimal_value(Value_list, Value):-

updateAdjacent([], _, _, _, _, _, _).
updateAdjacent(AdjacentVerteces, Cell, ListQ, ListU, ListQNew, ListUNew, ParentList, CostList) :-
    AdjacentVerteces = [Vertex|Tail],
    get_value_from_cell(Cell, CostList, CellValue),
    Score is Value + 1,
    ((
        member(Cell, ListU),
        distance(Vertex, CostList, VertexValue),
        Score >= VertexValue,
        updateAdjacent(Tail, Cell, ListQ, ListU, ListQNew, ListUNew, CostList)
     );
     ( 
        distance(Vertex, CostList, VertexValue),
        (not(member(Cell, ListU)); Score < VertexValue),
        setListValue(ParentList, Vertex, Cell),            %% list, index, value
        heuristic(Vertex, VertexHeuristic),
        NewVertexCost is VertexValue + VertexHeuristic,
        set_value_for_cell(Vertex, NewVertexCost, CostList, CostListNew),
        ((
            member(Vertex, ListQ),
            ListQ1 = ListQ
        );
        (
            ListQ1 = [Vertex|ListQ]
        )),
        updateAdjacent(Tail, Cell, ListQ1, ListU, ListQNew, ListUNew, ParentList, CostList)
    )).
    

astar(ListQ, ListU, Cost):-
    get_minimal_value(ListQ, Vertex),
    Vertex = [Cell, Value],
    (
        home(Cell);
        (
            delete_value(ListQ, Vertex, ListQNew),
            add_value(ListU, Vertex, ListUNew),
            bagof(Vert, adjacent_cells(Cell, Vert), AdjacentVerteces),
            updateAdjacent(AdjacentVerteces, Cell, ListQNew, ListUNew, ListQNew2, ListUNew2, Cost),
            astar(ListQNew2, ListUNew2)
        )
    ).


main(Path) :-
    initialize_cost(0, Cost),
    heuristic([1, 1], H),
    substitute([[1, 1], _], [[1, 1], H], Cost, Cost2),

    astar([[1, 1]], [], Cost2).           %% list of Q -- to consider, list of U -- considered, Cost List - distance + heuristic
