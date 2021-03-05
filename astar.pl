:- ensure_loaded(utils).

covid([1, 4]).
covid([2, 4]).

doctor([5, 1]).
home([4, 4]).
mask([5, 4]).

restrictions(4).

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

initialize_cost_and_parent(FirstValue, CostList, ParentList) :-
    restrictions(L),
    FirstValue =:= L * L,
    CostList = [],
    ParentList = [],
    !.

initialize_cost_and_parent(FirstValue, CostList, ParentList) :-
    restrictions(L),
    X is FirstValue mod L + 1,
    Y is FirstValue // L + 1,
    NextValue is FirstValue + 1,
    initialize_cost_and_parent(NextValue, CostNew, ParentNew),
    CostList = [[[X, Y], 999999] | CostNew],
    ParentList = [[[X, Y], [999999, 999999]] | ParentNew].

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

updateListValue(Index, Value, OldList, NewList) :-
    delete(OldList, [Index, _], MiddleList),
    concatenate(MiddleList, [Index, Value], NewList).

get_minimal_value(_, [], [[10, 10], 999999]).
get_minimal_value(KeyList, CostList, MinimalVertex):-
    CostList = [[Cell, Value]|Tail],
    get_minimal_value(KeyList, Tail, TailMinimalVertex),
    TailMinimalVertex = [_, TailMinimalValue],
    ((
        member(Cell, KeyList),
        TailMinimalValue >= Value,
        MinimalVertex = [Cell, Value],
        !
    );
    (
        (not(member(Cell, KeyList)); TailMinimalValue < Value), 
        MinimalVertex = TailMinimalVertex
    )).


updateAdjacent([], _, ListQ, ListQ, _, ParentList, ParentList, CostList, CostList).
updateAdjacent(AdjacentVerteces, Cell, ListQ, ListQNew, ListU, ParentList, ParentListNew, CostList, CostListNew) :-
    AdjacentVerteces = [Vertex|Tail],
    distance(Cell, CostList, CellDistance),
    Score is CellDistance + 1,
    ((
        member(Cell, ListU),
        distance(Vertex, CostList, VertexDistance),
        Score >= VertexDistance,
        updateAdjacent(Tail, Cell, ListQ, ListQNew, ListU, ParentList, ParentListNew, CostList, CostListNew)
     );
     ( 
        distance(Vertex, CostList, VertexDistance),
        (not(member(Cell, ListU)); Score < VertexDistance),
        updateListValue(Vertex, Cell, ParentList, ParentList1),            % index, value, oldlist, newlist
        heuristic(Vertex, VertexHeuristic),
        NewVertexCost is Score + VertexHeuristic,
        updateListValue(Vertex, NewVertexCost, CostList, CostList1),
        ((
            member(Vertex, ListQ),
            ListQ1 = ListQ
        );
        (
            ListQ1 = [Vertex|ListQ]
        )),
        updateAdjacent(Tail, Cell, ListQ1, ListQNew, ListU, ParentList1, ParentListNew, CostList1, CostListNew)
    )).
    

astar(ListQ, ListU, CostList, ParentList, ParentListNew):-
    get_minimal_value(ListQ, CostList, Vertex),     % get the cell from Q which has the smallest value in costlist
    Vertex = [Cell, _],
    (
        (
            home(Cell),
            ParentListNew = ParentList
        );
        (
            delete(ListQ, Cell, ListQ1),
            ListU1 = [Cell|ListU],
            bagof(AdjCell, adjacent_cells(Cell, AdjCell), AdjacentCells),
            updateAdjacent(AdjacentCells, Cell, ListQ1, ListQNew, ListU1, ParentList, ParentList1, CostList, CostList1),
            astar(ListQNew, ListU1, CostList1, ParentList1, ParentListNew)
        )
    ).

computeParentList(ParentListNew) :-
    initialize_cost_and_parent(0, CostList, ParentList),
    heuristic([1, 1], H),
    substitute([[1, 1], _], [[1, 1], H], CostList, CostList2),

    astar([[1, 1]], [], CostList2, ParentList, ParentListNew).           %% list of Q -- to consider, list of U -- considered, Cost List - distance + heuristic

main(X) :-
    computeParentList(X).