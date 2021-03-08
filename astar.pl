:- ensure_loaded(utils).
:- ensure_loaded(input).
initial_position([1, 1, 0]).


adjacent_cells_actor(Cell1, Cell2) :-
    Cell1 = [X1, Y1, Covid_level], 
    (X2 is X1 + 1; X2 is X1; X2 is X1 - 1),
    (Y2 is Y1 + 1; Y2 is Y1; Y2 is Y1 - 1),
    restrictions(L),
    X2 > 0, X2 =< L,
    Y2 > 0, Y2 =< L, 
    (
        (
            not((mask([X2, Y2]); doctor([X2, Y2]))),
            Cell2 = [X2, Y2, Covid_level]
        );
        (
            (mask([X2, Y2]); doctor([X2, Y2])),
            Cell2 = [X2, Y2, 1]
        )
    ),
    Cell1 \= Cell2 .



adjacent_cells_covid(Cell1, Cell2) :-
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
    adjacent_cells_covid(Y, X).

allowed_move(Cell1, Cell2) :-
    adjacent_cells_actor(Cell1, Cell2),
    Cell2 = [X, Y, Covid_level],
    (
        (
            covidAdjacentD1([X, Y]),
            Covid_level = 1
        );
        not(covidAdjacentD1([X, Y]))
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
    CostList = [[[X, Y, 0], 99], [[X, Y, 1], 99] | CostNew],
    ParentList = [[[X, Y, 0], [99, 99]], [[X, Y, 1], [99, 99]] | ParentNew].

heuristic(Cell, Ans) :- 
    Cell = [X, Y, _],
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
    concatenate(MiddleList, [[Index, Value]], NewList).

get_minimal_value(_, [], [[10, 10, 0], 99]).
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
            Cell = [X, Y, _],
            home([X, Y]),
            ParentListNew = ParentList
        );
        (
            delete(ListQ, Cell, ListQ1),
            ListU1 = [Cell|ListU],
            bagof(AdjCell, allowed_move(Cell, AdjCell), AdjacentCells),
            updateAdjacent(AdjacentCells, Cell, ListQ1, ListQNew, ListU1, ParentList, ParentList1, CostList, CostList1),
            astar(ListQNew, ListU1, CostList1, ParentList1, ParentListNew)
        )
    ).

computeParentList(ParentListNew) :-
    initialize_cost_and_parent(0, CostList, ParentList),
    initial_position(InitialPosition),
    heuristic(InitialPosition, Heuristic),
    updateListValue(InitialPosition, Heuristic, CostList, CostList2),

    astar([InitialPosition], [], CostList2, ParentList, ParentListNew).           %% list of Q -- to consider, list of U -- considered, Cost List - distance + heuristic

recursiveRestorePath(Cell, _, Path) :-
    initial_position(Cell),
    Path = [].

recursiveRestorePath(Cell, ParentList, Path) :-
    get_value_from_cell(Cell, ParentList, ParentCell),
    ParentCell = [X, Y, _],
    recursiveRestorePath(ParentCell, ParentList, Path1),
    concatenate(Path1, [[X, Y]], Path).

restorePath(ParentList, Path) :-
    home([X, Y]),
    recursiveRestorePath([X, Y, 0], ParentList, Covid0Path),
    concatenate(Covid0Path, [[X, Y]], Covid0Path1),
    recursiveRestorePath([X, Y, 1], ParentList, Covid1Path),
    concatenate(Covid1Path, [[X, Y]], Covid1Path1),
    length(Covid1Path1, N1), length(Covid0Path1, N2),
    (
        N1 < N2,
        Path = Covid1Path1;
        N1 >= N2,
        Path = Covid0Path1
    ).

astar_main :-
    get_time(T1),    
    computeParentList(ParentList),
    restorePath(ParentList, X),
    get_time(T2),
    write("Path: "),
    write(X),
    write("\nPath length: "),
    length(X, N),
    write(N),
    write("\nTime spend: "),
    T3 is T2 - T1,
    write(T3).  