concatenate([], List, List).
concatenate([X|List1], List2, [X|List3]) :- 
  concatenate(List1, List2, List3).

max(A, B, X):-
  (A > B, X is A);
  (A =< B, X is B).

min(A, B, X):-
  (A < B, X is A);
  (A >= B, X is B).

shortestListOfTwo([List1, List2], List):-
  length(List1, L1),
  length(List2, L2),
  (
    (L1 <  L2, List = List1);
    (L1 >= L2, List = List2)
  ).

find_shortest_list(ListOfLists, List):-
  shortestListOfTwo(ListOfLists, List).

find_shortest_list(ListOfLists, List):-
  ListOfLists = [List1|T],
  find_shortest_list(T, List2),
  shortestListOfTwo([List1, List2], List).