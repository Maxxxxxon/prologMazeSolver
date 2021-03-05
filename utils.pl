concatenate([], L, L).
concatenate([X|L1], L2, [X|L3]) :-   %%% rewrite
  concatenate(L1, L2, L3).

max(A, B, X):-
    (A > B, X is A);
    (A =< B, X is B).


substitute(Value, NewValue, List, NewList):-
    delete(List, Value, List2),
    concatenate(List2, [NewValue], List).

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