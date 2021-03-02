concatenate([], L, L).
concatenate([X|L1], L2, [X|L3]) :-   %%% rewrite
  concatenate(L1, L2, L3).

max(A, B, X):-
    (A > B, X is A);
    (A =< B, X is B).