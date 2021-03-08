%%% Please pay attention that bactracking requires ~20 seconds to find the way on 5x5 case,
%%% and too much time for larger cases
%%%
%%% In case the backtracking works on your machine too long even for provided case 5x5,
%%% I suggest you to test it on 4x4 case





% covid([2, 5]).
% covid([4, 1]).
% doctor([6, 6]).
% home([4, 5]).
% mask([2, 1]).
% restrictions(5).

% corresponds to
% [I C I H -]
% [I I I - -]
% [- - - - -]
% [- - I I I]
% [- M I C I]
% C - covid, I - infected cells, 
% H - home, M - mask, D - doctor
% restrictions - size of the map





% uncomment these 6 lines for usage, and comment other inputs

% covid([3, 1]).
% covid([4, 1]).
% doctor([2, 4]).
% home([4, 3]).
% mask([1, 4]).
% restrictions(4).

% corresponds to
% [M D - -]
% [- - - H]
% [- I I I]
% [- I C C]





% uncomment these 6 lines for usage, and comment other inputs

% covid([7, 5]).
% covid([7, 2]).
% doctor([1, 9]).
% home([9, 1]).
% mask([5, 5]).
% restrictions(9).

% corresponds to              Shortest path
% [- - - - - - - - -]       [- - - - - - - - -]
% [- - - - - - - - -]       [- - - - - - - - -]
% [- - - - - - - - -]       [- - - - - - - - -]
% [- - - - - I I I -]       [- - - - - - - - -]
% [- - - - M I C I -]       [- - - - P - - - -]
% [- - - - - I I I -]       [- - - P - P - - -]
% [- - - - - I I I -]       [- - P - - - P - -]
% [- - - - - I C I -]       [- P - - - - - P -]
% [- - - - - I I I H]       [P - - - - - - - P]





% uncomment these 6 lines for usage, and comment other inputs

covid([7, 5]).
covid([7, 9]).
doctor([1, 9]).
home([9, 9]).
mask([2, 9]).
restrictions(9).

% corresponds to
% [- - - - - I C I H]       [- - - - - - - - P]
% [- - - - - I I I -]       [- - - - - - - - P]
% [- - - - - - - - -]       [- - - - - P P P -]
% [- - - - - I I I -]       [- - - - P - - - -]
% [- - - - - I C I -]       [- - - - P - - - -]
% [- - - - - I I I -]       [- - - P - - - - -]
% [- - - - - - - - -]       [- - P - - - - - -]
% [- - - - - - - - -]       [- P - - - - - - -]
% [- - - - - - - - -]       [P - - - - - - - -]