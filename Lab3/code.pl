% Author: Johanna Schubert & Malin Hall
% Date: 10/12-2020

% Read input file -
% T - Each states respective transitions, in list form.
% V - Each states respective value, in list form.
% S - The current state, aka the node we are currently on/start on.
% X - the CTL-formula we are asked to check.

verify(Input) :-
  see(Input),
  read(T),
  read(V),
  read(S),
  read(X),
  seen, check(T, V, S, [], X).

% Help_function: Finds the sublist (transistions or label) to the current state S
getSublist(Head, [[Head, Tail]| _], Tail):- !.
getSublist(S, [H|T], List):-
getSublist(S, T, List).

% BASECASE: checks if the current state (S) contains the current value of X.
check(_, V, S, _, X) :-
getSublist(S, V, Vals), member(X, Vals).

% AND: true if the current state, S, has the value of F AND G.
check(T, V, S, _, and(F, G)) :-
check(T, V, S, [], F),
check(T, V, S, [], G).

% OR: true if the current state, S, has the value of F OR G.
check(T, V, S, _, or(F, G)):-
(check(T, V, S, [], F);
check(T, V, S, [], G)).

% AX: all the next nodes after S has value X.
check(T, V, S, Visited, ax(X)):-
getSublist(S, T, STransitions),
iterate(T, V, STransitions, Visited, X).

% EX: it exists a node after S that has value X.
check(T, V, S, Visited, ex(X)):-
getSublist(S, T, Neighbours),
member(TempState, Neighbours), check(T, V, TempState, Visited, X).

% AG: all the nodes in all the roads, starting with S, has value X.
check(_, _, S, Visited, ag(_)):- member(S, Visited), !.
check(T, V, S, Visited, ag(X)):-
check(T, V, S, [], X), check(T, V, S, [S|Visited], ax(ag(X))).

% EG: all the nodes in some road, starting with S, has value X.
check(_, _, S, Visited, eg(_)):- member(S, Visited), !.
check(T, V, S, Visited, eg(X)):-
check(T, V, S, [], X),
getSublist(S, T, Neighbours),
member(TempState, Neighbours),
check(T, V, TempState, [S|Visited], eg(X)).


% EF: it exsists some road, starting with S, that has some node with the value X.
check(T, V, S, Visited, ef(X)):-
not(member(S, Visited)), check(T, V, S, [], X).
check(T, V, S, Visited, ef(X)):-
not(member(S, Visited)),
getSublist(S, T, Neighbours),
member(TempState, Neighbours),
check(T, V, TempState, [S|Visited], ef(X)).


% AF: all the roads, starting with S, has some node with the value X.
check(T, V, S, Visited, af(X)):-
not(member(S, Visited)), check(T, V, S, [], X).
check(T, V, S, Visited, af(X)):-
not(member(S, Visited)), check(T, V, S, [S|Visited], ax(af(X))).


% NEGATION:
check(_, V, S, _, neg(X)) :-
getSublist(S, V, Vals), not(member(X, Vals)).


% HELP_function: Iterates through the all the next transistions to check that X holds. Used in AX.
iterate(_, _, [], _, _).
iterate(T, V, [S|STransitions], Visited, X):-
check(T, V, S, Visited, X), !, iterate(T, V, STransitions, Visited, X).
