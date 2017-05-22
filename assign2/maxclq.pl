:- lib(ic).
:- lib(branch_and_bound).

create_graph(NNodes, Density, Graph) :-
   cr_gr(1, 2, NNodes, Density, [], Graph).

cr_gr(NNodes, _, NNodes, _, Graph, Graph).
cr_gr(N1, N2, NNodes, Density, SoFarGraph, Graph) :-
   N1 < NNodes,
   N2 > NNodes,
   NN1 is N1 + 1,
   NN2 is NN1 + 1,
   cr_gr(NN1, NN2, NNodes, Density, SoFarGraph, Graph).
cr_gr(N1, N2, NNodes, Density, SoFarGraph, Graph) :-
   N1 < NNodes,
   N2 =< NNodes,
   rand(1, 100, Rand),
   (Rand =< Density ->
      append(SoFarGraph, [N1 - N2], NewSoFarGraph) ;
      NewSoFarGraph = SoFarGraph),
   NN2 is N2 + 1,
   cr_gr(N1, NN2, NNodes, Density, NewSoFarGraph, Graph).

rand(N1, N2, R) :-
   random(R1),
   R is R1 mod (N2 - N1 + 1) + N1.

elemY([H | _], 1, H).
elemY([_ | T], Y, E) :-
  Y > 1,
  Y1 is Y - 1,
  elemY(T, Y1, E).

elem([H | _], 1, Y, E) :-
  elemY(H, Y, E).
elem([_ | T], X, Y, E) :-
  X > 1,
  X1 is X - 1,
  elem(T, X1, Y, E).

setElemY([_ | T], 1, E, [E | T]).
setElemY([H | T], Y, E, [H | R]) :-
  Y > 1,
  Y1 is Y - 1,
  setElemY(T, Y1, E, R).

setElem([H | T], 1, Y, E, [H1 | T]) :-
  setElemY(H, Y, E, H1).
setElem([H | T], X, Y, E, [H | R]) :-
  X > 1,
  X1 is X - 1,
  setElem(T, X1, Y, E, R).

initMarkY([], _, 0).
initMarkY([1 | T], Y, Y) :-
  Y1 is Y - 1,
  initMarkY(T, Y, Y1).
initMarkY([0 | T], X, Y) :-
  Y >= 1,
  X =\= Y,
  Y1 is Y - 1,
  initMarkY(T, X, Y1).

initMark([], 0, _).
initMark([H | T], X, Y) :-
  X > 0,
  X1 is X - 1,
  initMarkY(H, X, Y),
  initMark(T, X1, Y).

maxclq(N, D, Clique, Size) :-
  create_graph(N, D, Graph),
  length(Clq, N),
  Clq #:: [0, 1],
  initMark(Mark1, N, N),
  getConnections(Graph, Mark1, Mark),
  constrain(Mark, Clq, N),
  Min #= N - sum(Clq),
  bb_min(search(Clq, 0, input_order, indomain_max, complete, []), Min, _),
  beautify(Clq, 1, Clique),
  length(Clique, Size).

beautify([], _, []).
beautify([0 | T], N, Clique) :-
  N1 is N + 1,
  beautify(T, N1, Clique).
beautify([1 | T], N, [N | Clique]) :-
  N1 is N + 1,
  beautify(T, N1, Clique).

constrain(_, _, 0).
constrain(Mark, Clq, I) :-
  I >= 1,
  constrainY(Mark, Clq, I, I),
  I1 is I - 1,
  constrain(Mark, Clq, I1).

constrainY(_, _, _, 0).
constrainY(Mark, Clq, I, J) :-
  J >= 1,
  elem(Mark, I, J, 1),
  J1 is J - 1,
  constrainY(Mark, Clq, I, J1).
constrainY(Mark, Clq, I, J) :-
  J >= 1,
  elem(Mark, I, J, 0),
  nthelem(Clq, I, U1),
  nthelem(Clq, J, U2),
  U1 + U2 #=< 1,
  J1 is J - 1,
  constrainY(Mark, Clq, I, J1).

nthelem([H | _], 1, H).
nthelem([_ | T], N, H) :-
  N1 is N - 1,
  nthelem(T, N1, H).

match((A, B), (A, B)).
match((A, B), (B, A)).

getConnections([], Mark, Mark).
getConnections([U - V | T], Mark, New) :-
  setElem(Mark, U, V, 1, New1),
  setElem(New1, V, U, 1, New2),
  getConnections(T, New2, New).
