:- lib(ic).
:- lib(ic_global).
:- lib(branch_and_bound).

vehicles([35, 40, 55, 15, 45, 25, 85, 55]).

clients([c(15,  77,  97), c(23, -28,  64), c(14,  77, -39),
         c(13,  32,  33), c(18,  32,   8), c(18, -42,  92),
         c(19,  -8,  -3), c(10,   7,  14), c(18,  82, -17),
         c(20, -48, -13), c(15,  53,  82), c(19,  39, -27),
         c(17, -48, -13), c(12,  53,  82), c(11,  39, -27),
         c(15, -48, -13), c(25,  53,  82), c(14, -39,   7),
         c(22,  17,   8), c(23, -38,  -7)]).

hcvrp(NCl, NVe, Timeout, Solution, Cost, Time) :-
  cputime(Start),
  clients(PartCl),
  append([c(0, 0, 0)], PartCl, Cl),
  getDist(Cl, Cl, Dist),

  length(Routes, NVe),
  vehicles(Cap),
  getQnt(Cl, ClQnt),
  initRoutes(NCl, Routes, ClQnt, Cap),

  flatten(Dist, FlattenDist),
  length(Cl, ClLen),
  getCost(Routes, ClLen, FlattenDist, Cost),

  flatten(Routes, FlatRoutes),
  setOccurrence(NCl, FlatRoutes),
  bb_min(search(FlatRoutes, 0, input_order, indomain, complete, []), Cost, bb_options{timeout: Timeout}),
  getNonZero(Routes, Solution),
  cputime(Finish),
  Time is round((Finish - Start) * 100) / 100, !.

getQnt([], []) :- !.
getQnt([c(D, _, _) | T], [D | R]) :-
  getQnt(T, R), !.

getDist(_, [], []) :- !.
getDist(Cl, [c(_, X, Y) | T], [DistY | R]) :-
  getDistY(Cl, X, Y, DistY),
  getDist(Cl, T, R), !.

getDistY([], _, _, []) :- !.
getDistY([c(_, X1, Y1) | T], X, Y, [Dist | R]) :-
  Dist2 is (X - X1) * (X - X1) + (Y - Y1) * (Y - Y1),
  sqrt(Dist2, DistPre),
  Dist is integer(round(DistPre * 1000)),
  getDistY(T, X, Y, R), !.

initRoutes(_, [], _, _) :- !.
initRoutes(NCl, [H | T], Cl, [HC | TC]) :-
  NCl > 0,
  length(H, NCl),
  H #:: [0..NCl],
  initRoutesY(H, H, Cl, CapY),
  CapY #=< HC,
  initRoutes(NCl, T, Cl, TC), !.

initRoutesY([X1 | _], [X2], Cl, Cap) :-
  (X1 #= 0) => (X2 #= 0),
  (X1 #\= 0 and X2 #\= 0) => (X1 #>= X2),
  I #= X2 + 1,
  element(I, Cl, Cap), !.
initRoutesY([X1 | T1], [X2, X3 | T], Cl, Cap) :-
  (X1 #= 0) => (X2 #= 0 and X3 #= 0),
  (X2 #= 0) => (X3 #= 0),
  (X1 #\= 0 and X2 #\= 0 and X3 #= 0) => (X1 #>= X2),
  I #= X2 + 1,
  element(I, Cl, Cap1),
  initRoutesY([X1 | T1], [X3 | T], Cl, Cap2),
  Cap #= Cap1 + Cap2, !.

getCost([], _, _, 0) :- !.
getCost([H | T], NCl, Dist, Cost) :-
  getCostY(0, H, NCl, Dist, CostY),
  getCost(T, NCl, Dist, Cost1),
  Cost #= CostY + Cost1, !.

getCostY(Pos, [], NCl, Dist, Cost) :-
  I #= NCl * Pos + 1,
  element(I, Dist, Cost), !.
getCostY(Pos, [H | T], NCl, Dist, Cost) :-
  I #= NCl * Pos + (H + 1),
  element(I, Dist, Cost1),
  getCostY(H, T, NCl, Dist, Cost2),
  Cost #= Cost1 + Cost2, !.

elemY([H | _], 0, H) :- !.
elemY([_ | T], Y, E) :-
  Y > 0,
  Y1 is Y - 1,
  elemY(T, Y1, E), !.

elem([H | _], 0, Y, E) :-
  elemY(H, Y, E), !.
elem([_ | T], X, Y, E) :-
  X > 0,
  X1 is X - 1,
  elem(T, X1, Y, E), !.

setOccurrence(0, _) :- !.
setOccurrence(NCl, R) :-
  NCl > 0,
  occurrences(NCl, R, 1),
  NCl1 is NCl - 1,
  setOccurrence(NCl1, R), !.

getNonZero([], []) :- !.
getNonZero([H | T], [S | R]) :-
  getNonZeroY(H, S),
  getNonZero(T, R), !.

getNonZeroY([], []) :- !.
getNonZeroY([0 | T], R) :-
  getNonZeroY(T, R), !.
getNonZeroY([H | T], [H | R]) :-
  H > 0,
  getNonZeroY(T, R), !.
