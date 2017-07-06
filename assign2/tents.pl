:- lib(ic).
:- lib(branch_and_bound).

nthelem([H | _], 1, H).
nthelem([_ | T], N, H) :-
  N1 is N - 1,
  nthelem(T, N1, H).

transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
  lists_firsts_rests(Ms, Ts, Ms1),
  transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
  lists_firsts_rests(Rest, Fs, Oss).

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

setElemY(L, Y, _, L) :-
  Y < 1.
setElemY([], Y, _, []) :-
  Y >= 1.
setElemY([_ | T], 1, E, [H | T]) :-
  H #= E.
setElemY([H | T], Y, E, [H | R]) :-
  Y > 1,
  Y1 is Y - 1,
  setElemY(T, Y1, E, R).

setElem(L, X, _, _, L) :-
  X < 1.
setElem([], X, _, _, []) :-
  X >= 1.
setElem([H | T], 1, Y, E, [H1 | T]) :-
  setElemY(H, Y, E, H1).
setElem([H | T], X, Y, E, [H | R]) :-
  X > 1,
  X1 is X - 1,
  setElem(T, X1, Y, E, R).

initMarkY([], 0).
initMarkY([H | T], Y) :-
  Y > 0,
  H #:: [0, 1],
  Y1 is Y - 1,
  initMarkY(T, Y1).

initMark([], 0, _).
initMark([H | T], X, Y) :-
  X > 0,
  X1 is X - 1,
  initMarkY(H, Y),
  initMark(T, X1, Y).

tents(RowTents, ColTents, Trees, Tents) :-
  length(RowTents, Rows),
  length(ColTents, Cols),
  initMark(Mark, Rows, Cols),
  constrainTrees(Trees, Mark, Rows, Cols),
  constrainTents(Mark, Rows, Cols, Rows, Cols),
  constrainLimit(Mark, RowTents, Rows, 1),
  transpose(Mark, TransMark),
  constrainLimit(TransMark, ColTents, Cols, 1),
  flatten(Mark, FlatMark),
  Min #= sum(FlatMark),
  bb_min(search(FlatMark, 0, input_order, indomain_min, complete, []), Min, _),
  tentsAgain(RowTents, ColTents, Trees, Min, Tents).

tentsAgain(RowTents, ColTents, Trees, Min, Tents) :-
  length(RowTents, Rows),
  length(ColTents, Cols),
  initMark(Mark, Rows, Cols),
  constrainTrees(Trees, Mark, Rows, Cols),
  constrainTents(Mark, Rows, Cols, Rows, Cols),
  constrainLimit(Mark, RowTents, Rows, 1),
  transpose(Mark, TransMark),
  constrainLimit(TransMark, ColTents, Cols, 1),
  flatten(Mark, FlatMark),
  sum(FlatMark) #= Min,
  search(FlatMark, 0, input_order, indomain_min, complete, []),
  beautify(Mark, Rows, Cols, 1, 1, Tents).

beautify(_, Rows, _, X, _, []) :-
  X > Rows.
beautify(Mark, Rows, Cols, X, Y, T) :-
  X =< Rows,
  Y > Cols,
  X1 is X + 1,
  beautify(Mark, Rows, Cols, X1, 1, T).
beautify(Mark, Rows, Cols, X, Y, T) :-
  X =< Rows,
  Y =< Cols,
  elem(Mark, X, Y, E),
  E #= 0,
  Y1 is Y + 1,
  beautify(Mark, Rows, Cols, X, Y1, T).
beautify(Mark, Rows, Cols, X, Y, [X-Y | T]) :-
  X =< Rows,
  Y =< Cols,
  elem(Mark, X, Y, E),
  E #= 1,
  Y1 is Y + 1,
  beautify(Mark, Rows, Cols, X, Y1, T).

getCon(_, _, _, X, _, 0) :-
  X < 1.
getCon(_, Rows, _, X, _, 0) :-
  X >= 1,
  X > Rows.
getCon(_, Rows, _, X, Y, 0) :-
  X >= 1,
  X =< Rows,
  Y < 1.
getCon(_, Rows, Cols, X, Y, 0) :-
  X >= 1,
  X =< Rows,
  Y >= 1,
  Y > Cols.
getCon(Mark, Rows, Cols, X, Y, E) :-
  X >= 1,
  X =< Rows,
  Y >= 1,
  Y =< Cols,
  elem(Mark, X, Y, E).

constrainTrees([], _, _, _).
constrainTrees([X-Y | T], Mark, Rows, Cols) :-
  elem(Mark, X, Y, E),
  E #= 0,
  XPlus is X + 1,
  XMinus is X - 1,
  YPlus is Y + 1,
  YMinus is Y - 1,
  getCon(Mark, Rows, Cols, X, YPlus, E1),
  getCon(Mark, Rows, Cols, XPlus, YPlus, E2),
  getCon(Mark, Rows, Cols, XPlus, Y, E3),
  getCon(Mark, Rows, Cols, XPlus, YMinus, E4),
  getCon(Mark, Rows, Cols, X, YMinus, E5),
  getCon(Mark, Rows, Cols, XMinus, YMinus, E6),
  getCon(Mark, Rows, Cols, XMinus, Y, E7),
  getCon(Mark, Rows, Cols, XMinus, YPlus, E8),
  E1 + E2 + E3 + E4 + E5 + E6 + E7 + E8 #>= 1,
  constrainTrees(T, Mark, Rows, Cols).

constrainTents(_, _, _, X, _) :-
  X < 1.
constrainTents(Mark, Rows, Cols, X, Y) :-
  X >= 1,
  Y < 1,
  X1 is X - 1,
  constrainTents(Mark, Rows, Cols, X1, Cols).
constrainTents(Mark, Rows, Cols, X, Y) :-
  X >= 1,
  Y >= 1,
  elem(Mark, X, Y, E),
  XPlus is X + 1,
  XMinus is X - 1,
  YPlus is Y + 1,
  YMinus is Y - 1,
  getCon(Mark, Rows, Cols, X, YPlus, E1),
  getCon(Mark, Rows, Cols, XPlus, YPlus, E2),
  getCon(Mark, Rows, Cols, XPlus, Y, E3),
  getCon(Mark, Rows, Cols, XPlus, YMinus, E4),
  getCon(Mark, Rows, Cols, X, YMinus, E5),
  getCon(Mark, Rows, Cols, XMinus, YMinus, E6),
  getCon(Mark, Rows, Cols, XMinus, Y, E7),
  getCon(Mark, Rows, Cols, XMinus, YPlus, E8),
  (E #= 1) => (E1 #= 0) and (E2 #= 0) and (E3 #= 0) and (E4 #= 0) and (E5 #= 0) and (E6 #= 0) and (E7 #= 0) and (E8 #= 0),
  Y1 is Y - 1,
  constrainTents(Mark, Rows, Cols, X, Y1).

constrainLimit(_, _, Rows, N) :-
  N > Rows.
constrainLimit([_ | MT], [-1 | T], Rows, N) :-
  N =< Rows,
  N1 is N + 1,
  constrainLimit(MT, T, Rows, N1).
constrainLimit([MH | MT], [H | T], Rows, N) :-
  N =< Rows,
  H =\= -1,
  sum(MH) #=< H,
  N1 is N + 1,
  constrainLimit(MT, T, Rows, N1).

s(Tents) :-
  tents([1, -1, -1, -1, -1, -1, 3, 1, -1, 1, 2, 2, -1],
        [2, 1, -1, 1, 4, -1, 3, -1, 2, 1, 1, -1],
        [2-3, 1-5, 5-4, 4-5, 7-7, 10-6, 2-2, 4-8, 8-5, 9-9,
         1-8, 9-2, 3-3, 1-1, 9-8, 8-7, 10-10, 2-7, 8-6,
         4-4, 9-1, 11-4, 11-8, 12-5, 12-9, 12-12, 6-11,
         9-11, 6-12], Tents).
