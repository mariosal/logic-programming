elemY([H | _], 0, H).
elemY([_ | T], Y, E) :-
  Y > 0,
  Y1 is Y - 1,
  elemY(T, Y1, E).

elem([H | _], 0, Y, E) :-
  elemY(H, Y, E).
elem([_ | T], X, Y, E) :-
  X > 0,
  X1 is X - 1,
  elem(T, X1, Y, E).

setElemY([_ | T], 0, E, [E | T]).
setElemY([H | T], Y, E, [H | R]) :-
  Y > 0,
  Y1 is Y - 1,
  setElemY(T, Y1, E, R).

setElem([H | T], 0, Y, E, [H1 | T]) :-
  setElemY(H, Y, E, H1).
setElem([H | T], X, Y, E, [H | R]) :-
  X > 0,
  X1 is X - 1,
  setElem(T, X1, Y, E, R).

delElem([], _, []).
delElem([H | T], X, R) :-
  matchDominos(H, X),
  delElem(T, H, R).
delElem([H | T], X, [H | R]) :-
  \+ matchDominos(H, X),
  delElem(T, X, R).

printH([], _).
printH([H | T], [1 | B]) :-
  write(H),
  write('-'),
  printH(T, B).
printH([H | T], [F | B]) :-
  F =\= 1,
  write(H),
  write(' '),
  printH(T, B).

printV([]).
printV([2 | T]) :-
  write('| '),
  printV(T).
printV([H | T]) :-
  H =\= 2,
  write('  '),
  printV(T).

print([], _).
print([H | T], [F | B]) :-
  printH(H, F),
  writeln(''),
  printV(F),
  writeln(''),
  print(T, B).

dominos([(0,0),(0,1),(0,2),(0,3),(0,4),(0,5),(0,6),
         (1,1),(1,2),(1,3),(1,4),(1,5),(1,6),
         (2,2),(2,3),(2,4),(2,5),(2,6),
         (3,3),(3,4),(3,5),(3,6),
         (4,4),(4,5),(4,6),
         (5,5),(5,6),
         (6,6)]).

frame([[3,1,2,6,6,1,2,2],
       [3,4,1,5,3,0,3,6],
       [5,6,6,1,2,4,5,0],
       [5,6,4,1,3,3,0,0],
       [6,1,0,6,3,2,4,0],
       [4,1,5,2,4,3,5,5],
       [4,1,0,2,4,5,2,0]]).

matchDominos((A, B), (A, B)).
matchDominos((A, B), (B, A)).

match(P, [H | _]) :-
  matchDominos(P, H).
match(P, [H | T]) :-
  \+ matchDominos(P, H),
  match(P, T).

len([], 0, 0).
len([H], 1, Y) :-
  length(H, Y).
len([_, H2 | T], X, Y) :-
  len([H2 | T], X1, Y),
  X is X1 + 1.

initMarkY([], 0).
initMarkY([-1 | T], Y) :-
  Y > 0,
  Y1 is Y - 1,
  initMarkY(T, Y1).

initMark([], 0, _).
initMark([H | T], X, Y) :-
  X > 0,
  X1 is X - 1,
  initMarkY(H, Y),
  initMark(T, X1, Y).

checkY([]).
checkY([H | T]) :-
  H =\= -1,
  checkY(T).

check([]).
check([H | T]) :-
  checkY(H),
  check(T).

solve(_, Marked, _, _, _, _, _, Marked) :-
  check(Marked).
solve(Frame, Marked, Dominos, I, J, X, Y, P) :-
  \+ length(Dominos, 0),
  \+ check(Marked),
  I < X - 1,
  J =:= Y,
  I1 is I + 1,
  solve(Frame, Marked, Dominos, I1, 0, X, Y, P).
solve(Frame, Marked, Dominos, I, J, X, Y, P) :-
  \+ length(Dominos, 0),
  \+ check(Marked),
  I < X,
  J < Y,
  J1 is J + 1,
  \+ elem(Marked, I, J, -1),
  solve(Frame, Marked, Dominos, I, J1, X, Y, P).
solve(Frame, Marked, Dominos, I, J, X, Y, P) :-
  \+ length(Dominos, 0),
  \+ check(Marked),
  I < X,
  J < Y - 1,
  J1 is J + 1,
  elem(Marked, I, J, -1),
  elem(Marked, I, J1, -1),
  elem(Frame, I, J, E1),
  elem(Frame, I, J1, E2),
  match((E1, E2), Dominos),
  setElem(Marked, I, J, 1, Marked1),
  setElem(Marked1, I, J1, 3, Marked2),
  delElem(Dominos, (E1, E2), Dominos1),
  solve(Frame, Marked2, Dominos1, I, J1, X, Y, P).
solve(Frame, Marked, Dominos, I, J, X, Y, P) :-
  \+ length(Dominos, 0),
  \+ check(Marked),
  I < X - 1,
  J < Y,
  I1 is I + 1,
  J1 is J + 1,
  elem(Marked, I, J, -1),
  elem(Marked, I1, J, -1),
  elem(Frame, I, J, E1),
  elem(Frame, I1, J, E2),
  match((E1, E2), Dominos),
  setElem(Marked, I, J, 2, Marked1),
  setElem(Marked1, I1, J, 0, Marked2),
  delElem(Dominos, (E1, E2), Dominos1),
  solve(Frame, Marked2, Dominos1, I, J1, X, Y, P).

put_dominos :-
  dominos(D),
  frame(F),
  len(F, X, Y),
  initMark(M, X, Y),
  solve(F, M, D, 0, 0, X, Y, P),
  print(F, P).
